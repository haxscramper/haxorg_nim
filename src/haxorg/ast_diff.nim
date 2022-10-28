import
  std/[
    sequtils,
    strutils,
    strformat,
    heapqueue,
    strformat,
    algorithm,
    sets,
    tables
  ]

import hmisc/core/all

import hmisc/types/colorstring
import hmisc/algo/[
  clformat,
  clformat_interpolate
]

export colorstring, clformat, clformat_interpolate



const InvalidNodeOffset = -1

type
  NodeId* = distinct int ## Within a tree, this identifies a node by its
                        ## preorder offset.

  SubNodeId* = distinct int ## \brief Identifies a node in a subtree by its
                           ## postorder offset, starting at 1.

func `$`*(id: NodeId | SubNodeId): string = $id.int

func `$$`*[T](s: seq[T]): string =
  result = "#" & $s.len & ": ["
  for idx, it in s:
    if 0 < idx: result &= ", "
    result &= $it

  result &= "]"

func initNodeId(Offset: int = InvalidNodeOffset): NodeId = NodeId(Offset)
proc initSubNodeId(Id: int): SubNodeId = SubNodeId(Id)

func inc(id: var NodeId) = inc id.int
func dec(id: var NodeId) = dec id.int
func isValid*(id: NodeId): bool = id.int != InvalidNodeOffset
func isInvalid*(id: NodeId): bool = id.int == InvalidNodeOffset
func isNil*(id: NodeId): bool = id.int == InvalidNodeOffset

type
  Mapping = object
    srcToDst: seq[NodeId]
    dstToSrc: seq[NodeId]

func initMapping(size: int): Mapping =
  Mapping(
    srcToDst: newSeqWith[NodeId](size, initNodeId()),
    dstToSrc: newSeqWith[NodeId](size, initNodeId()),
  )

func link(this: var Mapping, src, dst: NodeId) =
  this.srcToDst[src.int] = dst
  this.dstToSrc[dst.int] = src

func getDst(this: Mapping, src: NodeId): NodeId = this.srcToDst[src.int]
func getSrc(this: Mapping, dst: NodeId): NodeId = this.dstToSrc[dst.int]
func hasSrc(this: Mapping, dst: NodeId): bool = this.getSrc(dst).isValid()
func hasDst(this: Mapping, src: NodeId): bool = this.getDst(src).isValid()

type
  ChangeKind* = enum
    ChNone
    ChDelete ## (Src): delete node Src.
    ChUpdate ## (Src, Dst): update the value of node Src to match Dst.
    ChInsert ## (Src, Dst, Pos): insert Src as child of Dst at offset Pos.
    ChMove ## (Src, Dst, Pos): move Src to be a child of Dst at offset Pos.
    ChUpdateMove ## Same as Move plus Update.

type
  ASTNodeKind = distinct int

func initASTNodeKind(kind: int): ASTNodeKind =
  ASTNodeKind(kind)

func `==`*(k1, k2: ASTNodeKind): bool = int(k1) == int(k2)
func `$`*(k: ASTNodeKind): string = $int(k)

type
  CmpOpts*[IdT, ValT] = ref object
    minHeight*: int ## During top-down matching, only consider nodes of at
    ## least this height.

    minSimilarity*: float ## During bottom-up matching, match only nodes
    ## with at least this value as the ratio of their common descendants.

    maxSize*: int ## Whenever two subtrees are matched in the bottom-up
    ## phase, the optimal mapping is computed, unless the size of either
    ## subtrees exceeds this.

    stopAfterTopDown*: bool

    getValueImpl*: proc(id: IdT): ValT
    getNodeKindImpl*: proc(id: IdT): int
    areValuesEqual*: proc(v1, v2: ValT): bool
    isMatchingAllowedImpl*: proc(idSrc, idDst: IdT): bool

  TreeMirror[IdT, ValT] = object
    ## \brief Temporary container for transitioning the original AST structure
    ## to the SyntaxTree form.

    id: IdT ## Identifier value that can be used to get back the original
            ## node information

    nodes: seq[TreeMirror[IdT, ValT]] ## List of the subnodes


  Node[IdT, ValT] = ref object
    ## \brief Represents an AST node, alongside some additional
    ## information.
    ##
    ## Single node of the original AST

    parent*: NodeId
    leftMostDescendant: NodeId
    rightMostDescendant: NodeId
    indexInParent: int
    depth: int
    height: int
    shift: int
    ## Reference to the original AST node
    astNode: IdT ## Original AST node Id, used to get the kind/value
    ## information
    subnodes: seq[NodeId]  ## Explicit list of the subnode IDS
    change*: ChangeKind

func `$`*[IdT, ValT](n: Node[IdT, ValT]): string =
  "{H:$#, D:$#, S:$#, P:$#, L:$#, R:$#}" % [
    $n.height,
    $n.depth,
    $n.shift,
    $n.parent,
    $n.leftMostDescendant,
    $n.rightMostDescendant
  ]

iterator items*[IdT, ValT](
    tree: TreeMirror[IdT, ValT]): TreeMirror[IdT, ValT] =
  for it in items(tree.nodes):
    yield it

func len[IdT, ValT](tree: TreeMirror[IdT, ValT]): int =
  tree.nodes.len()

func mirror*[IdT, ValT](
    id: IdT, sub: varargs[TreeMirror[IdT, ValT]]): TreeMirror[IdT, ValT] =
  TreeMirror[IdT, ValT](id: id, nodes: @sub)

proc getValue*[IdT, ValT](
    self: CmpOpts[IdT, ValT], id: IdT): ValT =
  return self.getValueImpl(id)

proc getNodeKind[IdT, ValT](
    self: CmpOpts[IdT, ValT], id: IdT): int =
  assert(not isNil(self.getNodeKindImpl))
  return self.getNodeKindImpl(id)


proc isMatchingAllowed[IdT, ValT](
    self: CmpOpts[IdT, ValT], node1, node2: Node[IdT, ValT]): bool =
  ## Returns false if the nodes should never be matched.
  if self.isMatchingAllowedImpl.isNil():
    when compiles(isNil(node1.astNode)):
      assert(not isNil(node1.astNode))
      assert(not isNil(node2.astNode))

    let
      k1 = self.getNodeKind(node1.astNode)
      k2 = self.getNodeKind(node2.astNode)
    return k1 == k2

  else:
    return self.isMatchingAllowedImpl(node1.astNode, node2.astNode)


func initCmpOpts*[IdT, ValT](
    getValueImpl: proc(id: IdT): ValT = nil,
    getNodeKindImpl: proc(id: IdT): int = nil,
    areValuesEqual: proc(v1, v2: ValT): bool = nil,
    isMatchingAllowed: proc(idSrc, idDst: IdT): bool = nil
  ): CmpOpts[IdT, ValT] =
  CmpOpts[IdT, ValT](
    getValueImpl: getValueImpl,
    getNodeKindImpl: getNodeKindImpl,
    areValuesEqual: areValuesEqual,
    isMatchingAllowedImpl: isMatchingAllowed,
    minheight: 2,
    minSimilarity: 0.5,
    maxSize: 100,
    stopAfterTopDown: false,
  )

proc getNodeKind[IdT, ValT](
    this: Node[IdT, ValT], opts: CmpOpts[IdT, ValT]): AstNodeKind =
  return AstNodeKind(opts.getNodeKind(this.astNode))

func isLeaf[IdT, ValT](this: Node[IdT, ValT]): bool =
  this.subnodes.len() == 0

type
  SyntaxTree*[IdT, ValT] = ref object
    ## SyntaxTree objects represent subtrees of the AST.
    ##
    ## There are only two instances of the SyntaxTree class during comparison
    ## - destination and source. Structure is not recursive in tiself -
    ## subnodes are determined based on the Node::subnodes field which
    ## explicitly stores list of subnode ids.
    nodes: seq[Node[IdT, ValT]]     ## Nodes in preorder.
    leaves: seq[NodeId]
    postorderIds: seq[int]     ## Maps preorder indices to postorder ones.
    nodesBFS: seq[NodeId]
    opts: CmpOpts[IdT, ValT]




func getSize[IdT, ValT](this: SyntaxTree[IdT, ValT]): int =
  return this.nodes.len()

func getRootId[IdT, ValT](this: SyntaxTree[IdT, ValT]): NodeId =
  return initNodeId(0)

func getNode*[IdT, ValT](
    this: SyntaxTree[IdT, ValT], id: NodeId): Node[IdT, ValT] =
  return this.nodes[id]

func getId[IdT, ValT](this: SyntaxTree[IdT, ValT], id: NodeId): IdT =
  this.getNode(id).astNode

func `[]`*[T](it: seq[T], id: NodeId | SubNodeId): T =
  it[id.int]

func `[]`*[T](it: var seq[T], id: NodeId | SubNodeId): var T =
  it[id.int]

func `[]=`*[T](it: var seq[T], id: NodeId | SubNodeId, val: T) =
  it[id.int] = val

func `+`[src, dst: NodeId | int](idSrc: src, idDst: dst): NodeId =
  initNodeId(int(idSrc) + int(idDst))

func `-`[src, dst: NodeId | int](idSrc: src, idDst: dst): NodeId =
  initNodeId(int(idSrc) - int(idDst))

func `+`[src, dst: SubNodeId | int](idSrc: src, idDst: dst): SubNodeId =
  initSubNodeId(int(idSrc) + int(idDst))

func `-`[src, dst: SubNodeId | int](idSrc: src, idDst: dst): SubNodeId =
  initSubNodeId(int(idSrc) - int(idDst))

func getNode[IdT, ValT](
    this: var SyntaxTree[IdT, ValT],
    id: NodeId): var Node[IdT, ValT] =
  return this.nodes[id]
    # Node[IdT, ValT]& getNode(NodeId Id) { return Nodes[Id]; }

func isValidNodeId[IdT, ValT](this: SyntaxTree[IdT, ValT], id: NodeId): bool =
  return 0 <= id and id <= this.getSize()

func addNode[IdT, ValT](this: var SyntaxTree[IdT, ValT], N: Node[IdT, ValT]) =
  this.nodes.add(N)

func getNumberOfDescendants[IdT, ValT](
    this: SyntaxTree[IdT, ValT], id: NodeId): int =
  return int(this.getNode(id).rightMostDescendant - id + 1)


func isInSubtree[IdT, ValT](
    this: SyntaxTree[IdT, ValT], id, subtreeRoot: NodeId): bool =
  return subtreeRoot <= id and
         id <= this.getNode(subtreeRoot).rightMostDescendant

func findPositionInParent*[IdT, ValT](
    this: SyntaxTree[IdT, ValT], id: NodeId, shifted: bool = false): int =
  let parent = this.getNode(id).parent
  if (parent.isInvalid()):
    return 0

  let siblings = this.getNode(parent).subnodes
  var
    position = 0
    i = 0
    e = siblings.len()

  while i < e:
    if (shifted):
      position += this.getNode(siblings[i]).shift;

    if (siblings[i] == id):
      position += i
      return position

    inc i

  assert(false, "Node not found in parent's children.")

proc getValue*[IdT, ValT](this: SyntaxTree[IdT, ValT], id: NodeId): ValT =
  ## Serialize the node attributes to a value representation. This
  ## should uniquely distinguish nodes of the same kind. Note that this
  ## function just returns a representation of the node value, not
  ## considering descendants.
  return this.getValue(this.getNode(id))

proc getValue*[IdT, ValT](
    this: SyntaxTree[IdT, ValT], node: Node[IdT, ValT]): ValT =
  return this.opts.getValue(node.astNode)

func setleftMostDescendants[IdT, ValT](this: var SyntaxTree[IdT, ValT]) =
  for leaf in this.leaves:
    this.getNode(leaf).leftMostDescendant = leaf
    var
      parent = leaf
      cur = leaf
    while (parent = this.getNode(cur).parent ; parent.isValid()) and
          this.getNode(parent).subnodes[0] == cur:

       cur = parent
       this.getNode(cur).leftMostDescendant = leaf

func postInc[T](thing: var T): T =
  let tmp = thing
  inc thing
  return tmp

func getSubtreeBfs[IdT, ValT](
    tree: SyntaxTree[IdT, ValT], root: NodeId): seq[NodeId] =

  var ids: seq[NodeId]
  var expanded = 0
  ids.add(root)
  while expanded < ids.len():
    for subnode in tree.getNode(ids[ast_diff.postInc(expanded)]).subnodes:
      ids.add(subnode)

  return ids


proc initTree[IdT, ValT](this: var SyntaxTree[IdT, ValT]) =
  setleftMostDescendants(this)
  var postorderId = 0;
  this.postorderIds.setLen(this.getSize())
  proc traverse(this: SyntaxTree[IdT, ValT], id: NodeId) =
    for idx, subnode in this.getNode(id).subnodes:
      traverse(this, subnode)
      this.getNode(subnode).indexInParent = idx

    this.postorderIds[id] = postorderId
    inc postorderId

  traverse(this, this.getRootId())
  this.nodesBfs = getSubtreeBfs(this, this.getRootId())


type
  ASTDiff*[IdT, ValT] = ref object
    src, dst: SyntaxTree[IdT, ValT]
    map: Mapping
    opts: CmpOpts[IdT, ValT]

  HeightLess[IdT, ValT] = object
    ## Compares nodes by their depth.
    Tree: SyntaxTree[IdT, ValT]
    id: NodeId

  PriorityList[IdT, ValT] = object
    ## Priority queue for nodes, sorted descendingly by their height.
    tree: SyntaxTree[IdT, ValT]
    container: seq[NodeId]
    list: HeapQueue[HeightLess[IdT, ValT]]

func initHeightLess[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT], id: NodeId): HeightLess[IdT, ValT] =
  HeightLess[IdT, ValT](Tree: Tree, id: id)

func `<`*[IdT, ValT](h1, h2: HeightLess[IdT, ValT]): bool =
  return h1.Tree.getNode(h1.id).height < h2.Tree.getNode(h2.id).height

func initPriorityList[IdT, ValT](
    tree: SyntaxTree[IdT, ValT]): PriorityList[IdT, ValT] =
  result.tree = tree

func push[IdT, ValT](this: var PriorityList[IdT, ValT], id: NodeId) =
  this.list.push(initHeightLess(this.tree, id))

func top[T](queue: HeapQueue[T]): T = queue[queue.len() - 1]

func peekMax[IdT, ValT](this: PriorityList[IdT, ValT]): int =
  if this.list.len() == 0:
    return 0

  else:
    return this.tree.getNode(this.list.top().id).height

func pop[IdT, ValT](this: var PriorityList[IdT, ValT]): seq[NodeId] =
  let Max = peekMax(this)
  if Max == 0:
    return

  while peekMax(this) == Max:
    result.add(this.list.top().id)
    this.list.del(this.list.len() - 1)

  ## TODO this is here to get a stable output, not a good heuristic
  sort(result)


func open[IdT, ValT](this: var PriorityList[IdT, ValT], id: NodeId) =
  ## \brief add all subnodes in the input list
  for subnode in this.tree.getNode(id).subnodes:
    this.push(subnode)

proc isMatchingPossible[IdT, ValT](
    this: ASTDiff[IdT, ValT], idSrc, idDst: NodeId): bool =
  ## Returns false if the nodes must not be mached.
  bind assertRefFields
  bind assertRef
  assertRefFields(this)
  return this.opts.isMatchingAllowed(
    this.src.getNode(idSrc),
    this.dst.getNode(idDst))

proc sameValue[IdT, ValT](this: ASTDiff[IdT, ValT], v1, v2: ValT): bool =
  this.opts.areValuesEqual(v1, v2)

proc identical[IdT, ValT](this: ASTDiff[IdT, ValT], idSrc, idDst: NodeId): bool =
  ## Returns true if the two subtrees are isomorphic to each other.
  let
    nodeSrc = this.src.getNode(idSrc)
    nodeDst = this.dst.getNode(idDst)

  if nodeSrc.subnodes.len() != nodeDst.subnodes.len() or
     not isMatchingPossible(this, idSrc, idDst) or
     not this.sameValue(
       this.src.getValue(idSrc),
       this.dst.getValue(idDst)
     ):
      return false

  for id in 0 ..< nodeSrc.subnodes.len():
    if not identical(this, nodeSrc.subnodes[id], nodeDst.subnodes[id]):
      return false

  return true



proc matchTopDown[IdT, ValT](this: ASTDiff[IdT, ValT]): Mapping =
  ## Returns a mapping of identical subtrees.
  var
    srcList = initPriorityList(this.src)
    dstList = initPriorityList(this.dst)

  result = initMapping(this.src.getSize() + this.dst.getSize())
  srcList.push(this.src.getRootId())
  dstList.push(this.dst.getRootId())

  var
    maxSrc = 0
    maxDst = 0

  # until subtree of necessary height hasn't been reached
  while this.opts.minHeight <
        min(
          (maxSrc = srcList.peekMax() ; maxSrc),
          (maxDst = dstList.peekMax() ; maxDst)):

    # if two top subtrees don't have equal height
    if maxSrc > maxDst:
      # insert all nodes from tallest subforest
      for id in srcList.pop():
        srcList.open(id)

    elif maxDst > maxSrc:
      for id in dstList.pop():
        dstList.open(id)

    else:
      # otherwise get two subforest of equal height
      let topSrc = srcList.pop()
      let topDst = dstList.pop();
      # for each combination of subtrees is these forests
      for idSrc in topSrc:
        for idDst in topDst:
          # if pair of trees is isomorphic
          if identical(this, idSrc, idDst) and
             not result.hasSrc(idSrc) and
             not result.hasDst(idDst):

            for i in 0 ..< this.src.getNumberOfDescendants(idSrc):
              result.link(idSrc + i, idDst + i)

      # so we basically determine if there is any isomorphic
      # mapping between either (1) roots two highest subforests
      # or (2) root and subnodes of a root in other tree
      for idSrc in topSrc:
        # if there is unmatched forest root in first forest
        if not result.hasSrc(idSrc):
          # insert it's subnodes
          srcList.open(idSrc)

      for idDst in topDst:
        # do the same for other forest
        if not result.hasDst(idDst):
          dstList.open(idDst)

proc getSubtreePostorder[IdT, ValT](
    tree: SyntaxTree[IdT, ValT], root: NodeId): seq[NodeId] =
  var postorder: seq[NodeId]
  proc traverse(id: NodeId) =
    let n = tree.getNode(id)
    for subnode in n.subnodes:
      traverse(subnode)

    postorder.add(id)

  traverse(root)
  return postorder





type
  Subtree[IdT, ValT] = ref object
    Tree: SyntaxTree[IdT, ValT] ## The parent tree.
    rootIds: seq[NodeId] ## Maps SubNodeIds to original ids. Maps subtree
    ## nodes to their leftmost descendants wtihin the subtree.
    leftMostDescendants: seq[SubNodeId]
    KeyRoots: seq[SubNodeId]


proc getSize[IdT, ValT](this: Subtree[IdT, ValT]): int =
  return this.rootIds.len()

proc getleftMostDescendant[IdT, ValT](
    this: Subtree[IdT, ValT], Id: SubNodeId): SubNodeId =
  assert(0 < Id and Id <= this.getSize(), "Invalid subtree node index.")
  return this.leftMostDescendants[Id - 1]

proc computeKeyRoots[IdT, ValT](this: var SubTree[IdT, ValT], leaves: int) =
  this.KeyRoots.setLen(leaves)
  var Visited: HashSet[int]
  var K: int = leaves - 1
  var I = initSubNodeId(this.getSize())
  while 0 < I:
    let LeftDesc: SubNodeId = this.getleftMostDescendant(I)
    if LeftDesc.int in Visited:
      dec I
      continue

    assert(0 <= K, "K should be non-negative")
    this.KeyRoots[K] = I
    Visited.incl(LeftDesc.int)
    dec K
    dec I



proc getIdInRoot[IdT, ValT](
    this: Subtree[IdT, ValT], id: SubNodeId): NodeId =
  assert(0 < id and id <= this.getSize(), "Invalid subtree node index.")
  return this.rootIds[id - 1]

proc getNode[IdT, ValT](
      this: Subtree[IdT, ValT], id: SubNodeId): Node[IdT, ValT] =
  return this.Tree.getNode(this.getIdInRoot(id))


proc getPostorderOffset[IdT, ValT](this: Subtree[IdT, ValT]): NodeId =
  ## Returns the postorder index of the leftmost descendant in the
  ## subtree.
  return initNodeId(
    this.Tree.postorderIds[this.getIdInRoot(SubNodeId(1))])

proc setleftMostDescendants[IdT, ValT](this: var Subtree[IdT, ValT]): int =
  ## Returns the number of leafs in the subtree.
  var Numleaves = 0
  this.leftMostDescendants.setLen(this.getSize())
  for I in 0 ..< this.getSize():
    var SI = initSubNodeId(I + 1);
    let N = this.getNode(SI)
    Numleaves += (if N.isleaf(): 1 else: 0)
    assert(
      I == this.Tree.postorderIds[this.getIdInRoot(SI)] -
        this.getPostorderOffset(),
      "Postorder traversal in subtree should correspond to " &
      "traversal in the root tree by a constant offset.")

    this.leftMostDescendants[I] = SubNodeId(
        this.Tree.postorderIds[N.leftMostDescendant] -
        this.getPostorderOffset())

  return Numleaves



proc initSubtree[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT],
    subtreeRoot: NodeId
  ): Subtree[IdT, ValT] =

  result = Subtree[IdT, ValT](Tree: Tree)

  result.rootIds = getSubtreePostorder(Tree, subtreeRoot)
  let Numleaves: int = setleftMostDescendants(result)
  computeKeyRoots(result, Numleaves)

proc getValue[IdT, ValT](
    this: Subtree[IdT, ValT], Id: SubNodeId): ValT =
  return this.Tree.getValue(this.getIdInRoot(Id))



type
  ZhangShashaMatcher[IdT, ValT] = object
    ## Implementation of Zhang and Shasha's Algorithm for tree edit distance.
    ## Computes an optimal mapping between two trees using only
    ## insertion, deletion and update as edit actions (similar to the
    ## Levenshtein distance).

    diffImpl: ASTDiff[IdT, ValT]
    src: Subtree[IdT, ValT]
    dst: Subtree[IdT, ValT]
    treeDist: seq[seq[float]]
    forestDist: seq[seq[float]]

proc initZhangShashaMatcher[IdT, ValT](
    diffImpl: ASTDiff[IdT, ValT],
    src, dst: SyntaxTree[IdT, ValT],
    idDst, idSrc: NodeId
  ): ZhangShashaMatcher[IdT, ValT] =

  result.src = initSubtree(src, idSrc)
  result.dst = initSubtree(dst, idDst)
  result.diffImpl = diffImpl

  result.treeDist.setLen(result.src.getSize() + 1)
  result.forestDist.setLen(result.src.getSize() + 1)

  for it in mitems(result.treeDist):
    it.setlen(result.dst.getSize() + 1)

  for it in mitems(result.forestDist):
    it.setlen(result.dst.getSize() + 1)

func `<`*(
    idSrc: SubNodeId | NodeId | int,
    idDst: SubNodeId | NodeId | int
  ): bool =
  int(idSrc) < int(idDst)


func `==`*(
    idSrc: SubNodeId | NodeId | int,
    idDst: SubNodeId | NodeId | int
  ): bool =
  int(idSrc) == int(idDst)

func `<=`*(
    idSrc: SubNodeId | NodeId | int,
    idDst: SubNodeId | NodeId | int
  ): bool =
  int(idSrc) <= int(idDst)

## We use a simple cost model for edit actions, which seems good
## enough. Simple cost model for edit actions. This seems to make the
## matching algorithm perform reasonably well. The values range
## between 0 and 1, or infinity if this edit action should always be
## avoided.
const
  DeletionCost  = 1.0f
  InsertionCost = 1.0f
  UpdateCost    = 1.0f


proc getUpdateCost[IdT, ValT](
    this: ZhangShashaMatcher[IdT, ValT], idSrc, idDst: SubNodeId): float =

  if not this.diffImpl.isMatchingPossible(
    this.src.getIdInRoot(idSrc),
    this.dst.getIdInRoot(idDst)
  ):

    return high(float)

  else:
    if this.diffImpl.sameValue(
      this.src.getValue(idSrc),
      this.dst.getValue(idDst)
    ):
      return 0

    else:
      ## IMPLEMENT weighted node update cost that accounts for
      ## the value similarity
      return UpdateCost


proc computeForestDist[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT],
    idSrc, idDst: SubNodeId
  ) =

  assert(0 < idSrc and 0 < idDst, "Expecting offsets greater than 0.")
  var
    LMD1 = this.src.getleftMostDescendant(idSrc)
    LMD2 = this.dst.getleftMostDescendant(idDst)

  this.forestDist[LMD1][LMD2] = 0

  var D1 = LMD1 + 1
  while D1 <= idSrc:
    this.forestDist[D1][LMD2] = this.forestDist[D1 - 1][LMD2] + DeletionCost
    var D2 = LMD2 + 1

    while D2 <= idDst:
      this.forestDist[LMD1][D2] = this.forestDist[LMD1][D2 - 1] + InsertionCost
      let
        DLMD1 = this.src.getleftMostDescendant(D1)
        DLMD2 = this.dst.getleftMostDescendant(D2)

      if DLMD1 == LMD1 and DLMD2 == LMD2:
        let UpdateCost: float  = this.getUpdateCost(D1, D2)
        this.forestDist[D1][D2] = min([
          this.forestDist[D1 - 1][D2] + DeletionCost,
          this.forestDist[D1][D2 - 1] + InsertionCost,
          this.forestDist[D1 - 1][D2 - 1] + UpdateCost])

        this.treeDist[D1][D2] = this.forestDist[D1][D2];

      else:
        this.forestDist[D1][D2] = min([
          this.forestDist[D1 - 1][D2] + DeletionCost,
          this.forestDist[D1][D2 - 1] + InsertionCost,
          this.forestDist[DLMD1][DLMD2] + this.treeDist[D1][D2]])



      inc D2

    inc D1


proc computetreeDist[IdT, ValT](this: var ZhangShashaMatcher[IdT, ValT]) =
  for idSrc in this.src.KeyRoots:
    for idDst in this.dst.KeyRoots:
      computeforestDist(this, idSrc, idDst)



proc getMatchingNodes[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT]): seq[(NodeId, NodeId)] =

  var
    matches: seq[(NodeId, NodeId)]
    treePairs: seq[(SubNodeId, SubNodeId)]
    rootNodePair = true


  computetreeDist(this)
  treePairs.add((
    SubNodeId(this.src.getSize()),
    SubNodeId(this.dst.getSize())))

  # echov("SRC:", $$this.diffImpl.src.nodes)
  # echov("DST:", $$this.diffImpl.dst.nodes)

  while 0 < treePairs.len():
    var (lastRow, lastCol) = treePairs.pop()
    if not rootNodePair:
      computeforestDist(this, lastRow, lastCol)

    rootNodePair = false
    var
      firstRow     = this.src.getleftMostDescendant(lastRow)
      firstCol     = this.dst.getleftMostDescendant(lastCol)
      row          = lastRow
      col          = lastCol

    while row > firstRow or col > firstCol:
      # echov "rowcol", row, col
      # echov $$this.forestDist

      if row > firstRow and
         this.forestDist[row - 1][col] + 1 ==
         this.forestDist[row][col]:
        # echov "Dec row"
        dec row

      elif col > firstCol and
           this.forestDist[row][col - 1] + 1 == this.forestDist[row][col]:
        # echov "Dec col"
        dec col

      else:
        let
          LMD1: SubNodeId = this.src.getleftMostDescendant(row)
          LMD2: SubNodeId = this.dst.getleftMostDescendant(col)

        # echov ">", LMD1, LMD2, row, col
        if LMD1 == this.src.getleftMostDescendant(lastRow) and
           LMD2 == this.dst.getleftMostDescendant(lastCol):
          # echov(
          #   "ROOT IDS",
          #   $$this.src.rootIds,
          #   $$this.dst.rootIds,
          #   "Row", row,
          #   "Col", col
          # )
          let
            idSrc = this.src.getIdInRoot(row)
            idDst = this.dst.getIdInRoot(col)
            n1 = this.diffImpl.src.getNode(idSrc)
            n2 = this.diffImpl.dst.getNode(idDst)

          # echov(
          #   "match",
          #   this.diffImpl.isMatchingPossible(idSrc, idDst),
          #   idSrc,
          #   &"({n1})",
          #   idDst,
          #   &"({n2})",
          #   $getNodeKind(n1, this.diffImpl.opts),
          #   $getNodeKind(n2, this.diffImpl.opts),
          # )

          assert(
            this.diffImpl.isMatchingPossible(idSrc, idDst),
            "These nodes must not be matched.")

          matches.add((idSrc, idDst))
          dec row
          dec col

        else:
          treePairs.add((row, col))
          row = LMD1
          col = LMD2

  return matches





proc addOptimalMapping[IdT, ValT](
    this: var ASTDiff[IdT, ValT], map: var Mapping, idDst, idSrc: NodeId) =
  ## Uses an optimal albeit slow algorithm to compute a mapping
  ## between two subtrees, but only if both have fewer nodes than
  ## maxSize.
  if this.opts.maxSize < max(
    this.src.getNumberOfDescendants(idSrc),
    this.dst.getNumberOfDescendants(idDst)):
      return

  var matcher = initZhangShashaMatcher(this, this.src, this.dst, idSrc, idDst)
  let r = matcher.getMatchingNodes()
  for (src, dst) in r:
    if not map.hasSrc(src) and not map.hasDst(dst):
      map.link(src, dst);

proc getJaccardSimilarity[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    map: Mapping,
    idSrc, idDst: NodeId
  ): float =

  ## Computes the ratio of common descendants between the two nodes.
  ## Descendants are only considered to be equal when they are mapped in
  ## M.
  var commonDescendants = 0
  let nodeSrc = this.src.getNode(idSrc)
  # Count the common descendants, excluding the subtree root.
  var Src = idSrc + 1
  while Src <= nodeSrc.rightMostDescendant:
    let Dst = map.getDst(Src)
    commonDescendants += int(
        Dst.isValid() and this.dst.isInSubtree(Dst, idDst))

    inc Src
  # We need to subtract 1 to get the number of descendants excluding the
  # root.
  let
    denominator = this.src.getNumberOfDescendants(idSrc) - 1 +
                  this.dst.getNumberOfDescendants(idDst) - 1 -
                  commonDescendants

  # commonDescendants is less than the size of one subtree.
  assert(denominator >= 0, "Expected non-negative denominator.")
  if (denominator == 0):
    return 0

  return commonDescendants / denominator



iterator items*[IdT, ValT](tree: SyntaxTree[IdT, ValT]): NodeId =
  var start = tree.getRootId()
  let final = tree.getSize()
  while start < final:
    yield start
    inc start

iterator subnodes[IdT, ValT](
    tree: SyntaxTree[IdT, ValT], id: NodeId): NodeId =
  for sub in tree.getNode(id).subnodes:
    yield sub


proc findCandidate[IdT, ValT](
    this: var ASTDiff[IdT, ValT], map: var Mapping, idSrc: NodeId): NodeId =
  ## Returns the node that has the highest degree of similarity.

  var highestSimilarity = 0.0
  for idDst in this.dst:
    if not this.isMatchingPossible(idSrc, idDst):
      continue

    if map.hasDst(idDst):
      continue

    let similarity = this.getJaccardSimilarity(map, idSrc, idDst)
    if this.opts.minSimilarity <= similarity and
       highestSimilarity < similarity:

      highestSimilarity = similarity
      result = idDst


proc matchBottomUp[IdT, ValT](
    this: var ASTDiff[IdT, ValT], map: var Mapping) =
  let postorder = getSubtreePostorder(this.src, this.src.getRootId())
  # for all nodes in left, if node itself is not matched, but
  # has any children matched
  for src in postorder:
    # Iterating in postorder, so missing root mapping will cause more
    # expensive algorithm to kick in only if there is a top-level mismatch
    if src == this.src.getRootId() and
       not map.hasSrc(this.src.getRootId()) and
       not map.hasDst(this.dst.getRootId()):

      if isMatchingPossible(
        this, this.src.getRootId(), this.dst.getRootId()):

        map.link(this.src.getRootId(), this.dst.getRootId())
        addOptimalMapping(
          this, map, this.src.getRootId(), this.dst.getRootId())

      break

    # Check if node is properly mapped (in that case it will be skipped
    # because it is already OK) or it has no matched subnodes (in that case
    # it will be skipped because it is unlikely to have a proper candidate
    # anyway)
    let
      # WARNING original code has `hasSrc`, but I believe this to be a
      # semantic error in code as here we need to check if source node was
      # mapped to some destination target and `hasDst` checks it via
      # `srcToDst -> isValid()` sequence.
      #
      # In paper this part is described as "t1 is not mapped"
      matched = map.hasDst(src)
      nodeSrc = this.src.getNode(src)
      matchedSubnodes = anyIt(nodeSrc.subnodes, map.hasDst(it))

    # ploc()
    # if matched:
    #   echov "OK", src, map.getDst(src)

    # else:
    #   echov "NO MATCH", src
    #   if not matchedSubnodes:
    #     for sub in nodeSrc.subnodes:
    #       echov(
    #         "ERR",
    #         "src?", map.hasSrc(sub),
    #         "dst?", map.hasDst(sub),
    #         "for", sub
    #       )

    echov src, matched, matchedSubnodes
    # if it is a valid candidate and matches criteria for minimum number of
    # shares subnodes
    if (matched or not matchedSubnodes):
      continue

    let dst = this.findCandidate(map, src)
    if dst.isValid():
      # add node to mapping
      map.link(src, dst)
      # if max of number of subnodes does not exceed threshold
      addOptimalMapping(this, map, src, dst)

proc computeMapping[IdT, ValT](this: var ASTDiff[IdT, ValT]) =
  ## matches nodes one-by-one based on their similarity.
  this.map = matchTopDown(this)
  if (this.opts.stopAfterTopDown):
    return

  matchBottomUp(this, this.map)

proc computeChangeKinds[IdT, ValT](this: ASTDiff[IdT, ValT], map: var Mapping)


proc initASTDiff*[IdT, ValT](
    src, dst: SyntaxTree[IdT, ValT],
    opts: CmpOpts[IdT, ValT]
  ): ASTDiff[IdT, ValT] =
  assert(not isNil(src))
  assert(not isNil(dst))
  new(result)
  result.src = src
  result.dst = dst
  result.opts = opts
  computeMapping(result)
  computeChangeKinds(result, result.map)


proc getMapped*[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    tree: SyntaxTree[IdT, ValT], id: NodeId): NodeId =
  ## Returns the ID of the node that is mapped to the given node in
  ## SourceTree.
  result = initNodeId()
  if cast[int](tree) == cast[int](this.src):
    return this.map.getDst(id)

  assert(cast[int](tree) == cast[int](this.dst), "Invalid tree.")

  return this.map.getSrc(id)

proc getParent[IdT, ValT](
  tree: SyntaxTree[IdT, ValT], node: NodeId): NodeId =
  if node.isValid():
    result = tree.getNode(node).parent

  else:
    result = initNodeId()


proc getParentChain[IdT, ValT](
  tree: SyntaxTree[IdT, ValT], node: NodeId): seq[NodeId] =
  ## Return sequence of parent nodes for each of the `src`, `dst` argument.
  ## Resulting sequence starts from the input leaf nodes and goes upwards,
  ## so `.src[0]` is the original `src` node and so on.
  result.add node
  var parent = tree.getNode(node).parent
  while parent.isValid():
    result.add parent
    parent = tree.getNode(parent).parent

proc getParentKindPositionChain[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    map: Mapping, tree: SyntaxTree[IdT, ValT], node: NodeId
  ): seq[tuple[
    node: NodeId,
    kind: ASTNodeKind,
    position: int
  ]] =
  ## Return sequence of parent node positions and kind for each node in the
  ## parent chain for `src` and `dst`
  for node in getParentChain(tree, node):
    result.add((
      node: node,
      kind: tree.getNode(node).getNodeKind(this.opts),
      position: tree.findPositionInParent(node)
    ))

proc haveSameParents[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    map: Mapping, src, dst: NodeId
  ): bool =

  ## Returns true if the nodes' parents are matched.
  let
    srcParent = getParent(this.src, src)
    dstParent = getParent(this.dst, dst)

  return (srcParent.isInvalid() and
          dstParent.isInvalid()) or
         (srcParent.isValid() and
          dstParent.isValid() and
          map.getDst(srcParent) == dstParent)

type
  PreorderVisitor[IdT, ValT] = object
    ## Sets height, parent and subnodes for each node.
    id: int
    depth: int
    parent: NodeId
    tree: SyntaxTree[IdT, ValT]

proc initPreorderVisitor[IdT, ValT](
    tree: SyntaxTree[IdT, ValT]): PreorderVisitor[IdT, ValT] =
  result.tree = tree
  result.parent = initNodeId()

proc preTraverse[Tree, IdT, ValT](
    this: var PreorderVisitor[IdT, ValT],
    node: Tree,
    getId: proc(tree: Tree): IdT
  ): (NodeId, NodeId) =

  let myId = initNodeId(this.id)
  this.tree.nodes.add(Node[IdT, ValT]())
  var n = this.tree.getNode(myId)
  n.parent = this.parent
  n.depth = this.depth
  n.astNode = getId(node)

  if this.parent.isValid():
    this.tree.getNode(this.parent).subnodes.add(myId)

  this.parent = myId

  inc this.id
  inc this.depth
  return (myId, this.tree.getNode(myId).parent)

proc postTraverse[IdT, ValT](
    this: var PreorderVisitor[IdT, ValT],
    state: (NodeId, NodeId)
  ) =

  let (myId, prevParent) = state
  assert(myId.isValid(), "Expecting to only traverse valid nodes.")
  this.parent = prevParent
  dec this.depth
  var N = this.tree.getNode(myId)
  N.rightMostDescendant = initNodeId(this.id - 1)
  assert(
    0 <= N.rightMostDescendant and
    N.rightMostDescendant < this.tree.getSize(),
    "Rightmost descendant must be a valid tree node.")

  if N.isleaf():
    this.tree.leaves.add(myId)

  N.height = 1
  for subnode in N.subnodes:
    N.height = max(
      N.height,
      1 + this.tree.getNode(subnode).height)

proc traverse[Tree, IdT, ValT](
    this: var PreorderVisitor[IdT, ValT],
    node: Tree,
    getId: proc(tree: Tree): IdT
  ) =

  let savedState = preTraverse(this, node, getId)
  if 0 < len(node):
    for sub in items(node):
      traverse(this, sub, getId)

  postTraverse(this, savedState)

proc initSyntaxTree[IdT, ValT](
    opts: CmpOpts[IdT, ValT]): SyntaxTree[IdT, ValT] =
  result = SyntaxTree[IdT, ValT](opts: opts)


proc getMirrorId*[IdT, ValT](tree: TreeMirror[IdT, ValT]): IdT =
  tree.id

proc initSyntaxTree*[Tree, IdT, ValT](
    opts: CmpOpts[IdT, ValT],
    n: Tree,
    getId: proc(tree: Tree): IdT
  ): SyntaxTree[IdT, ValT] =
  ## Constructs a tree from an AST node.
  result = initSyntaxTree(opts)
  var walker = initPreorderVisitor[IdT, ValT](result)
  traverse(walker, n, getId)
  initTree(result)

proc computeChangeKinds[IdT, ValT](
    this: ASTDiff[IdT, ValT], map: var Mapping) =
  ## Compute Change for each node based on similarity.
  for src in this.src:
    if not map.hasSrc(src):
      this.src.getNode(src).change = ChDelete
      this.src.getNode(src).shift -= 1;

  for dst in this.dst:
    if not map.hasDst(dst):
      this.dst.getNode(dst).change = ChInsert
      this.dst.getNode(dst).shift -= 1

  const absoluteMove = false
  for src in this.src.nodesBfs:
    let dst = map.getDst(src)
    if dst.isInvalid():
      continue

    if not this.haveSameParents(map, src, dst) or
       this.src.findPositionInParent(src, absoluteMove) !=
       this.dst.findPositionInParent(dst, absoluteMove):

      this.src.getNode(src).shift -= 1
      this.dst.getNode(dst).shift -= 1

  for dst in this.dst.nodesBfs:
    let src = map.getSrc(dst)
    if src.isInvalid():
      continue

    var
      nodeSrc = this.src.getNode(src)
      nodeDst = this.dst.getNode(dst)

    if (src.isInvalid()):
      continue

    # Node either moved between completely different parents or it was
    # moved inside of the same node. In both cases it is a 'move'
    # operation. IDEA Maybe it should be split into "internal move" and
    # "external move".
    if not haveSameParents(this, map, src, dst) or
       this.src.findPositionInParent(src, absoluteMove) !=
       this.dst.findPositionInParent(dst, absoluteMove):

      # let
      #   srcParent = this.src.getParent(src)
      #   dstParent = this.dst.getParent(dst)

      # echov "map", src, dst, "as move"
      # echov(
      #   "pos",
      #   "in src", this.src.findPositionInParent(src, absoluteMove),
      #   "in dst", this.dst.findPositionInParent(dst, absoluteMove),
      #   "same", haveSameParents(this, map, src, dst),
      #   "p src", srcParent,
      #   "p dst", dstParent,
      #   "p src -> p dst", map.getDst(dstParent)
      # )


      nodeSrc.change = ChMove
      nodeDst.change = ChMove

    if not this.sameValue(
      this.src.getValue(src),
      this.dst.getValue(dst)
    ):
      nodeDst.change = if nodeSrc.change == ChMove:
                       ChUpdateMove
                     else:
                       ChUpdate

      nodeSrc.change = nodeDst.change


proc `$`*[IdT, ValT](tree: SyntaxTree[IdT, ValT], id: NodeId): string =
  if (id.isInvalid()):
    result = "None"
    return

  result = $tree.getNode(id).getNodeKind(tree.opts).int
  result &= ": " & $tree.getValue(id)
  result &= "(" & $id & ")"


type
  DiffResult*[IdT, ValT] = object
    src*: SyntaxTree[IdT, ValT]
    dst*: SyntaxTree[IdT, ValT]
    changes*: ASTDiff[IdT, ValT]

type
  NodeChange* = object
    src*: NodeId
    dst*: NodeId
    case kind: ChangeKind
      of ChInsert:
        parent*: NodeId
        position*: int

      of ChMove, ChUpdateMove:
        moveFrom*, moveTo*: tuple[parent: NodeId, position: int]

      else:
        discard

proc getNodeChange[IdT, ValT](
    diff: DiffResult[IdT, ValT],
    src, dst: NodeId,
    fromDst: bool = false
  ): NodeChange =

  let node = if fromDst:
               diff.dst.getNode(dst)

             else:
               diff.src.getNode(src)

  result = NodeChange(kind: node.change, src: src, dst: dst)
  case node.change:
    of ChNone:
      # Direct mapping, no change in value or position
      discard

    of ChDelete:
      discard

    of ChUpdate:
      discard

    of ChUpdateMove, ChMove:
      let
        srcNode = diff.src.getNode(src)
        dstNode = diff.dst.getNode(dst)

      result.moveFrom = (
        srcNode.parent,
        diff.src.findPositionInParent(src, true)
      )

      result.moveTo = (
        dstNode.parent,
        diff.dst.findPositionInParent(dst, true)
      )

    of ChInsert:
      result.parent = node.parent
      if fromDst:
        result.position = diff.dst.findPositionInParent(dst)

      else:
        result.position = diff.src.findPositionInParent(src)

proc changeFromDst[IdT, ValT](
    diff: DiffResult[IdT, ValT], dst: NodeId): NodeChange =
  let src = diff.changes.getMapped(diff.dst, dst)
  getNodeChange(diff, src, dst, fromDst = true)

proc changeFromSrc[IdT, ValT](
    diff: DiffResult[IdT, ValT], src: NodeId): NodeChange =
  let dst = diff.changes.getMapped(diff.src, src)
  getNodeChange(diff, src, dst, fromDst = false)

iterator items*[IdT, ValT](diff: DiffResult[IdT, ValT]): NodeChange =
  for dst in diff.dst:
    yield changeFromDst(diff, dst)


proc srcValue[IdT, ValT](diff: DiffResult[IdT, ValT], node: NodeId): ValT =
  diff.src.getValue(node)

proc dstValue[IdT, ValT](diff: DiffResult[IdT, ValT], node: NodeId): ValT =
  diff.dst.getValue(node)

proc srcValue[IdT, ValT](
    diff: DiffResult[IdT, ValT], change: NodeChange): ValT =
  diff.src.getValue(change.src)

proc dstValue[IdT, ValT](
    diff: DiffResult[IdT, ValT], change: NodeChange): ValT =
  diff.dst.getValue(change.dst)


proc `$`*[IdT, ValT](
    diff: DiffResult[IdT, ValT], change: NodeChange): string =
  if (change.src.isValid()):
    result &= "Match $# to $# -- " % [ $change.src, $change.dst ]

  case change.kind:
    of ChNone:
      result &= "None '$#' to '$#'" % [
        $diff.srcValue(change),
        $diff.dstValue(change)
      ]

    of ChDelete:
      result &= "Delete"

    of ChUpdate:
      result &= &"Update '$#' to '$#'" % [
        $diff.srcValue(change),
        $diff.dstValue(change),
      ]

    of ChMove, ChUpdateMove, ChInsert:
      case change.kind:
        of ChInsert: result &= "Insert"
        of ChMove: result &= "Move"
        of ChUpdateMove:
          result &= "Update '$#' to '$#' and Move" % [
            $diff.srcValue(change),
            $diff.dstValue(change)
          ]
        else: discard

      result &= &" $# into $# at $#" % [
        $diff.dstValue(change.parent),
        $diff.dstValue(change),
        $change.position
      ]



proc diffRefKind*[T](
    t1, t2: T,
    eqImpl: proc(v1, v2: T): bool,
    opts: CmpOpts[T, T] = nil
  ): DiffResult[T, T] =

  type
    IdT = T
    Valt = T

  let
    getValueImpl = proc(id: IdT): ValT = id
    getNodeKindImpl = proc(id: IdT): int = int(id.kind)

  var opts =
    if isNil(opts):
      initCmpOpts[IdT, ValT]()

    else:
      opts


  opts.getValueImpl = getValueImpl
  opts.getNodeKindImpl = getNodeKindImpl
  opts.areValuesEqual = eqImpl

  proc getId(id: T): T = id

  result.src = initSyntaxTree(opts, t1, getId)
  result.dst = initSyntaxTree(opts, t2, getId)
  result.changes = initASTDiff(result.src, result.dst, opts)


type
  GraphvizExplainNode* = object
    ## Single node in source or target tree
    selfId*: NodeId
    targetId*: Option[NodeId]
    change*: NodeChange
    next*: Option[NodeId]
    indexInParent*: int

  GraphvizExplain* = object
    ## Required information for the tree diff graph generation
    src*: seq[GraphvizExplainNode] ## Full list of nodes in the source
    ## syntax tree.
    dst*: seq[GraphvizExplainNode] ## Full list of nodes in the destination
    ## syntax tree.
    linked*: HashSet[tuple[src, dst: NodeId]] ## Source and destination ID
    ## pairings between two trees.

  GraphvizFormatConf*[ValT] = object
    horizontalDir*: bool ## Put both graphs in top-down manner or arrange
    ## both tries in a left-to-right fashion.
    maxMappingHeight*: int ## Only show directly matched links between
    ## nodes that are no higher than this. Depth is taken as a minimum of
    ## source and destination nodes.
    formatKind*: proc(kind: int): string ## Format node kind into a simple
    ## text string that will be used in a tree representation.
    formatValue*: proc(value: ValT): string ## Format node value into a simple
    ## text string that will be used in a tree representation. If resulting
    ## string is non-empty it should start with the `\l` newline if node
    ## formatting is multiline (or leading space for better one-line
    ## formatting).

func kind*(node: GraphvizExplainNode): ChangeKind =
  node.change.kind

proc initGraphvizFormat*[ValT](): GraphvizFormatConf[ValT] =
  return GraphvizFormatConf[ValT](
    horizontalDir: true,
    maxMappingHeight: 2
  )

proc formatGraphvizDiff*[IdT, ValT](
    diff: DiffResult[IdT, ValT],
    dot: GraphvizExplain,
    conf: GraphvizFormatConf[ValT],
  ): string =

  var
    subSrc: string
    subDst: string

  proc explainChange(entry: GraphvizExplainNode, src: bool):
    tuple[style, text: string] =

    case entry.kind:
      of ChNone: ("color=black; style=dotted", "")
      of ChDelete: ("color=red; style=filled", "")
      of ChInsert: ("color=green; style=filled", "")
      of ChMove:
        let ch = entry.change
        let dchange = diff.changes
        if ch.moveFrom.position == ch.moveTo.position:
          var relevantMove = false
          let srcChain = getParentKindPositionChain(
            dchange, dchange.map, dchange.src, ch.src)

          let dstChain = getParentKindPositionChain(
            dchange, dchange.map, dchange.dst, ch.dst)

          # echov "----"
          # echov "src", srcChain.mapIt(
          #   "sup:$#/kind:$#/idx:$#" % [$it[0], $it[1], $it[2]])
          # echov "dst", dstChain.mapIt(
          #   "sup:$#/kind:$#/idx:$#" % [$it[0], $it[1], $it[2]])

          relevantMove = dstChain != srcChain

          if relevantMove:
            ("color=yellow; style=bold;", "")

          else:
            ("color=black; style=dashed;",
             if true:
               ""

             else:
               if src:
                 "$# -> $# -> $#" % [
                   srcChain.mapIt(
                     "$#/$#/$#" % [$it[0], $it[1], $it[2]]).
                     join("-"),
                   $dchange.src.getParent(ch.src),
                   $dchange.map.getDst(dchange.src.getParent(ch.src))
                 ]
               else:
                 "$# -> $#" % [
                   dstChain.mapIt(
                     "$#/$#/$#" % [$it[0], $it[1], $it[2]]).
                     join("-"),
                   $dchange.dst.getParent(ch.dst)
                 ]
            )


        else:
          ("color=yellow; style=bold",
           "@$#" % [
             if src:
               $entry.change.moveFrom.parent
             else:
               $entry.change.moveTo.parent
           ]
          )

      of ChUpdateMove: ("color=orange; style=filled", "")
      of ChUpdate: ("color=pink; style=filled", "")


  var srcLayerLink = ""
  var srcLeafNodes: seq[string]

  for entry in dot.src:
    let n = diff.src.getNode(entry.selfId)
    let (format, text) = explainChange(entry, true)
    subSrc.add "    s$#[label=\"$1[$#] $# $#$#\", $#];\n" % [
      $entry.selfId,
      $entry.indexInParent,
      conf.formatKind(getNodeKind(n, diff.changes.opts).int),
      text,
      conf.formatValue(diff.src.getValue(entry.selfId)),
      format
    ]

    if n.height == 1:
      srcLeafNodes.add("s$#" % [$entry.selfId])

    # if entry.next.isSome():
    #   srcLayerLink.add "    {rankdir=TB; rank=same; s$# -> s$#[style=invis];}\n" % [
    #     $entry.next.get(),
    #     $entry.selfId,
    #   ]

  for entry in dot.src:
    for subnode in subnodes(diff.changes.src, entry.selfId):
      subsrc.add "    s$# -> s$#;\n" % [
        $entry.selfId,
        $subnode
      ]

  var
    dstLayerLink = ""
    dstLeafNodes: seq[string]

  for entry in dot.dst:
    let n = diff.dst.getNode(entry.selfId)
    let (format, text) = explainChange(entry, false)
    subDst.add "    t$#[label=\"$1[$#] $# $#$#\", $#];\n" % [
      $entry.selfId,
      $entry.indexInParent,
      conf.formatKind(getNodeKind(n, diff.changes.opts).int),
      text,
      conf.formatValue(diff.dst.getValue(entry.selfId)),
      format
    ]

    if n.height == 1:
      dstLeafNodes.add("t$#" % [$entry.selfId])

    # if entry.next.isSome():
    #   dstLayerLink.add "    {rank=same; d$# -> d$#[style=invis];}\n" % [
    #     $entry.next.get(),
    #     $entry.selfId,
    #   ]

  for entry in dot.dst:
    for subnode in subnodes(diff.changes.dst, entry.selfId):
      if conf.horizontalDir:
        subDst.add "    t$# -> t$#[dir=back];\n" % [
          $subnode,
          $entry.selfId
        ]

      else:
        subDst.add "    t$# -> t$#;\n" % [
          $entry.selfId,
          $subnode
        ]

  var mapping: string
  for (key, val) in items(dot.linked):
    let
      hKey = diff.changes.src.getNode(key).height
      hVal = diff.changes.src.getNode(val).height

    if min(hKey, hVal) < conf.maxMappingHeight:
      mapping &= "  s$# -> t$#[style=dashed, dir=none];\n" % [ $key, $val ]


  result = """
digraph G {
  node[shape=rect, fontname=consolas];
  splines=polyline;
  rankdir=$#;
  spline=polyline;
  subgraph cluster_0 {
label = "Source tree";
$#
$#
{rank=same;$#[style=invis];}  }

  subgraph cluster_1 {
label = "Destination tree";
$#
$#
{rank=same;$#[style=invis];}  }
$#}
""" % [
    if conf.horizontalDir: "LR" else: "TB",
    subSrc, srcLayerLink, srcLeafNodes.join(" -> "),
    subDst, dstLayerLink, dstLeafNodes.join(" -> "),
    mapping
  ]

proc explainGraphvizDiff*[IdT, ValT](
    diff: DiffResult[IdT, ValT]
  ): GraphvizExplain =
  ## Generate visualization data for differences between two trees.

  let d = diff.changes
  var
    lastTop = initNodeId()
    lastNode = initNodeId()
    explain: Table[NodeId, GraphvizExplainNode]

  for node in d.src.nodesBFS:
    var src = GraphvizExplainNode(selfid: node)
    let dst = d.map.getDst(node)
    src.indexInParent = d.src.getNode(node).indexInParent

    if dst.isValid():
      src.targetId = some dst
      src.change = getNodeChange(
        diff, node, dst, fromDst = false)

      result.linked.incl((node, dst))

    else:
      src.change.kind = ChDelete

    let newTop = d.src.getNode(node).parent
    if lastTop.isValid() and lastTop == newTop:
      explain[lastNode].next = some node

    else:
      lastTop = newTop
      lastNode = initNodeId()

    lastNode = node
    explain[node] = src

  for _, src in explain:
    result.src.add(src)

  explain.clear()
  lastNode = initNodeId()
  lastTop = initNodeId()

  for node in d.dst.nodesBFS:
    var dst = GraphvizExplainNode(selfId: node)
    let src = d.map.getSrc(node)
    dst.indexInParent = d.dst.getNode(node).indexInParent

    if src.isValid():
      dst.targetId = some src
      dst.change = getNodeChange(
        diff, src, node, fromDst = true)

      result.linked.incl((src, node))

    else:
      dst.change = NodeChange(kind: ChInsert)

    let newTop = d.dst.getNode(node).parent
    if lastTop.isValid() and lastTop == newTop:
      explain[lastNode].next = some node

    else:
      lastTop = newTop
      lastNode = initNodeId()

    lastNode = node
    explain[node] = dst

  for _, dst in explain:
    result.dst.add(dst)

proc hasChanges*[IdT, ValT](diff: DiffResult[IdT, ValT]): bool =
  ## Check if two trees had any mismatching elements
  for node in diff.changes.src:
    let dst = diff.changes.map.getDst(node)
    if dst.isInvalid() or
       getNodeChange(diff, node, dst).kind != ChNone:
      return true

  for node in diff.changes.dst:
    let src = diff.changes.map.getSrc(node)
    if src.isInvalid() or
       getNodeChange(diff, src, node).kind != ChNone:
      return true

  return false

  
proc explainDiff*[IdT, ValT](
    diff: DiffResult[IdT, ValT],
    valueChange: proc(v1, v2: ValT): ColoredText,
    value: proc(v: ValT): ColoredText,
    fromDst: bool = false
  ): ColoredText =
  coloredResult()

  proc aux(id: NodeId, level: int) =
    let change = if fromDst:
                   diff.changeFromDst(id)

                 else:
                   diff.changeFromSrc(id)

    let srcNode = if change.src.isNil():
                    default(IdT)
                  else:
                    diff.src.getId(change.src)

    let dstNode = if change.dst.isNil():
                    default(IdT)
                  else:
                    diff.dst.getId(change.dst)

    if fromDst:
      addi level, $dstNode.kind + fgCyan

    else:
      addi level, $srcNode.kind + fgCyan

    add "($#)" % [
      if fromDst: $change.dst else: $change.src
    ]

    case change.kind:
      of ChNone:
        if diff.dst.getNode(change.dst).isLeaf():
          add " "
          add value(diff.dstValue(change))

      of ChUpdate:
        add " "
        add valueChange(
          diff.srcValue(change),
          diff.dstValue(change)
        )

      of ChInsert:
        add " insert " + fgGreen
        add value(diff.dstValue(change))

      of ChDelete:
        add " delete " + fgRed

      of ChMove:
        add " move"

      of ChUpdateMove:
        add " update "
        add valueChange(
          diff.srcValue(change),
          diff.dstValue(change)
        )

        add " move from $#[$#] to $#[$#]" % [
          $change.moveFrom.parent,
          $change.moveFrom.position,
          $change.moveTo.parent,
          $change.moveTo.position
        ]

    for node in subnodes(if fromDst: diff.dst else: diff.src, id):
      add "\n"
      aux(node, level + 1)


  if fromDst:
    aux(diff.dst.getRootId(), 0)

  else:
    aux(diff.src.getRootId(), 0)
