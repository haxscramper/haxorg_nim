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

func link(this: var Mapping, Src, Dst: NodeId) =
   this.srcToDst[Src.int] = Dst
   this.dstToSrc[Dst.int] = Src

func getDst(this: Mapping, Src: NodeId): NodeId = this.srcToDst[Src.int]
func getSrc(this: Mapping, Dst: NodeId): NodeId = this.dstToSrc[Dst.int]
func hasSrc(this: Mapping, Dst: NodeId): bool = this.getSrc(Dst).isValid()
func hasDst(this: Mapping, Src: NodeId): bool = this.getDst(Src).isValid()

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
    minheight*: int ## During top-down matching, only consider nodes of at
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
    isMatchingAllowedImpl*: proc(id1, id2: IdT): bool

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
    getValueImpl: proc(id: IdT): ValT,
    getNodeKindImpl: proc(id: IdT): int,
    areValuesEqual: proc(v1, v2: ValT): bool,
    isMatchingAllowed: proc(id1, id2: IdT): bool = nil
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

func `+`[src, dst: NodeId | int](id1: src, id2: dst): NodeId =
  initNodeId(int(id1) + int(id2))

func `-`[src, dst: NodeId | int](id1: src, id2: dst): NodeId =
  initNodeId(int(id1) - int(id2))

func `+`[src, dst: SubNodeId | int](id1: src, id2: dst): SubNodeId =
  initSubNodeId(int(id1) + int(id2))

func `-`[src, dst: SubNodeId | int](id1: src, id2: dst): SubNodeId =
  initSubNodeId(int(id1) - int(id2))

func getMutableNode[IdT, ValT](
    this: var SyntaxTree[IdT, ValT],
    id: NodeId): var Node[IdT, ValT] =
  return this.nodes[id]
    # Node[IdT, ValT]& getMutableNode(NodeId Id) { return Nodes[Id]; }

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
    this.getMutableNode(leaf).leftMostDescendant = leaf
    var
      parent = leaf
      cur = leaf
    while (parent = this.getNode(cur).parent ; parent.isValid()) and
          this.getNode(parent).subnodes[0] == cur:

       cur = parent
       this.getMutableNode(cur).leftMostDescendant = leaf

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
    this: ASTDiff[IdT, ValT], id1, id2: NodeId): bool =
  ## Returns false if the nodes must not be mached.
  bind assertRefFields
  bind assertRef
  assertRefFields(this)
  return this.opts.isMatchingAllowed(
    this.src.getNode(id1),
    this.dst.getNode(id2))

proc sameValue[IdT, ValT](this: ASTDiff[IdT, ValT], v1, v2: ValT): bool =
  this.opts.areValuesEqual(v1, v2)

proc identical[IdT, ValT](this: ASTDiff[IdT, ValT], id1, id2: NodeId): bool =
  ## Returns true if the two subtrees are isomorphic to each other.
  let
    node1 = this.src.getNode(id1)
    node2 = this.dst.getNode(id2)

  if node1.subnodes.len() != node2.subnodes.len() or
     not isMatchingPossible(this, id1, id2) or
     not this.sameValue(
       this.src.getValue(id1),
       this.dst.getValue(id2)
     ):
      return false

  for Id in 0 ..< node1.subnodes.len():
    if not identical(this, node1.subnodes[Id], node2.subnodes[Id]):
      return false

  return true



proc matchTopDown[IdT, ValT](this: ASTDiff[IdT, ValT]): Mapping =
  ## Returns a mapping of identical subtrees.
  var
    L1 = initPriorityList(this.src)
    L2 = initPriorityList(this.dst)
    M = initMapping(this.src.getSize() + this.dst.getSize())

  L1.push(this.src.getRootId())
  L2.push(this.dst.getRootId())

  var
    Max1: int = 0
    Max2: int = 0

  # until subtree of necessary height hasn't been reached
  while this.opts.minheight <
        min((Max1 = L1.peekMax() ; Max1), (Max2 = L2.peekMax() ; Max2)):

    # if two top subtrees don't have equal height
    if Max1 > Max2:
      # insert all nodes from tallest subforest
      for Id in L1.pop():
        L1.open(Id)

    elif Max2 > Max1:
      for Id in L2.pop():
        L2.open(Id)

    else:
      # otherwise get two subforest of equal height
      let H1 = L1.pop()
      let H2 = L2.pop();
      # for each combination of Therese is these forests
      for id1 in H1:
        for id2 in H2:
          # if pair of trees is isomorphic
          if identical(this, id1, id2) and
             not M.hasSrc(id1) and
             not M.hasDst(id2):

            for I in 0 ..< this.src.getNumberOfDescendants(id1):
              M.link(id1 + I, id2 + I)

      # so we basically determine if there is any isomorphic
      # mapping between either (1) roots two highest subforests
      # or (2) root and subnodes of a root in other tree
      for id1 in H1:
        # if there is unmatched forest root in first forest
        if not M.hasSrc(id1):
          # insert it's subnodes
          L1.open(id1)

      for id2 in H2:
        # do the same for other forest
        if not M.hasDst(id2):
          L2.open(id2)

  return M

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
    id2, id1: NodeId
  ): ZhangShashaMatcher[IdT, ValT] =

  result.src = initSubtree(src, id1)
  result.dst = initSubtree(dst, id2)
  result.diffImpl = diffImpl

  result.treeDist.setLen(result.src.getSize() + 1)
  result.forestDist.setLen(result.src.getSize() + 1)

  for it in mitems(result.treeDist):
    it.setlen(result.dst.getSize() + 1)

  for it in mitems(result.forestDist):
    it.setlen(result.dst.getSize() + 1)

func `<`*(
    id1: SubNodeId | NodeId | int,
    id2: SubNodeId | NodeId | int
  ): bool =
  int(id1) < int(id2)


func `==`*(
    id1: SubNodeId | NodeId | int,
    id2: SubNodeId | NodeId | int
  ): bool =
  int(id1) == int(id2)

func `<=`*(
    id1: SubNodeId | NodeId | int,
    id2: SubNodeId | NodeId | int
  ): bool =
  int(id1) <= int(id2)

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
    this: ZhangShashaMatcher[IdT, ValT], id1, id2: SubNodeId): float =

  if not this.diffImpl.isMatchingPossible(
    this.src.getIdInRoot(id1),
    this.dst.getIdInRoot(id2)
  ):

    return high(float)

  else:
    if this.diffImpl.sameValue(
      this.src.getValue(id1),
      this.dst.getValue(id2)
    ):
      return 0

    else:
      ## IMPLEMENT weighted node update cost that accounts for
      ## the value similarity
      return UpdateCost


proc computeForestDist[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT],
    id1, id2: SubNodeId
  ) =

  assert(0 < id1 and 0 < id2, "Expecting offsets greater than 0.")
  var
    LMD1 = this.src.getleftMostDescendant(id1)
    LMD2 = this.dst.getleftMostDescendant(id2)

  this.forestDist[LMD1][LMD2] = 0

  var D1 = LMD1 + 1
  while D1 <= id1:
    this.forestDist[D1][LMD2] = this.forestDist[D1 - 1][LMD2] + DeletionCost
    var D2 = LMD2 + 1

    while D2 <= id2:
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
  for id1 in this.src.KeyRoots:
    for id2 in this.dst.KeyRoots:
      computeforestDist(this, id1, id2)



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
            id1 = this.src.getIdInRoot(row)
            id2 = this.dst.getIdInRoot(col)
            n1 = this.diffImpl.src.getNode(id1)
            n2 = this.diffImpl.dst.getNode(id2)

          # echov(
          #   "match",
          #   this.diffImpl.isMatchingPossible(id1, id2),
          #   id1,
          #   &"({n1})",
          #   id2,
          #   &"({n2})",
          #   $getNodeKind(n1, this.diffImpl.opts),
          #   $getNodeKind(n2, this.diffImpl.opts),
          # )

          assert(
            this.diffImpl.isMatchingPossible(id1, id2),
            "These nodes must not be matched.")

          matches.add((id1, id2))
          dec row
          dec col

        else:
          treePairs.add((row, col))
          row = LMD1
          col = LMD2

  return matches





proc addOptimalMapping[IdT, ValT](
    this: var ASTDiff[IdT, ValT], map: var Mapping, id2, id1: NodeId) =
  ## Uses an optimal albeit slow algorithm to compute a mapping
  ## between two subtrees, but only if both have fewer nodes than
  ## maxSize.
  if this.opts.maxSize < max(
    this.src.getNumberOfDescendants(id1),
    this.dst.getNumberOfDescendants(id2)):
      return

  var matcher = initZhangShashaMatcher(this, this.src, this.dst, id1, id2)
  let r = matcher.getMatchingNodes()
  for (src, dst) in r:
    if not map.hasSrc(src) and not map.hasDst(dst):
      map.link(src, dst);

proc getJaccardSimilarity[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    map: Mapping,
    id1, id2: NodeId
  ): float =

  ## Computes the ratio of common descendants between the two nodes.
  ## Descendants are only considered to be equal when they are mapped in
  ## M.
  var CommonDescendants = 0
  let node1 = this.src.getNode(id1)
  # Count the common descendants, excluding the subtree root.
  var Src = id1 + 1
  while Src <= node1.rightMostDescendant:
    let Dst = map.getDst(Src)
    CommonDescendants += int(
        Dst.isValid() and this.dst.isInSubtree(Dst, id2))

    inc Src
  # We need to subtract 1 to get the number of descendants excluding the
  # root.
  let
    Denominator = this.src.getNumberOfDescendants(id1) - 1 +
                  this.dst.getNumberOfDescendants(id2) - 1 -
                  CommonDescendants

  # CommonDescendants is less than the size of one subtree.
  assert(Denominator >= 0, "Expected non-negative denominator.")
  if (Denominator == 0):
    return 0

  return CommonDescendants / Denominator



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
    this: var ASTDiff[IdT, ValT], map: var Mapping, id1: NodeId): NodeId =
  ## Returns the node that has the highest degree of similarity.

  var HighestSimilarity = 0.0
  for id2 in this.dst:
    if not this.isMatchingPossible(id1, id2):
      continue

    if map.hasDst(id2):
      continue

    let Similarity = this.getJaccardSimilarity(map, id1, id2)
    if this.opts.minSimilarity <= Similarity and
       HighestSimilarity < Similarity:

      HighestSimilarity = Similarity
      result = id2


proc matchBottomUp[IdT, ValT](
    this: var ASTDiff[IdT, ValT], map: var Mapping) =

  let Postorder = getSubtreePostorder(this.src, this.src.getRootId())
  # for all nodes in left, if node itself is not matched, but
  # has any children matched
  for id1 in Postorder:
    if id1 == this.src.getRootId() and
       not map.hasSrc(this.src.getRootId()) and
       not map.hasDst(this.dst.getRootId()):

      if isMatchingPossible(
        this, this.src.getRootId(), this.dst.getRootId()):

        map.link(this.src.getRootId(), this.dst.getRootId())
        addOptimalMapping(
          this, map, this.src.getRootId(), this.dst.getRootId())

      break

    let matched = map.hasSrc(id1)
    let node1      = this.src.getNode(id1)

    let matchedsubnodes = anyIt(node1.subnodes, map.hasSrc(it))

    #  if it is a valid candidate and matches criteria for
    # minimum number of shares subnodes
    if (matched or not matchedsubnodes):
      continue

    let id2 = this.findCandidate(map, id1)
    if id2.isValid():
      # add node to mapping
      map.link(id1, id2)
      # if max of number of subnodes does not exceed threshold
      addOptimalMapping(this, map, id1, id2)


proc computeMapping[IdT, ValT](this: var ASTDiff[IdT, ValT]) =
  ## matches nodes one-by-one based on their similarity.
  this.map = matchTopDown(this)
  if (this.opts.stopAfterTopDown):
    return

  matchBottomUp(this, this.map)

proc computeChangeKinds[IdT, ValT](this: ASTDiff[IdT, ValT], map: var Mapping)
  ## Compute Change for each node based on similarity.


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
    map: Mapping, id1, id2: NodeId
  ): bool =

  ## Returns true if the nodes' parents are matched.
  let
    srcParent = getParent(this.src, id1)
    dstParent = getParent(this.dst, id2)

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
  var n = this.tree.getMutableNode(myId)
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
  var N = this.tree.getMutableNode(myId)
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

  for id1 in this.src:
    if not map.hasSrc(id1):
      this.src.getMutableNode(id1).change = ChDelete
      this.src.getMutableNode(id1).shift -= 1;

  for id2 in this.dst:
    if not map.hasDst(id2):
      this.dst.getMutableNode(id2).change = ChInsert
      this.dst.getMutableNode(id2).shift -= 1

  for id1 in this.src.nodesBfs:
    let id2 = map.getDst(id1)
    if id2.isInvalid():
      continue

    if not this.haveSameParents(map, id1, id2) or
       this.src.findpositionInParent(id1, true) !=
       this.dst.findpositionInParent(id2, true):

      this.src.getMutableNode(id1).shift -= 1
      this.dst.getMutableNode(id2).shift -= 1

  for id2 in this.dst.nodesBfs:
    let id1 = map.getSrc(id2)
    if id1.isInvalid():
      continue

    var
      node1 = this.src.getMutableNode(id1)
      node2 = this.dst.getMutableNode(id2)

    if (id1.isInvalid()):
      continue

    if not haveSameParents(this, map, id1, id2) or
       this.src.findPositionInParent(id1, true) !=
       this.dst.findPositionInParent(id2, true):

      node1.change = ChMove
      node2.change = ChMove

    if not this.sameValue(
      this.src.getValue(id1),
      this.dst.getValue(id2)
    ):
      node2.change = if node1.change == ChMove:
                       ChUpdateMove
                     else:
                       ChUpdate

      node1.change = node2.change


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
    t1, t2: T, eqImpl: proc(v1, v2: T): bool): DiffResult[T, T] =

  type
    IdT = T
    Valt = T

  let opts = initCmpOpts[IdT, ValT](
    proc(id: IdT): ValT = id,
    proc(id: IdT): int = int(id.kind),
    eqImpl
  )

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
          block check_mapping:
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
            ("color=black; style=dashed;", "")


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
    subSrc.add "    s$#[label=\"[$#] $#$#$#\", $#];\n" % [
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
    subDst.add "    t$#[label=\"[$#] $# $#$#\", $#];\n" % [
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
      mapping &= "  s$# -> t$#[style=dashed];\n" % [ $key, $val ]


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
