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

import
  hmisc/core/all

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

func `$`*[T](s: seq[T]): string =
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
    depth: int
    height: int
    shift: int
    ## Reference to the original AST node
    astNode: IdT ## Original AST node Id, used to get the kind/value
    ## information
    subnodes: seq[NodeId]  ## Explicit list of the subnode IDS
    change*: ChangeKind

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

    return self.getNodeKind(node1.astNode) == self.getNodeKind(node2.astNode)

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

func `$`*[IdT, ValT](node: Node[IdT, ValT]): string = $(node[])

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

func `+`[tree1, tree2: NodeId | int](id1: tree1, id2: tree2): NodeId =
  initNodeId(int(id1) + int(id2))

func `-`[tree1, tree2: NodeId | int](id1: tree1, id2: tree2): NodeId =
  initNodeId(int(id1) - int(id2))

func `+`[tree1, tree2: SubNodeId | int](id1: tree1, id2: tree2): SubNodeId =
  initSubNodeId(int(id1) + int(id2))

func `-`[tree1, tree2: SubNodeId | int](id1: tree1, id2: tree2): SubNodeId =
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
    Tree: SyntaxTree[IdT, ValT], Root: NodeId): seq[NodeId] =

  var ids: seq[NodeId]
  var Expanded = 0
  ids.add(Root)
  while Expanded < ids.len():
    for Subnode in Tree.getNode(ids[ast_diff.postInc(Expanded)]).subnodes:
      ids.add(Subnode)

  return ids


proc initTree[IdT, ValT](this: var SyntaxTree[IdT, ValT]) =
  setleftMostDescendants(this)
  var postorderId = 0;
  this.postorderIds.setLen(this.getSize())
  proc traverse(this: SyntaxTree[IdT, ValT], id: NodeId) =
    for subnode in this.getNode(id).subnodes:
      traverse(this, subnode)

    this.postorderIds[id] = postorderId
    inc postorderId

  traverse(this, this.getRootId())
  this.nodesBfs = getSubtreeBfs(this, this.getRootId())


type
  ASTDiff*[IdT, ValT] = ref object
    tree1, tree2: SyntaxTree[IdT, ValT]
    map: Mapping
    opts: CmpOpts[IdT, ValT]

  heightLess[IdT, ValT] = object
    ## Compares nodes by their depth.
    Tree: SyntaxTree[IdT, ValT]
    id: NodeId

  PriorityList[IdT, ValT] = object
    ## Priority queue for nodes, sorted descendingly by their height.
    tree: SyntaxTree[IdT, ValT]
    container: seq[NodeId]
    list: HeapQueue[heightLess[IdT, ValT]]

func initheightLess[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT], id: NodeId): heightLess[IdT, ValT] =
  heightLess[IdT, ValT](Tree: Tree, id: id)

func `<`*[IdT, ValT](h1, h2: heightLess[IdT, ValT]): bool =
  return h1.Tree.getNode(h1.id).height < h2.Tree.getNode(h2.id).height

func initPriorityList[IdT, ValT](
    tree: SyntaxTree[IdT, ValT]): PriorityList[IdT, ValT] =
  result.tree = tree

func push[IdT, ValT](this: var PriorityList[IdT, ValT], id: NodeId) =
  this.list.push(initheightLess(this.tree, id))

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
  assertRefFields(this)
  return this.opts.isMatchingAllowed(
    this.tree1.getNode(id1),
    this.tree2.getNode(id2))

proc sameValue[IdT, ValT](this: ASTDiff[IdT, ValT], v1, v2: ValT): bool =
  this.opts.areValuesEqual(v1, v2)

proc identical[IdT, ValT](this: ASTDiff[IdT, ValT], id1, id2: NodeId): bool =
  ## Returns true if the two subtrees are isomorphic to each other.
  let
    node1 = this.tree1.getNode(id1)
    node2 = this.tree2.getNode(id2)

  if node1.subnodes.len() != node2.subnodes.len() or
     not isMatchingPossible(this, id1, id2) or
     not this.sameValue(
       this.tree1.getValue(id1),
       this.tree2.getValue(id2)
     ):
      return false

  for Id in 0 ..< node1.subnodes.len():
    if not identical(this, node1.subnodes[Id], node2.subnodes[Id]):
      return false

  return true



proc matchTopDown[IdT, ValT](this: ASTDiff[IdT, ValT]): Mapping =
  ## Returns a mapping of identical subtrees.
  var
    L1 = initPriorityList(this.tree1)
    L2 = initPriorityList(this.tree2)
    M = initMapping(this.tree1.getSize() + this.tree2.getSize())

  L1.push(this.tree1.getRootId())
  L2.push(this.tree2.getRootId())

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

            for I in 0 ..< this.tree1.getNumberOfDescendants(id1):
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
    Tree: SyntaxTree[IdT, ValT], Root: NodeId): seq[NodeId] =
  var Postorder: seq[NodeId]
  proc traverse(Id: NodeId) =
    let N = Tree.getNode(Id)
    for Subnode in N.subnodes:
      traverse(Subnode)

    Postorder.add(Id)

  traverse(Root)
  return Postorder





type
  Subtree[IdT, ValT] = ref object
    Tree: SyntaxTree[IdT, ValT] ## The parent tree.
    RootIds: seq[NodeId] ## Maps SubNodeIds to original ids. Maps subtree
    ## nodes to their leftmost descendants wtihin the subtree.
    leftMostDescendants: seq[SubNodeId]
    KeyRoots: seq[SubNodeId]


proc getSize[IdT, ValT](this: Subtree[IdT, ValT]): int =
  return this.RootIds.len()

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
    this: Subtree[IdT, ValT], Id: SubNodeId): NodeId =
  assert(0 < Id and Id <= this.getSize(), "Invalid subtree node index.")
  return this.RootIds[Id - 1]

proc getNode[IdT, ValT](
      this: Subtree[IdT, ValT], Id: SubNodeId): Node[IdT, ValT] =
  return this.Tree.getNode(this.getIdInRoot(Id))


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

  result.RootIds = getSubtreePostorder(Tree, subtreeRoot)
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
    S1: Subtree[IdT, ValT]
    S2: Subtree[IdT, ValT]
    treeDist: seq[seq[float]]
    forestDist: seq[seq[float]]

proc initZhangShashaMatcher[IdT, ValT](
    diffImpl: ASTDiff[IdT, ValT],
    tree1, tree2: SyntaxTree[IdT, ValT],
    id2, id1: NodeId
  ): ZhangShashaMatcher[IdT, ValT] =

  result.S1 = initSubtree(tree1, id1)
  result.S2 = initSubtree(tree2, id2)
  result.diffImpl = diffImpl

  result.treeDist.setLen(result.S1.getSize() + 1)
  result.forestDist.setLen(result.S1.getSize() + 1)

  for it in mitems(result.treeDist):
    it.setlen(result.S2.getSize() + 1)

  for it in mitems(result.forestDist):
    it.setlen(result.S2.getSize() + 1)

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
    this.S1.getIdInRoot(id1),
    this.S2.getIdInRoot(id2)
  ):

    return high(float)

  else:
    if this.diffImpl.sameValue(
      this.S1.getValue(id1),
      this.S2.getValue(id2)
    ):
      return 0

    else:
      ## IMPLEMENT weighted node update cost that accounts for
      ## the value similarity
      return UpdateCost


proc computeforestDist[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT],
    id1, id2: SubNodeId
  ) =

  assert(id1 > 0 and id2 > 0, "Expecting offsets greater than 0.")
  var
    LMD1 = this.S1.getleftMostDescendant(id1)
    LMD2 = this.S2.getleftMostDescendant(id2)

  this.forestDist[LMD1][LMD2] = 0

  var D1 = LMD1 + 1
  while D1 < id1:
    this.forestDist[D1][LMD2] = this.forestDist[D1 - 1][LMD2] + DeletionCost
    var D2 = LMD2 + 1

    while D2 <= id2:
      this.forestDist[LMD1][D2] = this.forestDist[LMD1][D2 - 1] + InsertionCost
      let
        DLMD1 = this.S1.getleftMostDescendant(D1)
        DLMD2 = this.S2.getleftMostDescendant(D2)

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
  for id1 in this.S1.KeyRoots:
    for id2 in this.S2.KeyRoots:
      computeforestDist(this, id1, id2)



proc getMatchingNodes[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT]): seq[(NodeId, NodeId)] =

  var Matches: seq[(NodeId, NodeId)]
  var TreePairs: seq[(SubNodeId, SubNodeId)]
  computetreeDist(this)
  var RootNodePair = true
  TreePairs.add((
    SubNodeId(this.S1.getSize()),
    SubNodeId(this.S2.getSize())))

  while 0 < TreePairs.len():
    var LastRow, LastCol, FirstRow, FirstCol, Row, Col: SubNodeId
    (LastRow, LastCol) = TreePairs.pop()
    if not RootNodePair:
      computeforestDist(this, LastRow, LastCol)

    RootNodePair = false
    FirstRow     = this.S1.getleftMostDescendant(LastRow)
    FirstCol     = this.S2.getleftMostDescendant(LastCol)
    Row          = LastRow
    Col          = LastCol
    while Row > FirstRow or Col > FirstCol:
      if Row > FirstRow and
         this.forestDist[Row - 1][Col] + 1 == this.forestDist[Row][Col]:
        dec Row;

      elif Col > FirstCol and
           this.forestDist[Row][Col - 1] + 1 == this.forestDist[Row][Col]:
        dec Col

      else:
        var
          LMD1: SubNodeId = this.S1.getleftMostDescendant(Row)
          LMD2: SubNodeId = this.S2.getleftMostDescendant(Col)

        if LMD1 == this.S1.getleftMostDescendant(LastRow) and
           LMD2 == this.S2.getleftMostDescendant(LastCol):

          let
            id1 = this.S1.getIdInRoot(Row)
            id2 = this.S2.getIdInRoot(Col)

          assert(
            this.diffImpl.isMatchingPossible(id1, id2),
            "These nodes must not be matched.")

          Matches.add((id1, id2))
          dec Row
          dec Col

        else:
          TreePairs.add((Row, Col))
          Row = LMD1
          Col = LMD2

  return Matches





proc addOptimalMapping[IdT, ValT](
    this: var ASTDiff[IdT, ValT], M: var Mapping, id2, id1: NodeId) =
  ## Uses an optimal albeit slow algorithm to compute a mapping
  ## between two subtrees, but only if both have fewer nodes than
  ## maxSize.
  if this.opts.maxSize < max(
    this.tree1.getNumberOfDescendants(id1),
    this.tree2.getNumberOfDescendants(id2)):
      return

  var Matcher = initZhangShashaMatcher(this, this.tree1, this.tree2, id1, id2)
  let R = Matcher.getMatchingNodes()
  for Tuple in R:
    let Src = Tuple[0]
    let Dst = Tuple[0]
    if not M.hasSrc(Src) and not M.hasDst(Dst):
      M.link(Src, Dst);

proc getJaccardSimilarity[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    M: Mapping,
    id1, id2: NodeId
  ): float =

  ## Computes the ratio of common descendants between the two nodes.
  ## Descendants are only considered to be equal when they are mapped in
  ## M.
  var CommonDescendants = 0
  let node1 = this.tree1.getNode(id1)
  # Count the common descendants, excluding the subtree root.
  var Src = id1 + 1
  while Src <= node1.rightMostDescendant:
    let Dst = M.getDst(Src)
    CommonDescendants += int(
        Dst.isValid() and this.tree2.isInSubtree(Dst, id2))

    inc Src
  # We need to subtract 1 to get the number of descendants excluding the
  # root.
  let
    Denominator = this.tree1.getNumberOfDescendants(id1) - 1 +
                  this.tree2.getNumberOfDescendants(id2) - 1 -
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
    this: var ASTDiff[IdT, ValT], M: var Mapping, id1: NodeId): NodeId =
  ## Returns the node that has the highest degree of similarity.

  var HighestSimilarity = 0.0
  for id2 in this.tree2:
    if not this.isMatchingPossible(id1, id2):
      continue

    if M.hasDst(id2):
      continue

    let Similarity = this.getJaccardSimilarity(M, id1, id2)
    if this.opts.minSimilarity <= Similarity and
       HighestSimilarity < Similarity:

      HighestSimilarity = Similarity
      result = id2


proc matchBottomUp[IdT, ValT](this: var ASTDiff[IdT, ValT], M: var Mapping) =
    let Postorder = getSubtreePostorder(this.tree1, this.tree1.getRootId())
    # for all nodes in left, if node itself is not matched, but
    # has any children matched
    for id1 in Postorder:
      if id1 == this.tree1.getRootId() and
         not M.hasSrc(this.tree1.getRootId()) and
         not M.hasDst(this.tree2.getRootId()):

        if isMatchingPossible(this, this.tree1.getRootId(), this.tree2.getRootId()):
          M.link(this.tree1.getRootId(), this.tree2.getRootId())
          addOptimalMapping(this, M, this.tree1.getRootId(), this.tree2.getRootId())

        break

      let Matched = M.hasSrc(id1)
      let node1      = this.tree1.getNode(id1)

      let Matchedsubnodes = anyIt(node1.subnodes, M.hasSrc(it))

      #  if it is a valid candidate and matches criteria for
      # minimum number of shares subnodes
      if (Matched or not Matchedsubnodes):
        continue

      let id2 = this.findCandidate(M, id1)
      if id2.isValid():
        # add node to mapping
        M.link(id1, id2)
        # if max of number of subnodes does not exceed threshold
        addOptimalMapping(this, M, id1, id2)


proc computeMapping[IdT, ValT](this: var ASTDiff[IdT, ValT]) =
  ## Matches nodes one-by-one based on their similarity.
  this.map = matchTopDown(this)
  if (this.opts.stopAfterTopDown):
    return

  matchBottomUp(this, this.map)

proc computeChangeKinds[IdT, ValT](this: ASTDiff[IdT, ValT], M: var Mapping)
  ## Compute Change for each node based on similarity.


proc initASTDiff*[IdT, ValT](
    tree1, tree2: SyntaxTree[IdT, ValT],
    opts: CmpOpts[IdT, ValT]
  ): ASTDiff[IdT, ValT] =
  assert(not isNil(tree1))
  assert(not isNil(tree2))
  new(result)
  result.tree1 = tree1
  result.tree2 = tree2
  result.opts = opts
  computeMapping(result)
  computeChangeKinds(result, result.map)


proc getMapped*[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    tree: SyntaxTree[IdT, ValT], id: NodeId): NodeId =
  ## Returns the ID of the node that is mapped to the given node in
  ## SourceTree.
  result = initNodeId()
  if cast[int](tree) == cast[int](this.tree1):
    return this.map.getDst(id)

  assert(cast[int](tree) == cast[int](this.tree2), "Invalid tree.")

  return this.map.getSrc(id)

proc haveSameparents[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    M: Mapping, id1, id2: NodeId
  ): bool =

  ## Returns true if the nodes' parents are matched.
  let
    P1 = this.tree1.getNode(id1).parent
    P2 = this.tree2.getNode(id2).parent

  return (P1.isInvalid() and P2.isInvalid()) or
         (P1.isValid() and P2.isValid() and M.getDst(P1) == P2);


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
    N.height = min(
      N.height,
      1 + this.tree.getNode(subnode).height)

proc traverse[Tree, IdT, ValT](
    this: var PreorderVisitor[IdT, ValT],
    node: Tree,
    getId: proc(tree: Tree): IdT
  ) =

  let SavedState = preTraverse(this, node, getId)
  if 0 < len(node):
    for sub in items(node):
      traverse(this, sub, getId)

  postTraverse(this, SavedState)

proc initSyntaxTree[IdT, ValT](
    opts: CmpOpts[IdT, ValT]): SyntaxTree[IdT, ValT] =
  result = SyntaxTree[IdT, ValT](opts: opts)


proc getMirrorId*[IdT, ValT](tree: TreeMirror[IdT, ValT]): IdT =
  tree.id

proc initSyntaxTree*[Tree, IdT, ValT](
    opts: CmpOpts[IdT, ValT],
    N: Tree,
    getId: proc(tree: Tree): IdT
  ): SyntaxTree[IdT, ValT] =
  ## Constructs a tree from an AST node.
  result = initSyntaxTree(opts)
  var walker = initPreorderVisitor[IdT, ValT](result)
  traverse(walker, N, getId)
  initTree(result)

proc computeChangeKinds[IdT, ValT](
    this: ASTDiff[IdT, ValT], M: var Mapping) =
  for id1 in this.tree1:
    if not M.hasSrc(id1):
      this.tree1.getMutableNode(id1).change = ChDelete
      this.tree1.getMutableNode(id1).shift -= 1;

  for id2 in this.tree2:
    if not M.hasDst(id2):
      this.tree2.getMutableNode(id2).change = ChInsert
      this.tree2.getMutableNode(id2).shift -= 1

  for id1 in this.tree1.nodesBfs:
    let id2 = M.getDst(id1)
    if id2.isInvalid():
      continue

    if not this.haveSameparents(M, id1, id2) or
       this.tree1.findpositionInParent(id1, true) !=
       this.tree2.findpositionInParent(id2, true):

      this.tree1.getMutableNode(id1).shift -= 1
      this.tree2.getMutableNode(id2).shift -= 1

  for id2 in this.tree2.nodesBfs:
    let id1 = M.getSrc(id2)
    if id1.isInvalid():
      continue

    var
      node1 = this.tree1.getMutableNode(id1)
      node2 = this.tree2.getMutableNode(id2)

    if (id1.isInvalid()):
      continue

    if not haveSameParents(this, M, id1, id2) or
       this.tree1.findPositionInParent(id1, true) !=
       this.tree2.findPositionInParent(id2, true):

      node1.change = ChMove
      node2.change = ChMove

    if not this.sameValue(
      this.tree1.getValue(id1),
      this.tree2.getValue(id2)
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
  DiffResult[IdT, ValT] = object
    src: SyntaxTree[IdT, ValT]
    dst: SyntaxTree[IdT, ValT]
    diff: ASTDiff[IdT, ValT]

type
  NodeChange = object
    src: NodeId
    dst: NodeId
    case kind: ChangeKind
      of ChInsert:
        parent: NodeId
        position: int

      of ChMove, ChUpdateMove:
        moveFrom, moveTo: tuple[parent: NodeId, position: int]

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
        diff.src.findPositionInParent(src)
      )

      result.moveTo = (
        dstNode.parent,
        diff.dst.findPositionInParent(dst)
      )

    of ChInsert:
      result.parent = node.parent
      if fromDst:
        result.position = diff.dst.findPositionInParent(dst)

      else:
        result.position = diff.src.findPositionInParent(src)

proc changeFromDst[IdT, ValT](
    diff: DiffResult[IdT, ValT], dst: NodeId): NodeChange =
  let src = diff.diff.getMapped(diff.dst, dst)
  getNodeChange(diff, src, dst, fromDst = true)

proc changeFromSrc[IdT, ValT](
    diff: DiffResult[IdT, ValT], src: NodeId): NodeChange =
  let dst = diff.diff.getMapped(diff.src, src)
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
  result.diff = initASTDiff(result.src, result.dst, opts)

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
