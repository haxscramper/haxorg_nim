import
  std/[
    sequtils,
    heapqueue,
    algorithm,
    sets,
    tables
  ]

import
  hmisc/core/all
startHax()

const InvalidNodeOffset = -1

type
  NodeId = distinct int ## Within a tree, this identifies a node by its
                        ## preorder offset.

  SubNodeId = distinct int ## \brief Identifies a node in a subtree by its
                           ## postorder offset, starting at 1.

func `$`(id: NodeId | SubNodeId): string = $id.int

func `$`[T](s: seq[T]): string =
  result = "#" & $s.len & ": ["
  for idx, it in s:
    if 0 < idx: result &= ", "
    result &= $it

  result &= "]"

func initNodeId(Offset: int = InvalidNodeOffset): NodeId = NodeId(Offset)
proc initSubNodeId(Id: int): SubNodeId = SubNodeId(Id)

func inc(id: var NodeId) = inc id.int
func dec(id: var NodeId) = dec id.int
func isValid(id: NodeId): bool = id.int != InvalidNodeOffset
func isInvalid(id: NodeId): bool = id.int == InvalidNodeOffset

type
  Mapping = object
    SrcToDst: seq[NodeId]
    DstToSrc: seq[NodeId]

func initMapping(size: int): Mapping =
  Mapping(
    SrcToDst: newSeqWith[NodeId](size, initNodeId()),
    DstToSrc: newSeqWith[NodeId](size, initNodeId()),
  )

func link(this: var Mapping, Src, Dst: NodeId) =
   this.SrcToDst[Src.int] = Dst
   this.DstToSrc[Dst.int] = Src

func getDst(this: Mapping, Src: NodeId): NodeId = this.SrcToDst[Src.int]
func getSrc(this: Mapping, Dst: NodeId): NodeId = this.DstToSrc[Dst.int]
func hasSrc(this: Mapping, Dst: NodeId): bool = this.getSrc(Dst).isValid()
func hasDst(this: Mapping, Src: NodeId): bool = this.getDst(Src).isValid()

type
  ChangeKind = enum
    None
    Delete ## (Src): delete node Src.
    Update ## (Src, Dst): update the value of node Src to match Dst.
    Insert ## (Src, Dst, Pos): insert Src as child of Dst at offset Pos.
    Move ## (Src, Dst, Pos): move Src to be a child of Dst at offset Pos.
    UpdateMove ## Same as Move plus Update.

type
  ASTNodeKind = distinct int

func initASTNodeKind(kind: int): ASTNodeKind =
  ASTNodeKind(kind)

func `==`(k1, k2: ASTNodeKind): bool = int(k1) == int(k2)

type
  CmpOpts[IdT, ValT] = ref object
    MinHeight: int ## During top-down matching, only consider nodes of at
    ## least this height.

    MinSimilarity: float ## During bottom-up matching, match only nodes
    ## with at least this value as the ratio of their common descendants.

    MaxSize: int ## Whenever two subtrees are matched in the bottom-up
    ## phase, the optimal mapping is computed, unless the size of either
    ## subtrees exceeds this.

    StopAfterTopDown: bool

    getNodeValueImpl: proc(id: IdT): ValT
    getNodeKindImpl: proc(id: IdT): int
    isMatchingAllowedImpl: proc(id1, id2: IdT): bool


  TreeMirror[IdT, ValT] = object
    ## \brief Temporary container for transitioning the original AST structure
    ## to the SyntaxTree form.

    id: IdT  ## Identifier value that can be used to get back the original
            ## node information

    subnodes: seq[TreeMirror[IdT, ValT]] ## List of the subnodes


  Node[IdT, ValT] = ref object
    ## \brief Represents an AST node, alongside some additional
    ## information.
    ##
    ## Single node of the original AST

    Parent: NodeId
    LeftMostDescendant: NodeId
    RightMostDescendant: NodeId
    Depth: int
    Height: int
    Shift: int
    ## Reference to the original AST node
    ASTNode: IdT ## Original AST node Id, used to get the kind/value
    ## information
    subnodes: seq[NodeId]  ## Explicit list of the subnode IDS
    Change: ChangeKind

func mirror[IdT, ValT](
    id: IdT, sub: varargs[TreeMirror[IdT, ValT]]): TreeMirror[IdT, ValT] =
  TreeMirror[IdT, ValT](id: id, subnodes: @sub)

proc getNodeValue[IdT, ValT](
    self: CmpOpts[IdT, ValT], id: IdT): ValT =
  return self.getNodeValueImpl(id)

proc getNodeKind[IdT, ValT](
    self: CmpOpts[IdT, ValT], id: IdT): int =
  assert(not isNil(self.getNodeKindImpl))
  return self.getNodeKindImpl(id)


proc isMatchingAllowed[IdT, ValT](
    self: CmpOpts[IdT, ValT], N1, N2: Node[IdT, ValT]): bool =
  ## Returns false if the nodes should never be matched.
  if self.isMatchingAllowedImpl.isNil():
    when compiles(isNil(N1.ASTNode)):
      assert(not isNil(N1.ASTNode))
      assert(not isNil(N2.ASTNode))

    return self.getNodeKind(N1.ASTNode) == self.getNodeKind(N2.ASTNode)

  else:
    return self.isMatchingAllowedImpl(N1.ASTNode, N2.ASTNode)


func initCmpOpts[IdT, ValT](
    getNodeValueImpl: proc(id: IdT): ValT,
    getNodeKindImpl: proc(id: IdT): int,
    isMatchingAllowed: proc(id1, id2: IdT): bool = nil
  ): CmpOpts[IdT, ValT] =
  CmpOpts[IdT, ValT](
    getNodeValueImpl: getNodeValueImpl,
    getNodeKindImpl: getNodeKindImpl,
    isMatchingAllowedImpl: isMatchingAllowed,
    MinHeight: 2,
    MinSimilarity: 0.5,
    MaxSize: 100,
    StopAfterTopDown: false,
  )


proc getNodeKind[IdT, ValT](
    this: Node[IdT, ValT], opts: CmpOpts[IdT, ValT]): ASTNodeKind =
  return ASTNodeKind(opts.getNodeKind(this.ASTNode))

func isLeaf[IdT, ValT](this: Node[IdT, ValT]): bool =
  this.subnodes.len() == 0

type
  SyntaxTree[IdT, ValT] = ref object
    ## SyntaxTree objects represent subtrees of the AST.
    ##
    ## There are only two instances of the SyntaxTree class during comparison
    ## - destination and source. Structure is not recursive in tiself -
    ## subnodes are determined based on the Node::subnodes field which
    ## explicitly stores list of subnode ids.


    Nodes: seq[Node[IdT, ValT]]     ## Nodes in preorder.
    Leaves: seq[NodeId]
    PostorderIds: seq[int]     ## Maps preorder indices to postorder ones.
    NodesBFS: seq[NodeId]
    opts: CmpOpts[IdT, ValT]



proc initSyntaxTree[IdT, ValT](
    opts: CmpOpts[IdT, ValT],
    N: TreeMirror[IdT, ValT]): SyntaxTree[IdT, ValT]
  ## Constructs a tree from an AST node.


func getSize[IdT, ValT](this: SyntaxTree[IdT, ValT]): int =
  return this.Nodes.len()

func getRootId[IdT, ValT](this: SyntaxTree[IdT, ValT]): NodeId =
  return initNodeId(0)

func `$`[IdT, ValT](node: Node[IdT, ValT]): string = $(node[])

    # PreorderIterator             begin() const { return getRootId(); }
    # PreorderIterator             end() const { return getSize(); }
func getNode[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Id: NodeId): Node[IdT, ValT] =
  return this.Nodes[Id]

func `[]`[T](it: seq[T], id: NodeId | SubNodeId): T =
  it[id.int]

func `[]`[T](it: var seq[T], id: NodeId | SubNodeId): var T =
  it[id.int]

func `[]=`[T](it: var seq[T], id: NodeId | SubNodeId, val: T) =
  it[id.int] = val

func `+`[T1, T2: NodeId | int](id1: T1, id2: T2): NodeId =
  initNodeId(int(id1) + int(id2))

func `-`[T1, T2: NodeId | int](id1: T1, id2: T2): NodeId =
  initNodeId(int(id1) - int(id2))

func `+`[T1, T2: SubNodeId | int](id1: T1, id2: T2): SubNodeId =
  initSubNodeId(int(id1) + int(id2))

func `-`[T1, T2: SubNodeId | int](id1: T1, id2: T2): SubNodeId =
  initSubNodeId(int(id1) - int(id2))

func getMutableNode[IdT, ValT](
    this: var SyntaxTree[IdT, ValT],
    Id: NodeId): var Node[IdT, ValT] =
  return this.Nodes[Id]
    # Node[IdT, ValT]& getMutableNode(NodeId Id) { return Nodes[Id]; }

func isValidNodeId[IdT, ValT](this: SyntaxTree[IdT, ValT], Id: NodeId): bool =
  return 0 <= Id and Id <= this.getSize()

func addNode[IdT, ValT](this: var SyntaxTree[IdT, ValT], N: Node[IdT, ValT]) =
  this.Nodes.add(N)

func getNumberOfDescendants[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Id: NodeId): int =
  return int(this.getNode(Id).RightMostDescendant - Id + 1)


func isInSubtree[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Id, SubtreeRoot: NodeId): bool =
  return Id >= SubtreeRoot and
         Id <= this.getNode(SubtreeRoot).RightMostDescendant

func findPositionInParent[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Id: NodeId, Shifted: bool = false): int =
    let Parent = this.getNode(Id).Parent
    if (Parent.isInvalid()):
      return 0

    let Siblings = this.getNode(Parent).subnodes
    var
      Position = 0
      I = 0
      E = Siblings.len()

    while I < E:
        if (Shifted):
          Position += this.getNode(Siblings[I]).Shift;

        if (Siblings[I] == Id):
            Position += I
            return Position

        inc I

    assert(false, "Node not found in parent's children.")

proc getNodeValue[IdT, ValT](this: SyntaxTree[IdT, ValT], Id: NodeId): ValT =
  ## Serialize the node attributes to a value representation. This
  ## should uniquely distinguish nodes of the same kind. Note that this
  ## function just returns a representation of the node value, not
  ## considering descendants.
  return this.getNodeValue(this.getNode(Id))

proc getNodeValue[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Node: Node[IdT, ValT]): ValT =
  return this.opts.getNodeValue(Node.ASTNode)

func setLeftMostDescendants[IdT, ValT](this: var SyntaxTree[IdT, ValT]) =
  for Leaf in this.Leaves:
    this.getMutableNode(Leaf).LeftMostDescendant = Leaf
    var
      Parent = Leaf
      Cur = Leaf
    while (Parent = this.getNode(Cur).Parent ; Parent.isValid()) and
          this.getNode(Parent).subnodes[0] == Cur:

       Cur = Parent
       this.getMutableNode(Cur).LeftMostDescendant = Leaf

func postInc[T](thing: var T): T =
  let tmp = thing
  inc thing
  return tmp

func getSubtreeBfs[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT], Root: NodeId): seq[NodeId] =

  var Ids: seq[NodeId]
  var Expanded = 0
  Ids.add(Root)
  while Expanded < Ids.len():
    for Subnode in Tree.getNode(Ids[postInc(Expanded)]).subnodes:
      Ids.add(Subnode)

  return Ids


proc initTree[IdT, ValT](this: var SyntaxTree[IdT, ValT]) =
  echov this.getNode(this.getRootId()).subnodes

  setLeftMostDescendants(this)
  var PostorderId = 0;
  this.PostorderIds.setLen(this.getSize())
  proc PostorderTraverse(this: SyntaxTree[IdT, ValT], Id: NodeId) =
    for Subnode in this.getNode(Id).subnodes:
      PostorderTraverse(this, Subnode)

    this.PostorderIds[Id] = PostorderId
    inc PostorderId

  PostorderTraverse(this, this.getRootId())
  this.NodesBfs = getSubtreeBfs(this, this.getRootId())


type
  ASTDiff[IdT, ValT] = ref object
    T1, T2: SyntaxTree[IdT, ValT]
    map: Mapping
    opts: CmpOpts[IdT, ValT]

  HeightLess[IdT, ValT] = object
    ## Compares nodes by their depth.
    Tree: SyntaxTree[IdT, ValT]
    Id: NodeId

  PriorityList[IdT, ValT] = object
    ## Priority queue for nodes, sorted descendingly by their height.
    Tree: SyntaxTree[IdT, ValT]
    Container: seq[NodeId]
    List: HeapQueue[HeightLess[IdT, ValT]]

func initHeightLess[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT], Id: NodeId): HeightLess[IdT, ValT] =
  HeightLess[IdT, ValT](Tree: Tree, Id: Id)

func `<`[IdT, ValT](h1, h2: HeightLess[IdT, ValT]): bool =
  return h1.Tree.getNode(h1.Id).Height < h2.Tree.getNode(h2.Id).Height


  # std::priority_queue<NodeId, seq<NodeId>, HeightLess[IdT, ValT]>
  #     List;

func initPriorityList[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT]): PriorityList[IdT, ValT] =
  result.Tree = Tree

func push[IdT, ValT](this: var PriorityList[IdT, ValT], id: NodeId) =
  this.List.push(initHeightLess(this.Tree, id))

func top[T](queue: HeapQueue[T]): T = queue[queue.len() - 1]

func peekMax[IdT, ValT](this: PriorityList[IdT, ValT]): int =
  if this.List.len() == 0:
    return 0

  else:
    return this.Tree.getNode(this.List.top().Id).Height


func pop[IdT, ValT](this: var PriorityList[IdT, ValT]): seq[NodeId] =
  let Max = peekMax(this)
  if Max == 0:
    return

  while peekMax(this) == Max:
    result.add(this.List.top().Id)
    this.List.del(this.List.len() - 1)

  ## TODO this is here to get a stable output, not a good heuristic
  sort(result)


func open[IdT, ValT](this: var PriorityList[IdT, ValT], Id: NodeId) =
  ## \brief add all subnodes in the input list
  for Subnode in this.Tree.getNode(Id).subnodes:
    this.push(Subnode)

proc isMatchingPossible[IdT, ValT](
    this: ASTDiff[IdT, ValT], Id1, Id2: NodeId): bool =
  ## Returns false if the nodes must not be mached.
  assertRefFields(this)
  return this.opts.isMatchingAllowed(
    this.T1.getNode(Id1),
    this.T2.getNode(Id2))

proc identical[IdT, ValT](this: ASTDiff[IdT, ValT], Id1, Id2: NodeId): bool =
  ## Returns true if the two subtrees are isomorphic to each other.
  let
    N1 = this.T1.getNode(Id1)
    N2 = this.T2.getNode(Id2)

  if N1.subnodes.len() != N2.subnodes.len() or
     not isMatchingPossible(this, Id1, Id2) or
     this.T1.getNodeValue(Id1) != this.T2.getNodeValue(Id2):
      return false

  for Id in 0 ..< N1.subnodes.len():
    if not identical(this, N1.subnodes[Id], N2.subnodes[Id]):
      return false

  return true



proc matchTopDown[IdT, ValT](this: ASTDiff[IdT, ValT]): Mapping =
  ## Returns a mapping of identical subtrees.
  var
    L1 = initPriorityList(this.T1)
    L2 = initPriorityList(this.T2)
    M = initMapping(this.T1.getSize() + this.T2.getSize())

  L1.push(this.T1.getRootId())
  L2.push(this.T2.getRootId())

  var
    Max1: int = 0
    Max2: int = 0

  # until subtree of necessary height hasn't been reached
  while this.opts.MinHeight <
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
      for Id1 in H1:
        for Id2 in H2:
          # if pair of trees is isomorphic
          if identical(this, Id1, Id2) and
             not M.hasSrc(Id1) and
             not M.hasDst(Id2):

            for I in 0 ..< this.T1.getNumberOfDescendants(Id1):
              M.link(Id1 + I, Id2 + I)

      # so we basically determine if there is any isomorphic
      # mapping between either (1) roots two highest subforests
      # or (2) root and subnodes of a root in other tree
      for Id1 in H1:
        # if there is unmatched forest root in first forest
        if not M.hasSrc(Id1):
          # insert it's subnodes
          L1.open(Id1)

      for Id2 in H2:
        # do the same for other forest
        if not M.hasDst(Id2):
          L2.open(Id2)

  return M

proc getSubtreePostorder[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT], Root: NodeId): seq[NodeId] =
  var Postorder: seq[NodeId]
  proc Traverse(Id: NodeId) =
    let N = Tree.getNode(Id)
    for Subnode in N.subnodes:
      Traverse(Subnode)

    Postorder.add(Id)

  Traverse(Root)
  return Postorder





type
  Subtree[IdT, ValT] = ref object
    Tree: SyntaxTree[IdT, ValT] ## The parent tree.
    RootIds: seq[NodeId] ## Maps SubNodeIds to original ids. Maps subtree
    ## nodes to their leftmost descendants wtihin the subtree.
    LeftMostDescendants: seq[SubNodeId]
    KeyRoots: seq[SubNodeId]


proc getSize[IdT, ValT](this: Subtree[IdT, ValT]): int =
  return this.RootIds.len()

proc getLeftMostDescendant[IdT, ValT](
    this: Subtree[IdT, ValT], Id: SubNodeId): SubNodeId =
  assert(0 < Id and Id <= this.getSize(), "Invalid subtree node index.")
  return this.LeftMostDescendants[Id - 1]

proc computeKeyRoots[IdT, ValT](this: var SubTree[IdT, ValT], Leaves: int) =
  this.KeyRoots.setLen(Leaves)
  var Visited: HashSet[int]
  var K: int = Leaves - 1
  var I = initSubNodeId(this.getSize())
  while 0 < I:
    let LeftDesc: SubNodeId = this.getLeftMostDescendant(I)
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
    this.Tree.PostorderIds[this.getIdInRoot(SubNodeId(1))])

proc setLeftMostDescendants[IdT, ValT](this: var Subtree[IdT, ValT]): int =
  ## Returns the number of leafs in the subtree.
  var NumLeaves = 0
  this.LeftMostDescendants.setLen(this.getSize())
  for I in 0 ..< this.getSize():
    var SI = initSubNodeId(I + 1);
    let N = this.getNode(SI)
    NumLeaves += (if N.isLeaf(): 1 else: 0)
    assert(
      I == this.Tree.PostorderIds[this.getIdInRoot(SI)] -
        this.getPostorderOffset(),
      "Postorder traversal in subtree should correspond to " &
      "traversal in the root tree by a constant offset.")

    this.LeftMostDescendants[I] = SubNodeId(
        this.Tree.PostorderIds[N.LeftMostDescendant] -
        this.getPostorderOffset())

  return NumLeaves



proc initSubtree[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT],
    SubtreeRoot: NodeId
  ): Subtree[IdT, ValT] =

  result = Subtree[IdT, ValT](Tree: Tree)

  result.RootIds = getSubtreePostorder(Tree, SubtreeRoot)
  let NumLeaves: int = setLeftMostDescendants(result)
  computeKeyRoots(result, NumLeaves)

proc getNodeValue[IdT, ValT](
    this: Subtree[IdT, ValT], Id: SubNodeId): ValT =
  return this.Tree.getNodeValue(this.getIdInRoot(Id))



type
  ZhangShashaMatcher[IdT, ValT] = object
    ## Implementation of Zhang and Shasha's Algorithm for tree edit distance.
    ## Computes an optimal mapping between two trees using only
    ## insertion, deletion and update as edit actions (similar to the
    ## Levenshtein distance).

    DiffImpl: ASTDiff[IdT, ValT]
    S1: Subtree[IdT, ValT]
    S2: Subtree[IdT, ValT]
    treeDist: seq[seq[float]]
    forestDist: seq[seq[float]]

proc initZhangShashaMatcher[IdT, ValT](
    DiffImpl: ASTDiff[IdT, ValT],
    T1, T2: SyntaxTree[IdT, ValT],
    Id2, Id1: NodeId
  ): ZhangShashaMatcher[IdT, ValT] =

  result.S1 = initSubtree(T1, Id1)
  result.S2 = initSubtree(T2, Id2)
  result.DiffImpl = DiffImpl

  echov result.S1.getSize(), result.S2.getSize()
  result.treeDist.setLen(result.S1.getSize() + 1)
  result.forestDist.setLen(result.S1.getSize() + 1)

  for it in mitems(result.treeDist):
    it.setlen(result.S2.getSize() + 1)

  for it in mitems(result.forestDist):
    it.setlen(result.S2.getSize() + 1)

func `<`(
    Id1: SubNodeId | NodeId | int,
    Id2: SubNodeId | NodeId | int
  ): bool =
  int(Id1) < int(Id2)


func `==`(
    Id1: SubNodeId | NodeId | int,
    Id2: SubNodeId | NodeId | int
  ): bool =
  int(Id1) == int(Id2)

func `<=`(
    Id1: SubNodeId | NodeId | int,
    Id2: SubNodeId | NodeId | int
  ): bool =
  int(Id1) <= int(Id2)

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
    this: ZhangShashaMatcher[IdT, ValT], Id1, Id2: SubNodeId): float =

  if not this.DiffImpl.isMatchingPossible(
    this.S1.getIdInRoot(Id1),
    this.S2.getIdInRoot(Id2)
  ):

    return high(float)

  else:
    if this.S1.getNodeValue(Id1) == this.S2.getNodeValue(Id2):
      return 0

    else:
      ## IMPLEMENT weighted node update cost that accounts for
      ## the value similarity
      return UpdateCost


proc computeforestDist[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT],
    Id1, Id2: SubNodeId
  ) =

  assert(Id1 > 0 and Id2 > 0, "Expecting offsets greater than 0.")
  var
    LMD1 = this.S1.getLeftMostDescendant(Id1)
    LMD2 = this.S2.getLeftMostDescendant(Id2)

  this.forestDist[LMD1][LMD2] = 0

  var D1 = LMD1 + 1
  while D1 < Id1:
    this.forestDist[D1][LMD2] = this.forestDist[D1 - 1][LMD2] + DeletionCost
    var D2 = LMD2 + 1

    while D2 <= Id2:
      this.forestDist[LMD1][D2] = this.forestDist[LMD1][D2 - 1] + InsertionCost
      let
        DLMD1 = this.S1.getLeftMostDescendant(D1)
        DLMD2 = this.S2.getLeftMostDescendant(D2)

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
  for Id1 in this.S1.KeyRoots:
    for Id2 in this.S2.KeyRoots:
      computeforestDist(this, Id1, Id2)



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
    FirstRow     = this.S1.getLeftMostDescendant(LastRow)
    FirstCol     = this.S2.getLeftMostDescendant(LastCol)
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
          LMD1: SubNodeId = this.S1.getLeftMostDescendant(Row)
          LMD2: SubNodeId = this.S2.getLeftMostDescendant(Col)

        if LMD1 == this.S1.getLeftMostDescendant(LastRow) and
           LMD2 == this.S2.getLeftMostDescendant(LastCol):

          let
            Id1 = this.S1.getIdInRoot(Row)
            Id2 = this.S2.getIdInRoot(Col)

          assert(
            this.DiffImpl.isMatchingPossible(Id1, Id2),
            "These nodes must not be matched.")

          Matches.add((Id1, Id2))
          dec Row
          dec Col

        else:
          TreePairs.add((Row, Col))
          Row = LMD1
          Col = LMD2

  echov this.forestDist
  echov this.treeDist

  return Matches





proc addOptimalMapping[IdT, ValT](
    this: var ASTDiff[IdT, ValT], M: var Mapping, Id2, Id1: NodeId) =
  ## Uses an optimal albeit slow algorithm to compute a mapping
  ## between two subtrees, but only if both have fewer nodes than
  ## MaxSize.
  if this.opts.MaxSize < max(
    this.T1.getNumberOfDescendants(Id1),
    this.T2.getNumberOfDescendants(Id2)):
      return

  var Matcher = initZhangShashaMatcher(this, this.T1, this.T2, Id1, Id2)
  let R = Matcher.getMatchingNodes()
  echov R
  for Tuple in R:
    let Src = Tuple[0]
    let Dst = Tuple[0]
    if not M.hasSrc(Src) and not M.hasDst(Dst):
      M.link(Src, Dst);

proc getJaccardSimilarity[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    M: Mapping,
    Id1, Id2: NodeId
  ): float =

  ## Computes the ratio of common descendants between the two nodes.
  ## Descendants are only considered to be equal when they are mapped in
  ## M.
  var CommonDescendants = 0
  let N1 = this.T1.getNode(Id1)
  # Count the common descendants, excluding the subtree root.
  var Src = Id1 + 1
  while Src <= N1.RightMostDescendant:
    let Dst = M.getDst(Src)
    CommonDescendants += int(
        Dst.isValid() and this.T2.isInSubtree(Dst, Id2))

    inc Src
  # We need to subtract 1 to get the number of descendants excluding the
  # root.
  let
    Denominator = this.T1.getNumberOfDescendants(Id1) - 1 +
                  this.T2.getNumberOfDescendants(Id2) - 1 -
                  CommonDescendants

  # CommonDescendants is less than the size of one subtree.
  assert(Denominator >= 0, "Expected non-negative denominator.")
  if (Denominator == 0):
    return 0

  return CommonDescendants / Denominator



iterator items[IdT, ValT](tree: SyntaxTree[IdT, ValT]): NodeId =
  var start = tree.getRootId()
  let final = tree.getSize()
  while start < final:
    yield start
    inc start


proc findCandidate[IdT, ValT](
    this: var ASTDiff[IdT, ValT], M: var Mapping, Id1: NodeId): NodeId =
  ## Returns the node that has the highest degree of similarity.

  var HighestSimilarity = 0.0
  for Id2 in this.T2:
    if not this.isMatchingPossible(Id1, Id2):
      continue

    if M.hasDst(Id2):
      continue

    let Similarity = this.getJaccardSimilarity(M, Id1, Id2)
    if this.opts.MinSimilarity <= Similarity and
       HighestSimilarity < Similarity:

      HighestSimilarity = Similarity
      result = Id2


proc matchBottomUp[IdT, ValT](this: var ASTDiff[IdT, ValT], M: var Mapping) =
    let Postorder = getSubtreePostorder(this.T1, this.T1.getRootId())
    # for all nodes in left, if node itself is not matched, but
    # has any children matched
    for Id1 in Postorder:
      if Id1 == this.T1.getRootId() and
         not M.hasSrc(this.T1.getRootId()) and
         not M.hasDst(this.T2.getRootId()):

        if isMatchingPossible(this, this.T1.getRootId(), this.T2.getRootId()):
          M.link(this.T1.getRootId(), this.T2.getRootId())
          addOptimalMapping(this, M, this.T1.getRootId(), this.T2.getRootId())

        break

      let Matched = M.hasSrc(Id1)
      let N1      = this.T1.getNode(Id1)

      let Matchedsubnodes = anyIt(N1.subnodes, M.hasSrc(it))

      #  if it is a valid candidate and matches criteria for
      # minimum number of shares subnodes
      if (Matched or not Matchedsubnodes):
        continue

      let Id2 = this.findCandidate(M, Id1)
      if Id2.isValid():
        # add node to mapping
        M.link(Id1, Id2)
        # if max of number of subnodes does not exceed threshold
        addOptimalMapping(this, M, Id1, Id2)


proc computeMapping[IdT, ValT](this: var ASTDiff[IdT, ValT]) =
  ## Matches nodes one-by-one based on their similarity.
  this.map = matchTopDown(this)
  if (this.opts.StopAfterTopDown):
    return

  matchBottomUp(this, this.map)

proc computeChangeKinds[IdT, ValT](this: ASTDiff[IdT, ValT], M: var Mapping)
  ## Compute Change for each node based on similarity.


proc initASTDiff[IdT, ValT](
    T1, T2: SyntaxTree[IdT, ValT],
    opts: CmpOpts[IdT, ValT]
  ): ASTDiff[IdT, ValT] =
  assert(not isNil(T1))
  assert(not isNil(T2))
  new(result)
  result.T1 = T1
  result.T2 = T2
  result.opts = opts
  computeMapping(result)
  computeChangeKinds(result, result.map)


proc getMapped[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    Tree: SyntaxTree[IdT, ValT], Id: NodeId): NodeId =
  ## Returns the ID of the node that is mapped to the given node in
  ## SourceTree.
  result = initNodeId()
  if cast[int](Tree) == cast[int](this.T1):
    return this.map.getDst(Id)

  assert(cast[int](Tree) == cast[int](this.T2), "Invalid tree.")

  return this.map.getSrc(Id)

proc haveSameParents[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    M: Mapping, Id1, Id2: NodeId
  ): bool =

  ## Returns true if the nodes' parents are matched.
  let
    P1 = this.T1.getNode(Id1).Parent
    P2 = this.T2.getNode(Id2).Parent

  return (P1.isInvalid() and P2.isInvalid()) or
         (P1.isValid() and P2.isValid() and M.getDst(P1) == P2);


type
  PreorderVisitor[IdT, ValT] = object
    ## Sets Height, Parent and subnodes for each node.
    Id: int
    Depth: int
    Parent: NodeId
    Tree: SyntaxTree[IdT, ValT]

proc initPreorderVisitor[IdT, ValT](
    tree: SyntaxTree[IdT, ValT]): PreorderVisitor[IdT, ValT] =
  result.Tree = tree
  result.Parent = initNodeId()

proc PreTraverse[IdT, ValT](
    this: var PreorderVisitor[IdT, ValT],
    node: TreeMirror[IdT, ValT]
  ): (NodeId, NodeId) =

  let MyId = initNodeId(this.Id)
  echov "Pre traverse start ID", this.Id
  this.Tree.Nodes.add(Node[IdT, ValT]())
  var N = this.Tree.getMutableNode(MyId)
  N.Parent = this.Parent
  N.Depth = this.Depth
  N.ASTNode = node.id

  if this.Parent.isValid():
    echov "Add ", MyId, "as subnode"
    var P = this.Tree.getMutableNode(this.Parent)
    P.subnodes.add(MyId)

  this.Parent = MyId
  inc this.Id
  inc this.Depth
  return (MyId, this.Tree.getNode(MyId).Parent)

proc PostTraverse[IdT, ValT](
    this: var PreorderVisitor[IdT, ValT],
    State: (NodeId, NodeId)
  ) =

  let (MyId, PreviousParent) = State;
  assert(MyId.isValid(), "Expecting to only traverse valid nodes.")
  this.Parent = PreviousParent
  dec this.Depth
  var N = this.Tree.getMutableNode(MyId)
  N.RightMostDescendant = initNodeId(this.Id - 1)
  assert(
    0 <= N.RightMostDescendant and
    N.RightMostDescendant < this.Tree.getSize(),
    "Rightmost descendant must be a valid tree node.")

  if N.isLeaf():
    this.Tree.Leaves.add(MyId)

  N.Height = 1
  for Subnode in N.subnodes:
    N.Height = min(
      N.Height,
      1 + this.Tree.getNode(Subnode).Height)

proc Traverse[IdT, ValT](
    this: var PreorderVisitor[IdT, ValT],
    node: TreeMirror[IdT, ValT]
  ) =

  let SavedState = PreTraverse(this, node)
  for sub in items(node.subnodes):
    Traverse(this, sub)

  PostTraverse(this, SavedState)

proc initSyntaxTree[IdT, ValT](
    opts: CmpOpts[IdT, ValT]): SyntaxTree[IdT, ValT] =
  result = SyntaxTree[IdT, ValT](opts: opts)

proc initSyntaxTree[IdT, ValT](
    opts: CmpOpts[IdT, ValT],
    N: TreeMirror[IdT, ValT]
  ): SyntaxTree[IdT, ValT] =

  result = initSyntaxTree(opts)
  var walker = initPreorderVisitor[IdT, ValT](result)
  echov walker.Id
  walker.Traverse(N)
  initTree(result)

proc computeChangeKinds[IdT, ValT](
    this: ASTDiff[IdT, ValT], M: var Mapping) =
  for Id1 in this.T1:
    if not M.hasSrc(Id1):
      this.T1.getMutableNode(Id1).Change = Delete
      this.T1.getMutableNode(Id1).Shift -= 1;

  for Id2 in this.T2:
    if not M.hasDst(Id2):
      this.T2.getMutableNode(Id2).Change = Insert
      this.T2.getMutableNode(Id2).Shift -= 1

  for Id1 in this.T1.NodesBfs:
    let Id2 = M.getDst(Id1)
    if Id2.isInvalid():
      continue

    if not this.haveSameParents(M, Id1, Id2) or
       this.T1.findPositionInParent(Id1, true) !=
       this.T2.findPositionInParent(Id2, true):

      this.T1.getMutableNode(Id1).Shift -= 1
      this.T2.getMutableNode(Id2).Shift -= 1

  for Id2 in this.T2.NodesBfs:
    let Id1 = M.getSrc(Id2)
    if Id1.isInvalid():
      continue

    var
      N1 = this.T1.getMutableNode(Id1)
      N2 = this.T2.getMutableNode(Id2)

    if (Id1.isInvalid()):
      continue

    if not haveSameParents(this, M, Id1, Id2) or
       this.T1.findPositionInParent(Id1, true) !=
       this.T2.findPositionInParent(Id2, true):

      N1.Change = Move
      N2.Change = Move

    if this.T1.getNodeValue(Id1) != this.T2.getNodeValue(Id2):
      N2.Change = if N1.Change == Move:
                    UpdateMove
                  else:
                    Update

      N1.Change = N2.Change


proc `$`[IdT, ValT](Tree: SyntaxTree[IdT, ValT], Id: NodeId): string =
  if (Id.isInvalid()):
    result = "None"
    return

  result = $Tree.getNode(Id).getNodeKind(Tree.opts).int
  result &= ": " & $Tree.getNodeValue(Id)
  result &= "(" & $Id & ")"


proc printDstChange[IdT, ValT](
    Diff: ASTDiff[IdT, ValT],
    SrcTree: SyntaxTree[IdT, ValT],
    DstTree: SyntaxTree[IdT, ValT],
    Dst: NodeId
  ): string =
    let DstNode = DstTree.getNode(Dst)
    let Src     = Diff.getMapped(DstTree, Dst)
    case DstNode.Change:
      of None:
        result = "None"

      of Delete:
        assert(false, "The destination tree can't have deletions.")

      of Update:
        result &= "Update "
        result &= $SrcTree.getNodeValue(Src);
        result &= " to " & $DstTree.getNodeValue(Dst)

      of Move, UpdateMove, Insert:
        case DstNode.Change:
          of Insert: result &= "Insert"
          of Move: result &= "Move"
          of UpdateMove: result &= "Update and Move"
          else: discard

        result &= " "
        result &= $DstTree.getNodeValue(Dst)
        result &= " into "
        result &= $DstTree.getNodeValue(DstNode.Parent)
        result &= " at " & $DstTree.findPositionInParent(Dst)

type
  NodeKind = enum NInt, NFloat, NString
  NodeValue = object
    case kind: NodeKind
      of NInt:
        intVal: int

      of NFloat:
        floatVal: float

      of NString:
        strVal: string
func `==`(v1, v2: NodeValue): bool =
  v1.kind == v2.kind and (
    case v1.kind:
      of NInt: v1.intVal == v2.intVal
      of NFloat: v1.floatVal == v2.floatVal
      of Nstring: v1.strVal == v2.strVal
  )


proc main() =
  block:
    type
      RealNode = ref object
        value: string
        kind: int
        sub: seq[RealNode]

      IdT  = RealNode
      ValT = string;

    proc ast(
      value: string, kind: int, sub: varargs[RealNode]): RealNode =

      RealNode(value: value, kind: kind, sub: @sub)

    let src = ast(
      "main", 0,
      ast("sub-1", 1), ast("sub-2", 2), ast("subnode", 0))

    let dst = ast(
      "main",
      0,
      ast("sub-1", 1), ast("sub-2'", 2), ast("sub-3", 3))

    var Src = mirror[IdT, ValT](
      src,
      mirror[IdT, ValT](src.sub[0]),
      mirror[IdT, ValT](src.sub[1]))

    var Dst = mirror[IdT, ValT](
      dst,
      mirror[IdT, ValT](dst.sub[0]),
      mirror[IdT, ValT](dst.sub[1]),
      mirror[IdT, ValT](dst.sub[2]))

    let opts = initCmpOpts[IdT, ValT](
      proc(id: IdT): ValT = id.value,
      proc(id: IdT): int = id.kind)

    var SrcTree = initSyntaxTree(opts, Src)
    var DstTree = initSyntaxTree(opts, Dst)
    var Diff = initASTDiff(SrcTree, DstTree, opts)
    echov Diff.map
    echov SrcTree.PostorderIds
    echov DstTree.PostorderIds

    for Dst in DstTree:
      echov Dst
      let Src = Diff.getMapped(DstTree, Dst)
      if (Src.isValid()):
        echov "is valid", Src, " -> ", Dst
        let src = SrcTree $ Src
        let dst = DstTree $ Dst
        echo("Match ", src, " to ", dst)

      echo printDstChange(Diff, SrcTree, DstTree, Dst)

  echo "done"

  block:

    type
      RealNode = ref object
        case kind: NodeKind
          of NInt:
            intVal: int

          of NFloat:
            floatVal: float

          of NString:
            strVal: string

        sub: seq[RealNode]


    proc ast(val: int | float | string, sub: varargs[RealNode]): RealNode =
      when val is int:
        result = RealNode(kind: NInt, intVal: val)

      elif val is float:
        result = RealNode(kind: NFloat, floatVal: val)

      else:
        result = RealNode(kind: NString, strVal: val)

      result.sub = @sub


    let src = ast(
      "toplevel",
      ast(1),
      ast(1.2),
      ast("subnode"))

    let dst = ast(
      "toplevel",
      ast(22),
      ast(1.2),
      ast("subnode'"))

    type
      IdT  = RealNode
      ValT = NodeValue

    let Src = mirror[IdT, ValT](
      src,
      mirror[IdT, ValT](src.sub[0]),
      mirror[IdT, ValT](src.sub[1]))

    let Dst = mirror[IdT, ValT](
      dst,
      mirror[IdT, ValT](dst.sub[0]),
      mirror[IdT, ValT](dst.sub[1]),
      mirror[IdT, ValT](dst.sub[2]))


    proc toValue(node: RealNode): NodeValue =
      result = NodeValue(kind: node.kind)
      case node.kind:
        of NInt: result.intVal = node.intVal
        of NFloat: result.floatVal = node.floatVal
        of NString: result.strVal = node.strVal

    let opts = initCmpOpts[IdT, ValT](
      proc(id: IdT): ValT = toValue(id),
      proc(id: IdT): int = int(id.kind))

    var SrcTree = initSyntaxTree(opts, Src)
    var DstTree = initSyntaxTree(opts, Dst)
    var Diff = initASTDiff(SrcTree, DstTree, opts)

    for Dst in DstTree:
      echov Dst
      let Src = Diff.getMapped(DstTree, Dst)
      if (Src.isValid()):
        echov "is valid", Src, " -> ", Dst
        let src = SrcTree $ Src
        let dst = DstTree $ Dst
        echo("Match ", src, " to ", dst)

      echo printDstChange(Diff, SrcTree, DstTree, Dst)

main()
echo "main ok"
