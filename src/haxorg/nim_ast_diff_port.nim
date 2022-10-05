import
  std/[
    sequtils,
    heapqueue,
    algorithm
  ]

const InvalidNodeOffset = -1

type
  NodeId = object
    ## Within a tree, this identifies a node by its preorder offset.
    Offset: int ## Offset in the postorder iteratio

func initNodeId(Offset: int = InvalidNodeOffset): NodeId =
  NodeId(Offset: Offset)

func toInt(id: NodeId): int = id.Offset
func inc(id: var NodeId) = inc id.Offset
func dec(id: var NodeId) = dec id.Offset
func isValid(id: NodeId): bool = id.Offset != InvalidNodeOffset
func isInvalid(id: NodeId): bool = id.Offset == InvalidNodeOffset

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
   this.SrcToDst[Src.toInt()] = Dst
   this.DstToSrc[Dst.toInt()] = Src

func getDst(this: Mapping, Src: NodeId): NodeId = this.SrcToDst[Src.toInt()]
func getSrc(this: Mapping, Dst: NodeId): NodeId = this.DstToSrc[Dst.toInt()]
func hasSrc(this: Mapping, Dst: NodeId): bool = this.getSrc(Dst).isValid()
func hasSrc(this: Mapping, Src: NodeId): bool = this.getDst(Src).isValid()

type
  ChangeKind = enum
    None
    Delete ## (Src): delete node Src.
    Update ## (Src, Dst): update the value of node Src to match Dst.
    Insert ## (Src, Dst, Pos): insert Src as child of Dst at offset Pos.
    Move ## (Src, Dst, Pos): move Src to be a child of Dst at offset Pos.
    UpdateMove ## Same as Move plus Update.

type
  ASTNodeKind = object
    value: int

func initASTNodeKind(kind: int): ASTNodeKind =
  ASTNodeKind(value: kind)

func `==`(k1, k2: ASTNodeKind): bool = k1.value == k2.value

type
  CmpOpts[IdT, ValT] = object
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


  Node[IdT, ValT] = object
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
    Subnodes: seq[NodeId]  ## Explicit list of the subnode IDS
    Change: ChangeKind



func getNodeValue[IdT, ValT](
    self: CmpOpts[IdT, ValT], id: IdT): ValT =
  return self.getNodeValueImpl(id)

func getNodeKind[IdT, ValT](
    self: CmpOpts[IdT, ValT], id: IdT): int =
  return self.getNodeKindImpl(id)


func isMatchingAllowed[IdT, ValT](
    self: CmpOpts[IdT, ValT], N1, N2: Node[IdT, ValT]): bool =
  ## Returns false if the nodes should never be matched.
  if self.isMatchingAllowedImpl.isNil():
     return self.getNodeKind(N1) == self.getNodeKind(N2);

  else:
     return self.isMatchingAllowedImpl(N1, N2)


func initCmpOpts[IdT, ValT](): CmpOpts =
  CmpOpts(
    MinHeight: 2,
    MinSimilarity: 0.5,
    MaxSize: 100,
    StopAfterTopDown: false,
  )


func getNodeKind[IdT, ValT](
    this: Node[IdT, ValT], opts: CmpOpts[IdT, ValT]): ASTNodeKind =
  return opts.getNodeKind(this.ASTNode)

func isLead[IdT, ValT](this: Node[IdT, ValT]): bool =
  this.subnodes.len() == 0

type
  SyntaxTree[IdT, ValT] = ref object
    ## SyntaxTree objects represent subtrees of the AST.
    ##
    ## There are only two instances of the SyntaxTree class during comparison
    ## - destination and source. Structure is not recursive in tiself -
    ## subnodes are determined based on the Node::Subnodes field which
    ## explicitly stores list of subnode ids.


    Nodes: seq[Node[IdT, ValT]]     ## Nodes in preorder.
    Leaves: seq[NodeId]
    PostorderIds: seq[int]     ## Maps preorder indices to postorder ones.
    NodesBFS: seq[NodeId]
    opts: CmpOpts[IdT, ValT]


func initSyntaxTree[IdT, ValT](
    opts: CmpOpts[IdT, ValT]): SyntaxTree


func initSyntaxTree[IdT, ValT](
    opts: CmpOpts[IdT, ValT],
    N: TreeMirror[IdT, ValT]): SyntaxTree[IdT, ValT] =
  ## Constructs a tree from an AST node.

func getSize[IdT, ValT](this: SyntaxTree[IdT, ValT]): int =
  return this.Nodes.size()

func getRootId[IdT, ValT](this: SyntaxTree[IdT, ValT]): NodeId =
  return initNodeId(0)


    # PreorderIterator             begin() const { return getRootId(); }
    # PreorderIterator             end() const { return getSize(); }
func getNode[IdT, ValT](
    this: SyntaxTree[IdT, ValT],
    Id: NodeId): Node[IdT, ValT] =
  return this.Nodes[Id]

    # const Node[IdT, ValT]& getNode(NodeId Id) const { return Nodes[Id]; }

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
  return this.getNode(Id).RightMostDescendant - Id + 1;


func isInSubtree[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Id, SubtreeRoot: NodeId): bool =
  return Id >= SubtreeRoot and
         Id <= this.getNode(SubtreeRoot).RightMostDescendant

func findPositionInParent[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Id: NodeId, Shifted: bool = false): int =
    let Parent = getNode(Id).Parent
    if (Parent.isInvalid()):
      return 0

    let Siblings = getNode(Parent).Subnodes
    let Position = 0;
    var I = 0
    var E = Siblings.size()
    while I < E:
        if (Shifted):
          Position += getNode(Siblings[I]).Shift;

        if (Siblings[I] == Id):
            Position += I
            return Position

        inc I

    assert(false, "Node not found in parent's children.")

func getNodeValue[IdT, ValT](this: SyntaxTree[IdT, ValT], Id: NodeId): ValT =
    ## Serialize the node attributes to a value representation. This
    ## should uniquely distinguish nodes of the same kind. Note that this
    ## function just returns a representation of the node value, not
    ## considering descendants.

    return getNodeValue(getNode(Id))

func getNodeValue[IdT, ValT](
    this: SyntaxTree[IdT, ValT], Node: Node[IdT, ValT]): ValT =
  return this.opts.getNodeValue(Node.ASTNode)

func setLeftMostDescendants[IdT, ValT](this: var SyntaxTree[IdT, ValT]) =
  for Leaf in this.Leaves:
    getMutableNode(Leaf).LeftMostDescendant = Leaf
    var
      Parent = Leaf
      Cur = Leaf
    while (Parent = getNode(Cur).Parent ; Parent.isValid()) and
          getNode(Parent).Subnodes[0] == Cur:

       Cur                                    = Parent
       getMutableNode(Cur).LeftMostDescendant = Leaf

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
    for Subnode in Tree.getNode(Ids[postInc(Expanded)]).Subnodes:
      Ids.push_back(Subnode)

  return Ids


func initTree[IdT, ValT](this: var SyntaxTree[IdT, ValT]) =
    setLeftMostDescendants();
    var PostorderId = 0;
    this.PostorderIds.resize(getSize());
    proc PostorderTraverse(Id: NodeId) =
      for Subnode in this.getNode(Id).Subnodes:
        PostorderTraverse(Subnode)

      this.PostorderIds[Id] = PostorderId
      inc PostorderId

    PostorderTraverse(getRootId())
    this.NodesBfs = getSubtreeBfs(this, getRootId())


type
  ASTDiff[IdT, ValT] = ref object
    T1, T2: SyntaxTree[IdT, ValT]
    TheMapping: Mapping
    Options: CmpOpts[IdT, ValT]

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


  # std::priority_queue<NodeId, std::vector<NodeId>, HeightLess[IdT, ValT]>
  #     List;

func initPriorityList[IdT, ValT](
    Tree: SyntaxTree[IdT, ValT]): PriorityList =
  result.Tree = Tree

func push[IdT, ValT](this: var PriorityList[IdT, ValT], id: NodeId) =
  this.List.push(initHeightLess(this.Tree, id))

func peekMax[IdT, ValT](this: PriorityList[IdT, ValT]): int =
  if (this.List.empty()):
    return 0

  return this.Tree.getNode(this.List.top()).Height


func pop[IdT, ValT](this: var PriorityList[IdT, ValT]): seq[NodeId] =
  let Max = peekMax()
  if (Max == 0):
    return

  while (peekMax() == Max):
      result.add(this.List.top());
      this.List.pop();

  ## TODO this is here to get a stable output, not a good heuristic
  sort(result, proc(h1, h2: HeightLess[IdT, ValT]): bool = h1.Id < h2.Id)


func open[IdT, ValT](this: var PriorityList[IdT, ValT], Id: NodeId) =
  ## \brief add all subnodes in the input list
  for Subnode in this.Tree.getNode(Id).Subnodes:
    this.push(Subnode)

func isMatchingPossible[IdT, ValT](
    this: ASTDiff[IdT, ValT], Id1, Id2: NodeId): bool =
  ## Returns false if the nodes must not be mached.
  return this.Options.isMatchingAllowed(
    this.T1.getNode(Id1), this.T2.getNode(Id2))

proc identical[IdT, ValT](this: ASTDiff[IdT, ValT], Id1, Id2: NodeId): bool =
  ## Returns true if the two subtrees are isomorphic to each other.
  let
    N1 = this.T1.getNode(Id1)
    N2 = this.T2.getNode(Id2)

  if N1.Subnodes.size() != N2.Subnodes.size() or
     not isMatchingPossible(Id1, Id2) or
     this.T1.getNodeValue(Id1) != this.T2.getNodeValue(Id2):
      return false

  for Id in 0 ..< N1.Subnodes.size():
      if not identical(N1.Subnodes[Id], N2.Subnodes[Id]):
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
  while this.Options.MinHeight <
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
          if identical(Id1, Id2) and
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
    for Subnode in N.Subnodes:
      Traverse(Subnode)

    Postorder.add(Id)

  Traverse(Root)
  return Postorder

type
  ZhangShashaMatcher[IdT, ValT] = object
    ## Implementation of Zhang and Shasha's Algorithm for tree edit distance.
    ## Computes an optimal mapping between two trees using only
    ## insertion, deletion and update as edit actions (similar to the
    ## Levenshtein distance).

    DiffImpl: ASTDiff[IdT, ValT]
    S1: Subtree[IdT, ValT]
    S2: Subtree[IdT, ValT]
    TreeDist: seq[seq[float]]
    ForestDist: seq[seq[float]]

proc initZhangShashaMatcher[IdT, ValT](
    DiffImpl: ASTDiff[IdT, ValT],
    T1, T2: SyntaxTree[IdT, ValT],
    Id2, Id1: NodeId
  ): ZhangShashaMatcher =

  result.S1 = initSubtree(T1, Id1)
  result.S2 = initSubtree(T2, Id2)

  result.TreeDist = newSeqWith[seq[float]](
    S1.getSize() + 1, neqSeqWith[float](S2.getSize() + 1, 0))

  result.ForestDist = newSeqWith[seq[float]](
    S1.getSize() + 1, neqSeqWith[float](S2.getSize() + 1, 0))

    std::vector<std::pair<NodeId, NodeId>> getMatchingNodes() {
        std::vector<std::pair<NodeId, NodeId>>       Matches;
        std::vector<std::pair<SubNodeId, SubNodeId>> TreePairs;
        computeTreeDist();
        bool RootNodePair = true;
        TreePairs.emplace_back(
            SubNodeId(S1.getSize()), SubNodeId(S2.getSize()));

        while (!TreePairs.empty()) {
            SubNodeId LastRow, LastCol, FirstRow, FirstCol, Row, Col;
            std::tie(LastRow, LastCol) = TreePairs.back();
            TreePairs.pop_back();
            if (!RootNodePair) { computeForestDist(LastRow, LastCol); }
            RootNodePair = false;
            FirstRow     = S1.getLeftMostDescendant(LastRow);
            FirstCol     = S2.getLeftMostDescendant(LastCol);
            Row          = LastRow;
            Col          = LastCol;
            while (Row > FirstRow or Col > FirstCol) {
                if (Row > FirstRow and
                    ForestDist[Row - 1][Col] + 1 == ForestDist[Row][Col]) {
                    --Row;
                } else if (
                    Col > FirstCol and
                    ForestDist[Row][Col - 1] + 1 == ForestDist[Row][Col]) {
                    --Col;
                } else {
                    SubNodeId LMD1 = S1.getLeftMostDescendant(Row);
                    SubNodeId LMD2 = S2.getLeftMostDescendant(Col);
                    if (LMD1 == S1.getLeftMostDescendant(LastRow) and
                        LMD2 == S2.getLeftMostDescendant(LastCol)) {
                        NodeId Id1 = S1.getIdInRoot(Row);
                        NodeId Id2 = S2.getIdInRoot(Col);
                        assert(
                            DiffImpl.isMatchingPossible(Id1, Id2) and
                            "These nodes must not be matched.");
                        Matches.emplace_back(Id1, Id2);
                        --Row;
                        --Col;
                    } else {
                        TreePairs.emplace_back(Row, Col);
                        Row = LMD1;
                        Col = LMD2;
                    }
                }
            }
        }
        return Matches;
    }

  private:
    ## We use a simple cost model for edit actions, which seems good
    ## enough. Simple cost model for edit actions. This seems to make the
    ## matching algorithm perform reasonably well. The values range
    ## between 0 and 1, or infinity if this edit action should always be
    ## avoided.
    static constexpr double DeletionCost  = 1;
    static constexpr double InsertionCost = 1;
    static constexpr double UpdateCost    = 1;

    double getUpdateCost(SubNodeId Id1, SubNodeId Id2) {
        if (!DiffImpl.isMatchingPossible(
                S1.getIdInRoot(Id1), S2.getIdInRoot(Id2))) {
            return std::numeric_limits<double>::max();
        } else {
            if (S1.getNodeValue(Id1) == S2.getNodeValue(Id2)) {
                return 0;
            } else {
                ## IMPLEMENT weighted node update cost that accounts for
                ## the value similarity
                return UpdateCost;
            }
        }
    }

    void computeTreeDist() {
        for (SubNodeId Id1 : S1.KeyRoots) {
            for (SubNodeId Id2 : S2.KeyRoots) {
                computeForestDist(Id1, Id2);
            }
        }
    }

    void computeForestDist(SubNodeId Id1, SubNodeId Id2) {
        assert(Id1 > 0 and Id2 > 0 and "Expecting offsets greater than 0.");
        SubNodeId LMD1         = S1.getLeftMostDescendant(Id1);
        SubNodeId LMD2         = S2.getLeftMostDescendant(Id2);
        ForestDist[LMD1][LMD2] = 0;
        for (SubNodeId D1 = LMD1 + 1; D1 <= Id1; ++D1) {
            ForestDist[D1][LMD2] = ForestDist[D1 - 1][LMD2] + DeletionCost;
            for (SubNodeId D2 = LMD2 + 1; D2 <= Id2; ++D2) {
                ForestDist[LMD1][D2] = ForestDist[LMD1][D2 - 1] +
                                       InsertionCost;
                SubNodeId DLMD1 = S1.getLeftMostDescendant(D1);
                SubNodeId DLMD2 = S2.getLeftMostDescendant(D2);
                if (DLMD1 == LMD1 and DLMD2 == LMD2) {
                    double UpdateCost  = getUpdateCost(D1, D2);
                    ForestDist[D1][D2] = std::min(
                        {ForestDist[D1 - 1][D2] + DeletionCost,
                         ForestDist[D1][D2 - 1] + InsertionCost,
                         ForestDist[D1 - 1][D2 - 1] + UpdateCost});
                    TreeDist[D1][D2] = ForestDist[D1][D2];
                } else {
                    ForestDist[D1][D2] = std::min(
                        {ForestDist[D1 - 1][D2] + DeletionCost,
                         ForestDist[D1][D2 - 1] + InsertionCost,
                         ForestDist[DLMD1][DLMD2] + TreeDist[D1][D2]});
                }
            }
        }
    }
};



proc addOptimalMapping[IdT, ValT](
    this: var ASTDiff[IdT, ValT], M: var Mapping, Id2, Id1: NodeId) =
  ## Uses an optimal albeit slow algorithm to compute a mapping
  ## between two subtrees, but only if both have fewer nodes than
  ## MaxSize.
  if this.Options.MaxSize < max(
    this.T1.getNumberOfDescendants(Id1),
    this.T2.getNumberOfDescendants(Id2)):
      return

  let Matcher = initZhangShashaMatcher(this, this.T1, this.T2, Id1, Id2)
  for Tuple in Matcher.getMatchingNodes():
    let Src = Tuple[0]
    let Dst = Tuple[0]
    if not M.hasSrc(Src) and not M.hasDst(Dst):
      M.link(Src, Dst);


proc matchBottomUp[IdT, ValT](this: var ASTDiff[IdT, ValT], M: var Mapping) =
    let Postorder = getSubtreePostorder(this.T1, this.T1.getRootId())
    # for all nodes in left, if node itself is not matched, but
    # has any children matched
    for Id1 in Postorder:
      if Id1 == this.T1.getRootId() and
         not M.hasSrc(this.T1.getRootId()) and
         not M.hasDst(this.T2.getRootId()):

        if isMatchingPossible(this.T1.getRootId(), this.T2.getRootId()):
          M.link(this.T1.getRootId(), this.T2.getRootId())
          addOptimalMapping(M, this.T1.getRootId(), this.T2.getRootId())

        break

      let Matched = M.hasSrc(Id1)
      let N1      = this.T1.getNode(Id1)

      bool MatchedSubnodes = std::any_of(
          N1.Subnodes.begin(), N1.Subnodes.end(), [&](NodeId Subnode) {
              return M.hasSrc(Subnode);
          });

      #  if it is a valid candidate and matches criteria for
      # minimum number of shares subnodes
      if (Matched or not MatchedSubnodes):
        continue

      let Id2 = findCandidate(M, Id1)
      if Id2.isValid():
        # add node to mapping
        M.link(Id1, Id2)
        # if max of number of subnodes does not exceed threshold
        addOptimalMapping(M, Id1, Id2)


proc computeMapping[IdT, ValT](this: var ASTDiff[IdT, ValT]) =
  ## Matches nodes one-by-one based on their similarity.
  this.TheMapping = matchTopDown(this)
  if (this.Options.StopAfterTopDown):
    return

  matchBottomUp(this, TheMapping)

proc computeChangeKinds(this: ASTDiff[IdT, ValT], M: var Mapping)
  ## Compute Change for each node based on similarity.


func initASTDiff[IdT, ValT](
    T1, this.T2: SyntaxTree[IdT, ValT], opts: CmpOpts[IdT, ValT]): ASTDiff =
  result.T1 = this.T2
  result.T2 = T2
  computeMapping(result)
  computeChangeKinds(result, result.TheMapping)


proc getMapped[IdT, ValT](
    Tree: var SyntaxTree[IdT, ValT], Id: NodeId): NodeId =
  ## Returns the ID of the node that is mapped to the given node in
  ## SourceTree.

  if (addr Tree == addr T1):
    return TheMapping.getDst(Id)

  assert(addr Tree == addr T2, "Invalid tree.")

  return TheMapping.getSrc(Id)

  private:
    ## Returns true if the nodes' parents are matched.
    bool haveSameParents(const Mapping& M, NodeId Id1, NodeId Id2) const {
        NodeId P1 = T1.getNode(Id1).Parent;
        NodeId P2 = T2.getNode(Id2).Parent;
        return (P1.isInvalid() and P2.isInvalid()) or
               (P1.isValid() and P2.isValid() and M.getDst(P1) == P2);
    }
    // Computes the ratio of common descendants between the two nodes.
    // Descendants are only considered to be equal when they are mapped in
    // M.
    double getJaccardSimilarity(const Mapping& M, NodeId Id1, NodeId Id2)
        const;
    // Returns the node that has the highest degree of similarity.
    NodeId findCandidate(const Mapping& M, NodeId Id1) const;
    const CmpOpts[IdT, ValT]& Options;
    template <typename IdT_, typename ValT_>
    friend class ZhangShashaMatcher;
};

// Sets Height, Parent and Subnodes for each node.
template <typename IdT, typename ValT>
struct PreorderVisitor {
    int                    Id = 0, Depth = 0;
    NodeId                 Parent;
    SyntaxTree[IdT, ValT]& Tree;
    PreorderVisitor(SyntaxTree[IdT, ValT]& Tree) : Tree(Tree) {}
    std::tuple<NodeId, NodeId> PreTraverse(
        TreeMirror[IdT, ValT] const& node) {
        NodeId MyId = Id;
        Tree.Nodes.emplace_back();
        Node[IdT, ValT]& N = Tree.getMutableNode(MyId);
        N.Parent           = Parent;
        N.Depth            = Depth;
        N.ASTNode          = node.id;

        if (Parent.isValid()) {
            Node[IdT, ValT]& P = Tree.getMutableNode(Parent);
            P.Subnodes.push_back(MyId);
        }

        Parent = MyId;
        ++Id;
        ++Depth;
        return std::make_tuple(MyId, Tree.getNode(MyId).Parent);
    }

    void PostTraverse(std::tuple<NodeId, NodeId> State) {
        NodeId MyId, PreviousParent;
        std::tie(MyId, PreviousParent) = State;
        assert(
            MyId.isValid() and "Expecting to only traverse valid nodes.");
        Parent = PreviousParent;
        --Depth;
        Node[IdT, ValT]& N    = Tree.getMutableNode(MyId);
        N.RightMostDescendant = Id - 1;
        assert(
            N.RightMostDescendant >= 0 and
            N.RightMostDescendant < Tree.getSize() and
            "Rightmost descendant must be a valid tree node.");
        if (N.isLeaf()) Tree.Leaves.push_back(MyId);
        N.Height = 1;
        for (NodeId Subnode : N.Subnodes) {
            N.Height = std::max(
                N.Height, 1 + Tree.getNode(Subnode).Height);
        }
    }

    void Traverse(TreeMirror[IdT, ValT] const& node) {
        auto SavedState = PreTraverse(node);
        for (auto sub : node.subnodes) {
            Traverse(sub);
        }
        PostTraverse(SavedState);
    }
};

template <typename IdT, typename ValT>
SyntaxTree[IdT, ValT]::SyntaxTree(
    CmpOpts[IdT, ValT] const& _opts)
    : opts(_opts) {}

template <typename IdT, typename ValT>
SyntaxTree[IdT, ValT]::SyntaxTree(
    CmpOpts[IdT, ValT] const& _opts,
    TreeMirror[IdT, ValT] const&        N)
    : SyntaxTree(_opts) {
    PreorderVisitor[IdT, ValT] PreorderWalker{*this};
    PreorderWalker.Traverse(N);
    initTree();
}




## \brief Identifies a node in a subtree by its postorder offset, starting
## at 1.
struct SubNodeId {
    int Id = 0;
    explicit SubNodeId(int Id) : Id(Id) {}
    explicit SubNodeId() = default;
               operator int() const { return Id; }
    SubNodeId& operator++() { return ++Id, *this; }
    SubNodeId& operator--() { return --Id, *this; }
    SubNodeId  operator+(int Other) const { return SubNodeId(Id + Other); }
};


template <typename IdT, typename ValT>
class Subtree {
  private:
    ## The parent tree.
    const SyntaxTree[IdT, ValT]& Tree;
    ## Maps SubNodeIds to original ids.
    std::vector<NodeId> RootIds;
    ## Maps subtree nodes to their leftmost descendants wtihin the
    ## subtree.
    std::vector<SubNodeId> LeftMostDescendants;

  public:
    std::vector<SubNodeId> KeyRoots;
    Subtree(const SyntaxTree[IdT, ValT]& Tree, NodeId SubtreeRoot)
        : Tree(Tree) {
        RootIds       = getSubtreePostorder[IdT, ValT](Tree, SubtreeRoot);
        int NumLeaves = setLeftMostDescendants();
        computeKeyRoots(NumLeaves);
    }

    int getSize() const { return RootIds.size(); }

    NodeId getIdInRoot(SubNodeId Id) const {
        assert(Id > 0 and Id <= getSize() and "Invalid subtree node index.");
        return RootIds[Id - 1];
    }

    const Node[IdT, ValT]& getNode(SubNodeId Id) const {
        return Tree.getNode(getIdInRoot(Id));
    }

    SubNodeId getLeftMostDescendant(SubNodeId Id) const {
        assert(Id > 0 and Id <= getSize() and "Invalid subtree node index.");
        return LeftMostDescendants[Id - 1];
    }
    ## Returns the postorder index of the leftmost descendant in the
    ## subtree.
    NodeId getPostorderOffset() const {
        return Tree.PostorderIds[getIdInRoot(SubNodeId(1))];
    }

    ValT getNodeValue(SubNodeId Id) const {
        return Tree.getNodeValue(getIdInRoot(Id));
    }

  private:
    ## Returns the number of leafs in the subtree.
    int setLeftMostDescendants() {
        int NumLeaves = 0;
        LeftMostDescendants.resize(getSize());
        for (int I = 0; I < getSize(); ++I) {
            SubNodeId              SI(I + 1);
            const Node[IdT, ValT]& N = getNode(SI);
            NumLeaves += N.isLeaf();
            assert(
                I == Tree.PostorderIds[getIdInRoot(SI)] -
                         getPostorderOffset() and
                "Postorder traversal in subtree should correspond to "
                "traversal in the root tree by a constant offset.");
            LeftMostDescendants[I] = SubNodeId(
                Tree.PostorderIds[N.LeftMostDescendant] -
                getPostorderOffset());
        }
        return NumLeaves;
    }

    void computeKeyRoots(int Leaves) {
        KeyRoots.resize(Leaves);
        std::unordered_set<int> Visited;
        int                     K = Leaves - 1;
        for (SubNodeId I(getSize()); I > 0; --I) {
            SubNodeId LeftDesc = getLeftMostDescendant(I);
            if (Visited.count(LeftDesc)) continue;
            assert(K >= 0 and "K should be non-negative");
            KeyRoots[K] = I;
            Visited.insert(LeftDesc);
            --K;
        }
    }
};








template <typename IdT, typename ValT>
double ASTDiff[IdT, ValT]::getJaccardSimilarity(
    const Mapping& M,
    NodeId         Id1,
    NodeId         Id2) const {
    int                    CommonDescendants = 0;
    const Node[IdT, ValT]& N1                = T1.getNode(Id1);
    // Count the common descendants, excluding the subtree root.
    for (NodeId Src = Id1 + 1; Src <= N1.RightMostDescendant; ++Src) {
        NodeId Dst = M.getDst(Src);
        CommonDescendants += int(
            Dst.isValid() and T2.isInSubtree(Dst, Id2));
    }
    // We need to subtract 1 to get the number of descendants excluding the
    // root.
    double Denominator = T1.getNumberOfDescendants(Id1) - 1 +
                         T2.getNumberOfDescendants(Id2) - 1 -
                         CommonDescendants;
    // CommonDescendants is less than the size of one subtree.
    assert(Denominator >= 0 and "Expected non-negative denominator.");
    if (Denominator == 0) { return 0; }
    return CommonDescendants / Denominator;
}


template <typename IdT, typename ValT>
NodeId ASTDiff[IdT, ValT]::findCandidate(const Mapping& M, NodeId Id1)
    const {
    NodeId Candidate;
    double HighestSimilarity = 0.0;
    for (NodeId const& Id2 : T2) {
        if (!isMatchingPossible(Id1, Id2)) { continue; }
        if (M.hasDst(Id2)) { continue; }
        double Similarity = getJaccardSimilarity(M, Id1, Id2);
        if (Similarity >= Options.MinSimilarity and
            Similarity > HighestSimilarity) {
            HighestSimilarity = Similarity;
            Candidate         = Id2;
        }
    }
    return Candidate;
}






template <typename IdT, typename ValT>
void ASTDiff[IdT, ValT]::computeChangeKinds(Mapping& M) {
    for (NodeId const& Id1 : T1) {
        if (!M.hasSrc(Id1)) {
            T1.getMutableNode(Id1).Change = ChangeKind::Delete;
            T1.getMutableNode(Id1).Shift -= 1;
        }
    }
    for (NodeId const& Id2 : T2) {
        if (!M.hasDst(Id2)) {
            T2.getMutableNode(Id2).Change = ChangeKind::Insert;
            T2.getMutableNode(Id2).Shift -= 1;
        }
    }
    for (NodeId const& Id1 : T1.NodesBfs) {
        NodeId Id2 = M.getDst(Id1);
        if (Id2.isInvalid()) { continue; }
        if (!haveSameParents(M, Id1, Id2) or
            T1.findPositionInParent(Id1, true) !=
                T2.findPositionInParent(Id2, true)) {
            T1.getMutableNode(Id1).Shift -= 1;
            T2.getMutableNode(Id2).Shift -= 1;
        }
    }
    for (NodeId const& Id2 : T2.NodesBfs) {
        NodeId Id1 = M.getSrc(Id2);
        if (Id1.isInvalid()) { continue; }
        Node[IdT, ValT]& N1 = T1.getMutableNode(Id1);
        Node[IdT, ValT]& N2 = T2.getMutableNode(Id2);
        if (Id1.isInvalid()) { continue; }
        if (!haveSameParents(M, Id1, Id2) or
            T1.findPositionInParent(Id1, true) !=
                T2.findPositionInParent(Id2, true)) {
            N1.Change = N2.Change = ChangeKind::Move;
        }

        if (T1.getNodeValue(Id1) != T2.getNodeValue(Id2)) {
            N2.Change = (N1.Change == ChangeKind::Move ? ChangeKind::UpdateMove : ChangeKind::Update);
            N1.Change = N2.Change;
        }
    }
}

template <typename T>
std::ostream& operator<<(std::ostream& os, std::vector<T> const& vec) {
    os << "[";
    for (int i = 0; i < vec.size(); ++i) {
        if (0 < i) { os << ", "; }
        os << vec[i];
    }
    os << "]";
    return os;
}

template <int Idx, typename... Args>
void writeIfIndex(std::ostream& os, std::variant<Args...> const& var) {
    if constexpr (Idx == 0) {
        os << Idx << " " << std::get<Idx>(var);
    } else {
        if (var.index() == Idx) {
            os << Idx << " " << std::get<Idx>(var);
        } else {
            writeIfIndex<Idx - 1>(os, var);
        }
    }
}

template <typename... Args>
std::ostream& operator<<(
    std::ostream&                os,
    std::variant<Args...> const& var) {
    writeIfIndex<sizeof...(Args) - 1>(os, var);
    return os;
}


template <typename IdT, typename ValT>
static void printNode(
    std::ostream&          OS,
    SyntaxTree[IdT, ValT]& Tree,
    NodeId                 Id) {
    if (Id.isInvalid()) {
        OS << "None";
        return;
    }
    OS << Tree.getNode(Id).getNodeKind(Tree.getOpts()).value;
    OS << ": " << Tree.getNodeValue(Id);
    OS << "(" << Id << ")";
}


template <typename IdT, typename ValT>
static void printDstChange(
    std::ostream&          OS,
    ASTDiff[IdT, ValT]&    Diff,
    SyntaxTree[IdT, ValT]& SrcTree,
    SyntaxTree[IdT, ValT]& DstTree,
    NodeId                 Dst) {
    const Node[IdT, ValT]& DstNode = DstTree.getNode(Dst);
    NodeId                 Src     = Diff.getMapped(DstTree, Dst);
    switch (DstNode.Change) {
        case ChangeKind::None: {
            break;
        }
        case ChangeKind::Delete: {
            assert(false and "The destination tree can't have deletions.");
        }
        case ChangeKind::Update: {
            OS << "Update ";
            OS << SrcTree.getNodeValue(Src);
            OS << " to " << DstTree.getNodeValue(Dst) << "\n";
            break;
        }
        case ChangeKind::Insert:
        case ChangeKind::Move:
        case ChangeKind::UpdateMove: {
            if (DstNode.Change == ChangeKind::Insert) {
                OS << "Insert";
            } else if (DstNode.Change == ChangeKind::Move) {
                OS << "Move";
            } else if (DstNode.Change == ChangeKind::UpdateMove) {
                OS << "Update and Move";
            }
            OS << " ";
            OS << DstTree.getNodeValue(Dst);
            OS << " into ";
            OS << DstTree.getNodeValue(DstNode.Parent);
            OS << " at " << DstTree.findPositionInParent(Dst) << "\n";
            break;
        }
    }
}

int main() {
    {
        struct RealNode {
            std::string           value;
            int                   kind;
            std::vector<RealNode> sub;
        };

        using IdT  = RealNode*;
        using ValT = std::string;

        auto src = RealNode{
            "main",
            0,
            {RealNode{"sub-1", 1},
             RealNode{"sub-2", 2},
             RealNode{"subnode"}}};

        auto dst = RealNode{
            "main",
            0,
            {RealNode{"sub-1", 1},
             RealNode{"sub-2'", 2},
             RealNode{"sub-3", 3}}};

        auto Src = TreeMirror[IdT, ValT]{
            &src,
            {TreeMirror[IdT, ValT]{&src.sub[0]},
             TreeMirror[IdT, ValT]{&src.sub[1]}}};

        auto Dst = TreeMirror[IdT, ValT]{
            &dst,
            {TreeMirror[IdT, ValT]{&dst.sub[0]},
             TreeMirror[IdT, ValT]{&dst.sub[1]},
             TreeMirror[IdT, ValT]{&dst.sub[2]}}};

        CmpOpts[IdT, ValT] Options{
            .getNodeValueImpl = [](IdT id) { return id->value; },
            .getNodeKindImpl  = [](IdT id) { return id->kind; }};

        SyntaxTree[IdT, ValT] SrcTree{Options, Src};
        SyntaxTree[IdT, ValT] DstTree{Options, Dst};
        ASTDiff[IdT, ValT]    Diff{SrcTree, DstTree, Options};

        std::cout << SrcTree.PostorderIds << "\n";
        std::cout << DstTree.PostorderIds << "\n";

        for (NodeId Dst : DstTree) {
            NodeId Src = Diff.getMapped(DstTree, Dst);
            if (Src.isValid()) {
                std::cout << "Match ";
                printNode(std::cout, SrcTree, Src);
                std::cout << " to ";
                printNode(std::cout, DstTree, Dst);
                std::cout << "\n";
            }

            printDstChange(std::cout, Diff, SrcTree, DstTree, Dst);
        }
    }

    std::cout << "-----------------\n";
    {
        struct RealNode {
            std::variant<int, double, std::string> value;
            std::vector<RealNode>                  sub;
        };

        auto src = RealNode{
            "toplevel", {RealNode{1}, RealNode{1.2}, RealNode{"subnode"}}};

        auto dst = RealNode{
            "toplevel",
            {RealNode{22}, RealNode{1.2}, RealNode{"subnode'"}}};

        using IdT  = RealNode*;
        using ValT = decltype(src.value);


        auto Src = TreeMirror[IdT, ValT]{
            &src,
            {TreeMirror[IdT, ValT]{&src.sub[0]},
             TreeMirror[IdT, ValT]{&src.sub[1]}}};

        auto Dst = TreeMirror[IdT, ValT]{
            &dst,
            {TreeMirror[IdT, ValT]{&dst.sub[0]},
             TreeMirror[IdT, ValT]{&dst.sub[1]},
             TreeMirror[IdT, ValT]{&dst.sub[2]}}};

        CmpOpts[IdT, ValT] Options{
            .getNodeValueImpl = [](IdT id) { return id->value; },
            .getNodeKindImpl  = [](IdT id) { return 0; }};

        SyntaxTree[IdT, ValT] SrcTree{Options, Src};
        SyntaxTree[IdT, ValT] DstTree{Options, Dst};
        ASTDiff[IdT, ValT]    Diff{SrcTree, DstTree, Options};

        for (NodeId Dst : DstTree) {
            NodeId Src = Diff.getMapped(DstTree, Dst);
            if (Src.isValid()) {
                std::cout << "Match ";
                printNode(std::cout, SrcTree, Src);
                std::cout << " to ";
                printNode(std::cout, DstTree, Dst);
                std::cout << "\n";
            }

            printDstChange(std::cout, Diff, SrcTree, DstTree, Dst);
        }
    }


    std::cout << "diff done\n";

    return 0;
}
