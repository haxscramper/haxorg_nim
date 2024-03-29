## This module implements the tree diff algorithm, adopted from the papers
##
## "Fine-grained and Accurate Source Code Differencing" by Jean-Rémy
## Falleri, Floréal Morandat, Xavier Blanc, Matias Martinez, Martin
## Monperrus -- for implementation of the top-down, bottom up matching
## strategy.
##
## “Simple Fast Algorithms for the Editing Distance between Trees and
## Related Problems” by Zhang, Kaizhong and Dennis Shasha -- for
## implementation of the optimal tree mapping solution that is used as an
## `opt()` function for top-down-bottom-up matching.
##
## "Meaningful Change Detection in Structured Data" by Sudarshan S.
## Chawathe Hector Garcia-Molina -- for implementation of the optimal tree
## edit script configuration.
##
## The implementation is partially based on the clang-diff tool but is
## adopted to the different programming language and provides a much wider
## capabilities when it comes to generic programming -- API is not limited
## to the C++/clang Syntax trees.
##
## Main algorithm can be split into several steps
##
## 1. Preparation -- convert input AST into a `SyntaxTree` objects (source
##    and destination) that will be used for subsequent diff computations.
##
##    Done in `initSyntaxTree()`
##
## 2. Iterate the source and destination trees in parallel from top to
##    bottom, breadth-first, searching for any subtrees that are
##    isomorphic to each other. Node equality is checked using user-provided
##    predicates. This step produces a first mapping that is refined later.
##
##    Done in `matchTopDown()`, configured by `minHeight`
##
## 3. Iterate the source tree in postorder, searching for any possible
##    missing mappings. If unmapped source node is found, check whether it
##    has a sufficient number of subnodes that are already mapped and then
##    try to find a sutable candidate.
##
##    Done in `matchBottomUp()`, configured by `minSimilarity`.
##
## 4. Iterate over resulting mapping assigning change types `(src, dst)`
##    to node pairs.

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
  MappingGeneration* = enum
    MapNone ## No mapping metadata
    MapTopDown ## Link was created during top-down mapping stage.
    MapBottomUpCandidate ## Link was created during bottom-up stage using
    ## best matching candidate.
    MapBottomUpOptimal ## Link was created during bottom-up stage using
                       ## optimal mapping algorithm.

  MappingDebug* = object
    bottomUpRun*: int
    triedCandidates*: seq[tuple[tried: NodeId, similarity: float]]
    kind*: MappingGeneration

  Mapping = object
    bottomUpRun: int
    srcToDst: seq[NodeId]
    dstToSrc: seq[NodeId]
    srcMeta: seq[MappingDebug]
    dstMeta: seq[MappingDebug]

func initMapping(size: int): Mapping =
  Mapping(
    srcToDst: newSeqWith[NodeId](size, initNodeId()),
    dstToSrc: newSeqWith[NodeId](size, initNodeId()),
    srcMeta: newSeqWith[NodeId](size, MappingDebug()),
    dstMeta: newSeqWith[NodeId](size, MappingDebug())
  )

func link(this: var Mapping, src, dst: NodeId, dbg: MappingGeneration) =
  this.srcToDst[src.int] = dst
  this.dstToSrc[dst.int] = src

  this.srcMeta[src.int].kind = dbg
  this.dstMeta[src.int].kind = dbg

  this.srcMeta[src.int].bottomUpRun = this.bottomUpRun
  this.dstMeta[src.int].bottomUpRun = this.bottomUpRun

func getSrcMeta(this: var Mapping, src: NodeId): var MappingDebug =
  this.srcMeta[src.int]

func getDstMeta(this: var Mapping, dst: NodeId): var MappingDebug =
  this.dstMeta[dst.int]

func getSrcMeta(this: Mapping, src: NodeId): MappingDebug =
  this.srcMeta[src.int]

func getDstMeta(this: Mapping, dst: NodeId): MappingDebug =
  this.dstMeta[dst.int]

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
    ##
    ## This option is read in the bottom-up iteration of the tree matching
    ## and controls how loosely the trees need to map to each other.

    maxSize*: int ## Whenever two subtrees are matched in the bottom-up
    ## phase, the optimal mapping is computed, unless the size of either
    ## subtrees exceeds this.

    stopAfterTopDown*: bool ## Stop tree mapping computation after top-down
    ## traversal.

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
    tree: SyntaxTree[IdT, ValT]
    id: NodeId

  PriorityList[IdT, ValT] = object
    ## Priority queue for nodes, sorted descendingly by their height.
    tree: SyntaxTree[IdT, ValT]
    container: seq[NodeId]
    list: HeapQueue[HeightLess[IdT, ValT]]

func initHeightLess[IdT, ValT](
    tree: SyntaxTree[IdT, ValT], id: NodeId): HeightLess[IdT, ValT] =
  HeightLess[IdT, ValT](tree: tree, id: id)

func `<`*[IdT, ValT](h1, h2: HeightLess[IdT, ValT]): bool =
  return h1.tree.getNode(h1.id).height < h2.tree.getNode(h2.id).height

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
  let max = peekMax(this)
  if max == 0:
    return

  while peekMax(this) == max:
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
  ## Returns a mapping of identical subtrees. This procedure implements a
  ## first iteration of the tree mapping detection and is designed to
  ## detect a bigger structural similarities in the trees. It's operation
  ## is affected by the `minHeight` field in the configuration options --
  ## trees below this height won't be considered for possible mapping.
  var
    srcList = initPriorityList(this.src)
    dstList = initPriorityList(this.dst)

  result = initMapping(this.src.getSize() + this.dst.getSize())
  srcList.push(this.src.getRootId())
  dstList.push(this.dst.getRootId())

  var
    maxSrc = 0
    maxDst = 0

  # Until there is at least one toplevel subtree with sufficient height in
  # each list.
  while this.opts.minHeight <
        min(
          (maxSrc = srcList.peekMax() ; maxSrc),
          (maxDst = dstList.peekMax() ; maxDst)):

    # if two top subtrees don't have equal height
    if maxSrc > maxDst:
      # insert all nodes from tallest subforest -- either source one
      for id in srcList.pop():
        srcList.open(id)

    elif maxDst > maxSrc:
      # or destination one.
      for id in dstList.pop():
        dstList.open(id)

    else:
      # otherwise get two subforest of equal height
      let
        topSrc = srcList.pop()
        topDst = dstList.pop()

      # for each combination of subtrees is these forests
      for idSrc in topSrc:
        for idDst in topDst:
          # if pair of trees is isomorphic
          if identical(this, idSrc, idDst) and
             not result.hasSrc(idSrc) and
             not result.hasDst(idDst):

            for i in 0 ..< this.src.getNumberOfDescendants(idSrc):
              result.link(
                idSrc + i,
                idDst + i,
                MapTopDown
              )

      # Determine if there is any isomorphic mapping between either (1)
      # roots two highest subforests or (2) root and subnodes of a root in
      # other tree.
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
    tree: SyntaxTree[IdT, ValT] ## The parent tree.
    rootIds: seq[NodeId] ## Maps SubNodeIds to original ids. Maps subtree
    ## nodes to their leftmost descendants wtihin the subtree.
    leftMostDescendants: seq[SubNodeId]
    keyRoots: seq[SubNodeId]

proc getSize[IdT, ValT](this: Subtree[IdT, ValT]): int =
  return this.rootIds.len()

proc getLeftMostDescendant[IdT, ValT](
    this: Subtree[IdT, ValT], Id: SubNodeId): SubNodeId =
  assert(0 < Id and Id <= this.getSize(), "Invalid subtree node index.")
  return this.leftMostDescendants[Id - 1]

proc computekeyRoots[IdT, ValT](this: var SubTree[IdT, ValT], leaves: int) =
  this.keyRoots.setLen(leaves)
  var
    visited: HashSet[int]
    k = leaves - 1
    i = initSubNodeId(this.getSize())

  while 0 < i:
    let leftDesc = this.getLeftMostDescendant(i)
    if leftDesc.int in visited:
      dec i
      continue

    assert(0 <= k, "k should be non-negative")
    this.keyRoots[k] = i
    visited.incl(leftDesc.int)
    dec k
    dec i



proc getIdInRoot[IdT, ValT](
    this: Subtree[IdT, ValT], id: SubNodeId): NodeId =
  assert(0 < id and id <= this.getSize(), "Invalid subtree node index.")
  return this.rootIds[id - 1]

proc getNode[IdT, ValT](
      this: Subtree[IdT, ValT], id: SubNodeId): Node[IdT, ValT] =
  return this.tree.getNode(this.getIdInRoot(id))


proc getPostorderOffset[IdT, ValT](this: Subtree[IdT, ValT]): NodeId =
  ## Returns the postorder index of the leftmost descendant in the
  ## subtree.
  return initNodeId(
    this.tree.postorderIds[this.getIdInRoot(SubNodeId(1))])

proc setleftMostDescendants[IdT, ValT](this: var Subtree[IdT, ValT]): int =
  ## Returns the number of leafs in the subtree.
  this.leftMostDescendants.setLen(this.getSize())
  for I in 0 ..< this.getSize():
    var SI = initSubNodeId(I + 1);
    let N = this.getNode(SI)
    result += (if N.isleaf(): 1 else: 0)
    assert(
      I == this.tree.postorderIds[this.getIdInRoot(SI)] -
        this.getPostorderOffset(),
      "Postorder traversal in subtree should correspond to " &
      "traversal in the root tree by a constant offset.")

    this.leftMostDescendants[I] = SubNodeId(
        this.tree.postorderIds[N.leftMostDescendant] -
        this.getPostorderOffset())



proc initSubtree[IdT, ValT](
    tree: SyntaxTree[IdT, ValT],
    subtreeRoot: NodeId
  ): Subtree[IdT, ValT] =

  result = Subtree[IdT, ValT](tree: tree)

  result.rootIds = getSubtreePostorder(tree, subtreeRoot)
  let numleaves = setleftMostDescendants(result)
  computekeyRoots(result, numleaves)

proc getValue[IdT, ValT](
    this: Subtree[IdT, ValT], id: SubNodeId): ValT =
  return this.tree.getValue(this.getIdInRoot(id))

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
  deletionCost  = 1.0f
  insertionCost = 1.0f
  updateCost    = 1.0f


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
      return updateCost


proc computeForestDist[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT],
    idSrc, idDst: SubNodeId
  ) =

  assert(0 < idSrc and 0 < idDst, "Expecting offsets greater than 0.")
  var
    leftMostDescSrc = this.src.getLeftMostDescendant(idSrc)
    leftMostDescDst = this.dst.getLeftMostDescendant(idDst)

  this.forestDist[leftMostDescSrc][leftMostDescDst] = 0

  var descSrc = leftMostDescSrc + 1
  while descSrc <= idSrc:
    this.forestDist[descSrc][leftMostDescDst] =
      this.forestDist[descSrc - 1][leftMostDescDst] + deletionCost

    var descDst = leftMostDescDst + 1

    while descDst <= idDst:
      this.forestDist[leftMostDescSrc][descDst] =
        this.forestDist[leftMostDescSrc][descDst - 1] + insertionCost

      let
        dleftMostDescSrc = this.src.getLeftMostDescendant(descSrc)
        dleftMostDescDst = this.dst.getLeftMostDescendant(descDst)

      if dleftMostDescSrc == leftMostDescSrc and
         dleftMostDescDst == leftMostDescDst:

        let updateCost  = this.getUpdateCost(descSrc, descDst)
        this.forestDist[descSrc][descDst] = min([
          this.forestDist[descSrc - 1][descDst] + deletionCost,
          this.forestDist[descSrc][descDst - 1] + insertionCost,
          this.forestDist[descSrc - 1][descDst - 1] + updateCost
        ])

        this.treeDist[descSrc][descDst] = this.forestDist[descSrc][descDst];

      else:
        this.forestDist[descSrc][descDst] = min([
          this.forestDist[descSrc - 1][descDst] + deletionCost,
          this.forestDist[descSrc][descDst - 1] + insertionCost,
          this.forestDist[dleftMostDescSrc][dleftMostDescDst] +
            this.treeDist[descSrc][descDst]
        ])

      inc descDst
    inc descSrc


proc computetreeDist[IdT, ValT](this: var ZhangShashaMatcher[IdT, ValT]) =
  for idSrc in this.src.keyRoots:
    for idDst in this.dst.keyRoots:
      computeforestDist(this, idSrc, idDst)



proc getMatchingNodes[IdT, ValT](
    this: var ZhangShashaMatcher[IdT, ValT]): seq[(NodeId, NodeId)] =

  var
    matches: seq[(NodeId, NodeId)]
    treePairs: seq[(SubNodeId, SubNodeId)]
    rootNodePair = true

  computeTreeDist(this)
  treePairs.add((
    SubNodeId(this.src.getSize()),
    SubNodeId(this.dst.getSize())))

  while 0 < treePairs.len():
    var (lastRow, lastCol) = treePairs.pop()
    if not rootNodePair:
      computeForestDist(this, lastRow, lastCol)

    rootNodePair = false
    var
      firstRow     = this.src.getLeftMostDescendant(lastRow)
      firstCol     = this.dst.getLeftMostDescendant(lastCol)
      row          = lastRow
      col          = lastCol

    while row > firstRow or col > firstCol:
      if row > firstRow and
         this.forestDist[row - 1][col] + 1 ==
         this.forestDist[row][col]:
        dec row

      elif col > firstCol and
           this.forestDist[row][col - 1] + 1 ==
           this.forestDist[row][col]:
        dec col

      else:
        let
          leftMostDescSrc = this.src.getLeftMostDescendant(row)
          leftMostDescDst = this.dst.getLeftMostDescendant(col)

        if leftMostDescSrc == this.src.getLeftMostDescendant(lastRow) and
           leftMostDescDst == this.dst.getLeftMostDescendant(lastCol):
          let
            idSrc = this.src.getIdInRoot(row)
            idDst = this.dst.getIdInRoot(col)
            n1 = this.diffImpl.src.getNode(idSrc)
            n2 = this.diffImpl.dst.getNode(idDst)

          assert(
            this.diffImpl.isMatchingPossible(idSrc, idDst),
            "These nodes must not be matched.")

          matches.add((idSrc, idDst))
          dec row
          dec col

        else:
          treePairs.add((row, col))
          row = leftMostDescSrc
          col = leftMostDescDst

  return matches





proc addOptimalMapping[IdT, ValT](
    this: var ASTDiff[IdT, ValT], # map: var Mapping,
    idDst, idSrc: NodeId) =
  ## Uses an optimal albeit slow algorithm to compute a mapping
  ## between two subtrees, but only if both have fewer nodes than
  ## maxSize.
  ##
  ## This procedure fills in any missing mapping (source has no
  ## destination, destination has no source) between two trees, without
  ## altering already existing arrangements.
  if this.opts.maxSize < max(
    this.src.getNumberOfDescendants(idSrc),
    this.dst.getNumberOfDescendants(idDst)):
      return

  var matcher = initZhangShashaMatcher(
    this, this.src, this.dst, idSrc, idDst)

  let r = matcher.getMatchingNodes()
  for (src, dst) in r:
    # WARNING: original code used 'hasSrc(src)' and 'hasDst(dst)', but I
    # believe this to be semantic naming error in the code.
    if not this.map.hasDst(src) and
       not this.map.hasSrc(dst):
      this.map.link(src, dst, MapBottomUpOptimal)

proc getJaccardSimilarity[IdT, ValT](
    this: ASTDiff[IdT, ValT],
    idSrc, idDst: NodeId
  ): float =

  ## Computes the ratio of common descendants between the two nodes.
  ## Descendants are only considered to be equal when they are mapped in
  ## M.
  var commonDescendants = 0
  let nodeSrc = this.src.getNode(idSrc)
  # Count the common descendants, excluding the subtree root.
  var src = idSrc + 1
  while src <= nodeSrc.rightMostDescendant:
    let dst = this.map.getDst(src)
    # if idSrc.int == 14:
    #   echov "mapping", src, dst

    commonDescendants += int(
        dst.isValid() and this.dst.isInSubtree(dst, idDst))

    inc src
  # We need to subtract 1 to get the number of descendants excluding the
  # root.
  let denominator = this.src.getNumberOfDescendants(idSrc) - 1 +
                    this.dst.getNumberOfDescendants(idDst) - 1 -
                    commonDescendants

  # commonDescendants is less than the size of one subtree.
  assert(0 <= denominator, "Expected non-negative denominator.")
  if denominator == 0:
    return 0

  else:
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
    this: var ASTDiff[IdT, ValT], idSrc: NodeId): NodeId =
  ## Returns the node that has the highest degree of similarity.
  result = initNodeId()

  var highestSimilarity = 0.0
  for idDst in this.dst:
    if not this.isMatchingPossible(idSrc, idDst):
      continue

    if this.map.hasSrc(idDst):
      continue

    let similarity = this.getJaccardSimilarity(idSrc, idDst)
    this.map.getSrcMeta(idSrc).triedCandidates.add((idDst, similarity))
    if this.opts.minSimilarity <= similarity and
       highestSimilarity < similarity:
        highestSimilarity = similarity
        result = idDst


proc matchBottomUp[IdT, ValT](
    this: var ASTDiff[IdT, ValT],
    # map: var Mapping,
    doOptimalPass: bool = true
  ) =
  ## Execute bottom-up matching for the trees. This function expects
  ## already partially formed mapping construction and can be called
  ## multiple times, although `doOptimalPass` should ideally be invoked
  ## only once. The exact configuration depends on your specific needs in
  ## tree matching and does not have a single best configuration.
  inc this.map.bottomUpRun
  let postorder = getSubtreePostorder(this.src, this.src.getRootId())
  # for all nodes in left, if node itself is not matched, but
  # has any children matched
  for src in postorder:
    # Iterating in postorder, so missing root mapping will cause more
    # expensive algorithm to kick in only if there is a top-level mismatch
    if src == this.src.getRootId() and
       not this.map.hasSrc(this.src.getRootId()) and
       not this.map.hasDst(this.dst.getRootId()):

      if isMatchingPossible(
        this, this.src.getRootId(), this.dst.getRootId()):

        this.map.link(
          this.src.getRootId(),
          this.dst.getRootId(),
          MapBottomUpCandidate
        )

        if doOptimalPass:
          addOptimalMapping(
            this, this.src.getRootId(), this.dst.getRootId())

      break

    # Check if node is properly mapped (in that case it will be skipped
    # because it is already OK) or it has no matched subnodes (in that case
    # it will be skipped because it is unlikely to have a proper candidate
    # anyway)
    if this.map.hasDst(src):
      # WARNING original code has `hasSrc`, but I believe this to be a
      # semantic error in code as here we need to check if source node was
      # mapped to some destination target and `hasDst` checks it via
      # `srcToDst -> isValid()` sequence.
      #
      # In paper this part is described as "t1 is not mapped"
      continue

    let
      nodeSrc = this.src.getNode(src)
      matchedSubnodes = anyIt(nodeSrc.subnodes, this.map.hasDst(it))

    # if it is a valid candidate and matches criteria for minimum number of
    # shares subnodes -- no candidate would be found anyway, skipping.
    if not matchedSubnodes:
      continue

    let dst = this.findCandidate(src)
    if dst.isValid():
      # add node to mapping if max of number of subnodes does not exceed
      # threshold ratio.
      this.map.link(src, dst, MapBottomUpCandidate)
      if doOptimalPass:
        addOptimalMapping(this, src, dst)

proc computeMapping[IdT, ValT](this: var ASTDiff[IdT, ValT]) =
  ## Matches nodes one-by-one based on their similarity. This is the main
  ## entry point of the similarity mapping computation. As described in the
  ## module toplevel comment the mapping happens in two stages: first, a
  ## top-down iteration tries to find matching nodes and then, a first
  ## bottom-up iteration fills any missing parts.
  ##
  ## This algorithm mostly constructs the mapping in the ASTDiff object.
  ## The specific solution for top-down&bottom-up strategy is considered to
  ## produce the best results, but there can be other implementation
  ## plugged in place of this particular "compute mapping" solution.
  this.map = matchTopDown(this)
  if (this.opts.stopAfterTopDown):
    return

  # Iterate tree in the bottom-up manner, invoking the optimal tree mapping
  # when suitable candidates are found.
  matchBottomUp(this)

  # Do another bottom-up pass, this time connecting only nodes that have a
  # sufficient number of mapped subnodes.
  #
  # NOTE: this function call was not implemented in the original algorithm,
  # but without it tree mapping produces a large number of
  # questionably-placed moves on small trees with default `minHeight`
  # mapping (and reducing `minHeight` mapping significantly decreases
  # quality of the leaf matching).
  matchBottomUp(this, doOptimalPass = true)

proc computeChangeKinds[IdT, ValT](this: ASTDiff[IdT, ValT])


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
  computeChangeKinds(result)


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
    this: ASTDiff[IdT, ValT], src, dst: NodeId): bool =

  ## Returns true if the nodes' parents are matched.
  let
    srcParent = getParent(this.src, src)
    dstParent = getParent(this.dst, dst)

  return (srcParent.isInvalid() and
          dstParent.isInvalid()) or
         (srcParent.isValid() and
          dstParent.isValid() and
          this.map.getDst(srcParent) == dstParent)

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

proc computeChangeKinds[IdT, ValT](this: ASTDiff[IdT, ValT]) =
  ## Compute change in nodes based on the input mapping. Update node
  ## `.change` and `.shift` fields.

  # If the node has no destination mapping it is considered as "deleted"
  for src in this.src:
    if not this.map.hasSrc(src):
      this.src.getNode(src).change = ChDelete
      this.src.getNode(src).shift -= 1

  # If node has no source mapping it is considered as "added"
  for dst in this.dst:
    if not this.map.hasDst(dst):
      this.dst.getNode(dst).change = ChInsert
      this.dst.getNode(dst).shift -= 1

  const absoluteMove = false
  for src in this.src.nodesBfs:
    let dst = this.map.getDst(src)
    if dst.isInvalid():
      continue

    if not this.haveSameParents(src, dst) or
       this.src.findPositionInParent(src, absoluteMove) !=
       this.dst.findPositionInParent(dst, absoluteMove):

      this.src.getNode(src).shift -= 1
      this.dst.getNode(dst).shift -= 1

  # Iterate over all nodes in the destination tree
  for dst in this.dst.nodesBfs:
    let src = this.map.getSrc(dst)
    if src.isInvalid():
      # Already mapped as "inserted"
      continue

    var
      nodeSrc = this.src.getNode(src)
      nodeDst = this.dst.getNode(dst)

    # Node either moved between completely different parents or it was
    # moved inside of the same node. In both cases it is a 'move'
    # operation. IDEA Maybe it should be split into "internal move" and
    # "external move".
    if not haveSameParents(this, src, dst) or
       this.src.findPositionInParent(src, absoluteMove) !=
       this.dst.findPositionInParent(dst, absoluteMove):

      nodeSrc.change = ChMove
      nodeDst.change = ChMove

    # Node can be simply "updated" and "moved and updated", so it is
    # necessary to consider possible variants of the "move" operation.
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
    ## Result of the tree difference computation
    src*: SyntaxTree[IdT, ValT]
    dst*: SyntaxTree[IdT, ValT]
    changes*: ASTDiff[IdT, ValT]

type
  NodeChange* = object
    ## Description of changes between two nodes
    src*: NodeId ## Source node. Can be invalid of new node was inserted.
    dst*: NodeId ## Destination node. Can be invalid of old node was
                 ## deleted.
    case kind: ChangeKind
      of ChInsert:
        parent*: NodeId ## Node parent to insert information in
        position*: int ## Position in the parent to place the node in

      of ChMove, ChUpdateMove:
        moveFrom*, moveTo*: tuple[parent: NodeId, position: int] ##  Movement
        ## information -- original parent and position it and new parent
        ## and position in it.

      else:
        discard

proc getNodeChange[IdT, ValT](
    diff: DiffResult[IdT, ValT],
    src, dst: NodeId,
    fromDst: bool = false
  ): NodeChange =
  ## Create node change information object using already computed *kind* of
  ## the node change.

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
  ## Get node change from the destination node
  let src = diff.changes.getMapped(diff.dst, dst)
  getNodeChange(diff, src, dst, fromDst = true)

proc changeFromSrc[IdT, ValT](
    diff: DiffResult[IdT, ValT], src: NodeId): NodeChange =
  ## Get node change from the source node ID
  let dst = diff.changes.getMapped(diff.src, src)
  getNodeChange(diff, src, dst, fromDst = false)

iterator items*[IdT, ValT](diff: DiffResult[IdT, ValT]): NodeChange =
  ## Iterate over all node changs in the diff result
  for dst in diff.dst:
    yield changeFromDst(diff, dst)

  for src in diff.dst:
    let change = changeFromSrc(diff, src)
    if change.kind == ChDelete:
      yield change


proc srcValue[IdT, ValT](diff: DiffResult[IdT, ValT], node: NodeId): ValT =
  ## Get source node value
  diff.src.getValue(node)

proc dstValue[IdT, ValT](diff: DiffResult[IdT, ValT], node: NodeId): ValT =
  ## Get destination node value
  diff.dst.getValue(node)

proc srcValue[IdT, ValT](
    diff: DiffResult[IdT, ValT], change: NodeChange): ValT =
  ## Get source node value for a node change
  diff.src.getValue(change.src)

proc dstValue[IdT, ValT](
    diff: DiffResult[IdT, ValT], change: NodeChange): ValT =
  ## Get destination node value for node change
  diff.dst.getValue(change.dst)


proc `$`*[IdT, ValT](
    diff: DiffResult[IdT, ValT], change: NodeChange): string =
  ## Format node change using diff result information
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
  ## Diff two ref & kind nodes -- most commonly used tree structure with
  ## `.kind` field and `ref object` structured in a tree using `seq[]` of
  ## subnodes. For different tree structure types create a `TreeMirror`
  ## object manually and diff it instead.

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
    selfId*: NodeId ## Original node ID
    targetId*: Option[NodeId] ## Target node ID information
    change*: NodeChange ## Type of the node change
    next*: Option[NodeId] ## WIP ID of the next sibling node
    indexInParent*: int ## Original index in the parent tree

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
    maxMappingHeight*: tuple[
      direct, moved, updated, movedUpdated: int] ## Only show directly
    ## matched links between nodes that are no higher than this. Height is
    ## taken as a minimum of source and destination nodes.
    formatKind*: proc(kind: int): string ## Format node kind into a simple
    ## text string that will be used in a tree representation.
    formatValue*: proc(value: ValT): string ## Format node value into a simple
    ## text string that will be used in a tree representation. If resulting
    ## string is non-empty it should start with the `\l` newline if node
    ## formatting is multiline (or leading space for better one-line
    ## formatting).
    formatLink*: proc(node: ValT, subnode: int): Option[string] ## Format
    ## link between node and it's subnode.
    mappingComputeDebug*: bool ## Whether to add debug mapping computation
    ## information in node representation.
    srcLabel*: string
    dstLabel*: string
    graphFont*: string

func kind*(node: GraphvizExplainNode): ChangeKind =
  ## Get kind of the graphviz diff node change
  node.change.kind

proc initGraphvizFormat*[ValT](): GraphvizFormatConf[ValT] =
  ## Init default graphviz formatting configuration.
  return GraphvizFormatConf[ValT](
    horizontalDir: true,
    maxMappingHeight: (
      direct: 1,
      moved: 10,
      updated: 2,
      movedUpdated: 2
    ),
    graphFont: "Iosevka",
    srcLabel: "Src tree",
    dstLabel: "Dst tree"
  )

proc formatGraphvizDiff*[IdT, ValT](
    diff: DiffResult[IdT, ValT],
    dot: GraphvizExplain,
    conf: GraphvizFormatConf[ValT],
  ): string =
  ## Convert formatting information from the graphviz diff explanation into
  ## a string that can be rendered via `dot`.

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

          relevantMove = true # and dstChain != srcChain

          # REVIEW This code is used to color small-sized tree diffs
          # artefacts and probably needs to be configured better in the
          # future, but right now I don't have any exact suggestions to
          # implement this better. This needs more external feedback wrt.
          # to appearance.
          if relevantMove:
            ("color=yellow; style=filled;", "")

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
          ("color=yellow; style=filled",
           "@$#" % [
             if src:
               $entry.change.moveFrom.parent
             else:
               $entry.change.moveTo.parent
           ]
          )

      of ChUpdateMove: ("color=orange; style=filled", "")
      of ChUpdate: ("color=pink; style=filled", "")

  var
    srcLayerLink = ""
    srcLeafNodes: seq[string]

  proc formatMeta(dbg: MappingDebug): string =
    if not conf.mappingComputeDebug:
      return ""

    case dbg.kind:
      of MapTopDown:
        result = "top"
      of MapBottomUpCandidate:
        result = &"bot/cand({dbg.bottomUpRun})"

      of MapBottomUpOptimal:
        result = &"bot/opt({dbg.bottomUpRun})"
      else:
        discard

    for idx, cand in dbg.triedCandidates:
      if 0 < idx:
        result.add(",")
      result.add(" ")
      result.add(&"«{cand.tried}:{cand.similarity:.2}»")

    if 0 < len(result):
      result.add(" ")

  for entry in dot.src:
    let n = diff.src.getNode(entry.selfId)
    let (format, text) = explainChange(entry, true)

    subSrc.add "    s$#[label=\"$#$1[$#] $# $#$#\", $#];\n" % [
      $entry.selfId,
      formatMeta(diff.changes.map.getSrcMeta(entry.selfId)),
      $entry.indexInParent,
      conf.formatKind(getNodeKind(n, diff.changes.opts).int),
      text,
      conf.formatValue(diff.src.getValue(entry.selfId)),
      format
    ]

    if n.height == 1:
      srcLeafNodes.add("s$#" % [$entry.selfId])

  proc explainSubnode(node: ValT, idx: int): string =
    if isNil(conf.formatLink):
      result = ""

    else:
      let format = conf.formatLink(node, idx)
      if format.isSome():
        result = format.get()

      else:
        result = ""

  for entry in dot.src:
    var idx = 0
    for subnode in subnodes(diff.changes.src, entry.selfId):
      subsrc.add "    // subnode src\n"
      subsrc.add "    s$# -> s$#[label=\"$#\"];\n" % [
        $entry.selfId,
        $subnode,
        explainSubnode(diff.src.getValue(entry.selfId), idx)
      ]

      inc idx

  var
    dstLayerLink = ""
    dstLeafNodes: seq[string]

  for entry in dot.dst:
    let n = diff.dst.getNode(entry.selfId)
    let (format, text) = explainChange(entry, false)
    subDst.add "    t$#[label=\"$#$1[$#] $# $#$#\", $#];\n" % [
      $entry.selfId,
      formatMeta(diff.changes.map.getDstMeta(entry.selfId)),
      $entry.indexInParent,
      conf.formatKind(getNodeKind(n, diff.changes.opts).int),
      text,
      conf.formatValue(diff.dst.getValue(entry.selfId)),
      format
    ]

    if n.height == 1:
      dstLeafNodes.add("t$#" % [$entry.selfId])

  for entry in dot.dst:
    var idx = 0
    for subnode in subnodes(diff.changes.dst, entry.selfId):
      subDst.add "    // subnode dst\n"
      if conf.horizontalDir:
        subDst.add "    t$# -> t$#[dir=back, label=\"$#\"];\n" % [
          $subnode,
          $entry.selfId,
          explainSubnode(diff.dst.getValue(entry.selfId), idx)
        ]

      else:
        subDst.add "    t$# -> t$#[label=\"$#\"];\n" % [
          $entry.selfId,
          $subnode,
          explainSubnode(diff.dst.getValue(entry.selfId), idx)
        ]

      inc idx

  var mapping: string
  for (src, dst) in items(dot.linked):
    let
      nsrc = diff.changes.src.getNode(src)
      ndst = diff.changes.dst.getNode(dst)
      hmin = min(nsrc.height, ndst.height)

    let ok =
      case nsrc.change:
        of ChNone: hmin <= conf.maxMappingHeight.direct
        of ChMove: hmin <= conf.maxMappingHeight.moved
        of ChUpdate: hmin <= conf.maxMappingHeight.updated
        of ChUpdateMove: hmin <= conf.maxMappingHeight.movedUpdated
        else: false

    if ok:
      mapping &= "  // source destination link mapping\n"
      mapping &= "  s$# -> t$#[style=dashed, dir=none];\n" % [ $src, $dst ]

  result = """
digraph G {
  node[shape=rect, fontname=$graphFont];
  edge[fontname=$graphFont];
  splines=polyline;
  rankdir=$direction;
  spline=polyline;
  subgraph cluster_0 {
label = "$srcLabel";
// Subnode source links
$subSrc
// Source layer link
$srcLayerLink
// Source leaf nodes layer
{rank=same;$srcLeafNodes[style=invis];}  }

  subgraph cluster_1 {
label = "$dstLabel";
// Subnode destination links
$subDst
// Destination layer link
$dstLayerLink
// Destination leaf nodes layer
{rank=same;$dstLeafNodes[style=invis];}  }
// Mapping information
$mapping}
""" % {
    "graphFont": conf.graphFont,
    "direction": if conf.horizontalDir: "LR" else: "TB",
    "srcLabel": conf.srcLabel,
    "dstLabel": conf.dstLabel,
    "subSrc": subSrc,
    "srcLayerLink": srcLayerLink,
    "srcLeafNodes": srcLeafNodes.join(" -> "),
    "subDst": subDst,
    "dstLayerLink": dstLayerLink,
    "dstLeafNodes": dstLeafNodes.join(" -> "),
    "mapping": mapping
  }

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
