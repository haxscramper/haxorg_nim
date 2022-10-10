import ast_diff
import nimsuggest/sexp

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

startHax()

proc printDstChange[IdT, ValT](
    diff: ASTDiff[IdT, ValT],
    srcTree: SyntaxTree[IdT, ValT],
    dstTree: SyntaxTree[IdT, ValT],
    dst: NodeId
  ): string =
    let dstNode = dstTree.getNode(dst)
    let src     = diff.getMapped(dstTree, dst)
    case dstNode.change:
      of ChNone:
        result = "None"

      of ChDelete:
        assert(false, "The destination tree can't have deletions.")

      of ChUpdate:
        result &= "Update "
        result &= $srcTree.getValue(src);
        result &= " to " & $dstTree.getValue(dst)

      of ChMove, ChUpdateMove, ChInsert:
        case dstNode.change:
          of ChInsert: result &= "Insert"
          of ChMove: result &= "Move"
          of ChUpdateMove: result &= "Update and Move"
          else: discard

        result &= " "
        result &= $dstTree.getValue(dst)
        result &= " into "
        result &= $dstTree.getValue(dstNode.parent)
        result &= " at " & $dstTree.findPositionInParent(dst)

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

func `$`(v: NodeValue): string =
  case v.kind:
    of NInt: &"NInt({v.intVal})"
    of NFloat: &"NFloat({v.floatVal})"
    of NString: &"NString({v.strVal})"

func `==`(v1, v2: NodeValue): bool =
  v1.kind == v2.kind and (
    case v1.kind:
      of NInt: v1.intVal == v2.intVal
      of NFloat: v1.floatVal == v2.floatVal
      of Nstring: v1.strVal == v2.strVal
  )

type
  RealNode = ref object
    value: string
    kind: int
    sub: seq[RealNode]

block:
  type
    IdT  = RealNode
    ValT = string

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

  var srcMirror = mirror[IdT, ValT](
    src,
    mirror[IdT, ValT](src.sub[0]),
    mirror[IdT, ValT](src.sub[1]))

  var dstMirror = mirror[IdT, ValT](
    dst,
    mirror[IdT, ValT](dst.sub[0]),
    mirror[IdT, ValT](dst.sub[1]),
    mirror[IdT, ValT](dst.sub[2]))

  let opts = initCmpOpts[IdT, ValT](
    proc(id: IdT): ValT = id.value,
    proc(id: IdT): int = id.kind,
    proc(v1, v2: ValT): bool = v1 == v2
  )

  var
    srcTree = initSyntaxTree(opts, srcMirror, getMirrorId[IdT, ValT])
    dstTree = initSyntaxTree(opts, dstMirror, getMirrorId[IdT, ValT])
    diff = initASTDiff(srcTree, dstTree, opts)

  for dst in dstTree:
    let src = diff.getMapped(dstTree, dst)
    if (src.isValid()):
      let src = srcTree $ src
      let dst = dstTree $ dst
      stdout.write("Match ", src, " to ", dst, " -- ")

    echo printdstChange(diff, srcTree, dstTree, dst)

type
  RealNode2 = ref object
    case kind: NodeKind
      of NInt:
        intVal: int

      of NFloat:
        floatVal: float

      of NString:
        strVal: string

    sub: seq[RealNode2]

func `$`(n: RealNode2): string =
  "$#($#)" % [
    $n.kind,
    case n.kind:
      of NInt: $n.intVal
      of NFloat: $n.floatVal
      of NString: $n.strVal
  ]

proc len(n: RealNode2): int = n.sub.len()
iterator items(n: RealNode2): RealNode2 =
  for i in n.sub:
    yield i

block:
  proc ast(val: int | float | string, sub: varargs[RealNode2]): RealNode2 =
    when val is int:
      result = RealNode2(kind: NInt, intVal: val)

    elif val is float:
      result = RealNode2(kind: NFloat, floatVal: val)

    else:
      result = RealNode2(kind: NString, strVal: val)

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

  proc toValue(node: RealNode2): NodeValue =
    result = NodeValue(kind: node.kind)
    case node.kind:
      of NInt: result.intVal = node.intVal
      of NFloat: result.floatVal = node.floatVal
      of NString: result.strVal = node.strVal

  block:
    let diff = diffRefKind(
      src,
      dst,
      proc(v1, v2: RealNode2): bool =
        v1.kind == v2.kind and (
          case v1.kind:
            of NInt: v1.intVal == v2.intVal
            of NFloat: v1.floatVal == v2.floatVal
            of NString: v1.strVal == v2.strVal
        )
    )

    echov "diffRefKind RealNode2"
    for it in items(diff):
      echo diff $ it

  block:
    type
      IdT  = RealNode2
      ValT = NodeValue

    let src = mirror[IdT, ValT](
      src,
      mirror[IdT, ValT](src.sub[0]),
      mirror[IdT, ValT](src.sub[1]))

    let dst = mirror[IdT, ValT](
      dst,
      mirror[IdT, ValT](dst.sub[0]),
      mirror[IdT, ValT](dst.sub[1]),
      mirror[IdT, ValT](dst.sub[2]))



    let opts = initCmpOpts[IdT, ValT](
      proc(id: IdT): ValT = toValue(id),
      proc(id: IdT): int = int(id.kind),
      proc(v1, v2: ValT): bool = v1 == v2
    )

    var
      srcTree = initSyntaxTree(opts, src, getMirrorId[IdT, ValT])
      dstTree = initSyntaxTree(opts, dst, getMirrorId[IdT, ValT])
      diff = initASTDiff(srcTree, dstTree, opts)

    echov "Manual diff RealNode2"
    for dst in dstTree:
      let src = diff.getMapped(dstTree, dst)
      if (src.isValid()):
        let src = srcTree $ src
        let dst = dstTree $ dst
        stdout.write("Match ", src, " to ", dst, " -- ")

      echo printDstChange(diff, srcTree, dstTree, dst)


proc topEq(n1, n2: SexpNode): bool =
  result = n1.kind == n2.kind and (
    case n1.kind:
      of SInt: n1.getNum() == n2.getNum()
      of SList: true
      else:
        assert(false)
        true
  )

for (src, dst) in [
  (sexp(1), sexp(2)),
  (convertSexp([1, 2, 3]), convertSexp([1, 2, 4])),
  (convertSexp([1, [3]]), convertSexp([1, [3, 5]])),
  (convertSexp([1, [3, 5]]), convertSexp([1, [3]])),
  (convertSexp([1, [3, 5], [8, 9]]), convertSexp([1, [3], [8, 9, 10]])),
]:
  echov "diff", src, dst
  let diff = diffRefKind(src, dst, topEq)

  for source in [false, true]:
    echov "From source", source
    echo explainDiff(
      diff,
      valueChange = proc(src, dst: SexpNode): ColoredText =
        clfmt("value '{src:,fg-red}' to '{dst:,fg-green}'"),
      value = proc(v: SexpNode): ColoredText =
        clfmt("{v:,fg-yellow}"),
      fromDst = source
    )

echo "main ok"
