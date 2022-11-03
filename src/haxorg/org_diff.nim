import haxorg/[types, ast_diff, parser]
import std/[sequtils, strutils]
export ast_diff
import sexp
export sexp
import hmisc/core/all

proc diff*(
    src, dst: OrgNode,
    minHeight: int = 2
  ): DiffResult[OrgNode, OrgNode] =

  proc eqCmp(n1, n2: OrgNode): bool =
    if n1.kind in orgTokenKinds and
       n2.kind in orgTokenKinds:
      return n1.strVal() == n2.strVal()

    else:
      return true

  var opts = initCmpOpts[OrgNode, OrgNode]()
  opts.minHeight = minHeight

  result = diffRefKind[OrgNode](src, dst, eqCmp, opts)

proc explainDiff*(
    diff: DiffResult[OrgNode, OrgNode], fromDst: bool = false): ColoredText =
  explainDiff(
    diff,
    value = proc(v: OrgNode): ColoredText = v.strVal() + fgYellow,
    valueChange = proc(v1, v2: OrgNode): ColoredText =
      clfmt("{v1.strVal():,fg-red} to {v2.strVal():,fg-green}"),
    fromDst = fromDst
  )

func tok(k: OrgTokenKind, v: string = ""): OrgToken =
  initFakeTok(k, v)

func ast(k: OrgNodeKind, sub: seq[OrgNode] = @[]): OrgNode =
  newTree(k, sub)

func ast(k: OrgNodeKind, sub: openarray[(string, OrgNode)]): OrgNode =
  newTree(k, sub)

func ast(k: OrgNodeKind, tk: OrgTokenKind, tok: string): OrgNode =
  newTree(k, tok(tk, tok))

func ast(k: OrgNodeKind, tok: string): OrgNode =
  newTree(k, tok(otNone, tok))

func toOrgCompact*(sexp: SexpNode): string =
  func isToken(sexp: SexpNode): bool =
    return (sexp.len() == 2 and sexp[1].kind == SString) or (sexp.len() == 1)

  func aux(sexp: SexpNode, res: var string, indent: int = 0) =
    res.add(repeat(" ", indent))
    case sexp.kind:
      of SList:
        res.add("(")
        if isToken(sexp):
          for idx in 0 ..< len(sexp):
            if 0 < idx:
              res.add(" ")

            res.add($sexp[idx])

        else:
          res.add(sexp[0].getSymbol())
          for idx in 1 ..< len(sexp):
            res.add("\n")
            aux(sexp[idx], res, indent + 1)

        res.add(")")

      of SKeyword:
        res.add(":")
        res.add(sexp.key)
        if sexp.value.isToken():
          res.add(" ")
          res.add($sexp.value)

        else:
          res.add(" (")
          res.add(sexp.value[0].getSymbol())
          for idx in 1 ..< len(sexp.value):
            res.add("\n")
            aux(sexp.value[idx], res, indent + 3 + sexp.key.len())

          res.add(")")


      else:
        raise newUnexpectedKindError(sexp)

  aux(sexp, result)

  
proc toSexp*(node: OrgNode): SexpNode =
  ## Convert org-mode mode to the S-expression
  result = newSList()
  result.add newSSymbol($node.kind)
  if node of orgTokenKinds:
    result.add newSString(node.strVal())

  else:
    for idx, sub in node:
      let name = orgSubnodeFieldName(node, idx)
      if name.isSome():
        result.add newSKeyword(name.get(), toSexp(sub))

      else:
        result.add toSexp(sub)

proc toOrg(node: SexpNode): OrgNode =
  ## Convert org-mode node from the S-expression
  assert(
    node.kind == SList,
    "Expected list for org-mode but found " & $node.kind,
  )

  assert(
    0 < node.len() and node[0].kind == SSymbol,
    "Expected list with leading symbol element"
  )

  let kind = parseEnum[OrgNodeKind]("org" & node[0].getSymbol())
  if node.len() == 2 and node[1].kind == SString:
    result = ast(kind, node[1].getStr())

  else:
    result = ast(kind)
    for sub in node.elems[1..^1]:
      if sub of SKeyword:
        result[sub.key] = sub.value.toOrg()

      else:
        result.add(sub.toOrg())


type
  TestFile* = object
    name*: string
    filename*: string
    expected*: OrgNode
    givenRaw*: string
    parsed*: OrgNode

proc parseTestFile*(text: string): TestFile =
  let split = text.split("\n")
  assert(split[0].startsWith("==="))
  assert(split[2].startsWith("==="))
  result.name = split[1]
  var given = 3..3
  while (given.b + 1) < split.len() and
        not split[given.b + 1].startsWith("==="):
    inc given.b

  if given.b + 1 == split.len():
    result.givenRaw = split[given].join("\n")

  else:
    result.givenRaw = split[given].join("\n")
    var expected = (given.b + 2) .. split.high()
    let content = split[expected].join("\n").strip(
      leading = false, chars = {'\n'})

    result.expected = parseSexp(content).toOrg()



when isMainModule:
  proc p(str: string): OrgNode = parseSexp(str).toOrg()

  echo treeRepr(p("(Ident)"))
