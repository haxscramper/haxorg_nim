import haxorg/[types, ast_diff, parser]
import std/[sequtils, strutils]
import hmisc/macros/ast_spec
import std/strformat
export ast_diff
import std/sugar
import hmisc/other/oswrap
import std/tables
import hmisc/other/hshell
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


proc toSexp*(tok: OrgToken): SexpNode =
  result = newSList()
  result.add newSSymbol($tok.kind)
  if not tok.strVal().empty():
    result.add newSString(tok.strVal())

proc toSexp*(node: OrgNode): SexpNode =
  ## Convert org-mode mode to the S-expression
  result = newSList()
  result.add newSSymbol($node.kind)
  if node of orgTokenKinds:
    result.add newSString(node.strVal())

  else:
    for idx, sub in node:
      let name = orgSubnodeFieldName(node, idx)
      if name.isSome() and orgIsSingularField(node, idx):
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
    result = newEmptiedTree(kind)
    for sub in node.elems[1..^1]:
      if sub of SKeyword:
        result[sub.key] = sub.value.toOrg()

      else:
        result.add(sub.toOrg())



type
  TestFileLexerMode* = enum
    TLFull
    TLStructure

  TestFile* = object
    name*: string
    filename*: string
    expected*: OrgNode
    givenRaw*: string
    tokens*: seq[TestToken]
    lexerTestMode*: TestFileLexerMode
    lexed*: seq[TestToken]
    parsed*: OrgNode


  TestTokenCmpKind* = enum
    TCFull
    TCMatchall

  TestToken* = object
    kind*: OrgTokenKind
    strVal*: string
    cmpKind*: TestTokenCmpKind

proc toSexp*(tok: TestToken): SexpNode =
  result = newSList()
  result.add newSSymbol($tok.kind)
  if not tok.strVal.empty():
    result.add newSString(tok.strVal)

proc `$`*(t: TestToken): string =
  "($# \"$#\")" % [$t.kind, t.strVal.replace("\n", "␤")]

proc `==`*(lhs, rhs: TestToken): bool =
  case lhs.cmpKind:
    of TCFull:
      return lhs.kind == rhs.kind and lhs.strVal == rhs.strVal

    of TCMatchall:
      return lhs.kind == rhs.kind

proc toTest*(tok: OrgToken): TestToken =
  TestToken(kind: tok.kind, strVal: tok.strVal())

proc find*[T](s: seq[T], check: proc(item: T): bool, start: int): int =
  result = -1
  for idx in start ..< s.len():
    if check(s[idx]):
      return idx

proc parseTestFile*(text: string): TestFile =
  let split = text.split("\n")
  proc find(start: int): int =
    split.find(it => it.startsWith("==="), start)

  # First mandatory separator
  let afterConf = find(0)
  for item in split[0 ..< afterConf]:
    let split = item.split(":")
    case split[0].normalize():
      of "lex":
        case split[1].normalize():
          of "structure":
            result.lexerTestMode = TLStructure

          of "full":
            result.lexerTestMode = TLFull

          else:
            raise newUnexpectedKindError(split[1])

      else:
        raise newUnexpectedKindError(split[0])


  assert(split[afterConf].startsWith("==="))
  # Second separator after the name
  let afterName = find(afterConf + 1)
  assert(split[afterName].startsWith("==="))
  result.name = split[afterConf + 1]
  let treeDelimiter = find(afterName + 1)
  let tokenDelimiter = if treeDelimiter != -1: find(treeDelimiter + 1) else: -1
  if treeDelimiter == -1:
    result.givenRaw = split[afterName + 1 .. ^1].join("\n")

  else:
    result.givenRaw = split[afterName + 1 ..< treeDelimiter].join("\n")

  if tokenDelimiter == -1 and treeDelimiter != -1:
    result.expected = parseSexp(
      split[treeDelimiter + 1 .. ^1].join("\n")).toOrg()

  elif tokenDelimiter == -1 and treeDelimiter == -1:
    discard

  else:
    if treeDelimiter + 1 < tokenDelimiter:
      result.expected = parseSexp(
        split[treeDelimiter + 1 ..< tokenDelimiter].join("\n")).toOrg()

    for line in split[tokenDelimiter + 1 .. ^1]:
      if line.empty() or line.startsWith("#"):
        continue

      # Skipping optional index prefixes on the line: `[idx]:` or similar
      # elements.
      let token = parseSexp(line[line.find('(') .. ^1])
      let kind = parseEnum[OrgTokenKind](token[0].getSymbol())
      if 1 < token.len():
        let sym = token[1].getSymbol()
        if sym == "_":
          result.tokens.add TestToken(kind: kind, cmpKind: TCMatchall)

        else:
          result.tokens.add TestToken(
            kind: kind, strVal: token[1].getStr())

      else:
        result.tokens.add TestToken(kind: kind)

proc diffOrg*(
    src, dst: OrgNode,
    file: AbsFile,
    withSubnodeNames: bool = false,
    srcLabel: string = "Src tree",
    dstLabel: string = "Dst tree"
  ) =

  let diff = diff(src, dst, minHeight = 2)
  let data = explainGraphvizDiff(diff)
  var conf = initGraphvizFormat[OrgNode]()
  conf.dstLabel = dstLabel
  conf.srcLabel = srcLabel
  conf.formatKind = proc(kind: int): string = $OrgNodeKind(kind)
  conf.formatLink = proc(node: OrgNode, idx: int): Option[string] =
    if withSubnodeNames:
      result = orgSubnodeFieldName(node, idx)

  conf.formatValue = proc(value: OrgNode): string =
    if value of orgTokenKinds and
       not (value of { orgEmpty }):

      let str = value.strVal()
      if '\n' in str:
        result.add "\l" & str.replace("\n", "\l")

      else:
        result.add "\\\""
        result.add str
        result.add "\\\""

  let
    format = formatGraphvizDiff(diff, data, conf)
    file = file.withExt("dot")
    image = file.withExt("png")

  writeFile(file, format)
  shellCmd("dot", "-Tpng", $file, "-o", $image).execShell()



when isMainModule:
  proc p(str: string): OrgNode = parseSexp(str).toOrg()

  echo treeRepr(p("(Ident)"))
