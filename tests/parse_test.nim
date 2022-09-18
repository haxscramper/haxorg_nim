import hmisc/preludes/unittest
import haxorg/[
  parser,
  types
]

func `==`(t1, t2: OrgToken): bool =
  t1.strVal() == t2.strVal() and t1.kind == t2.kind

func eqTree(n1, n2: OrgNode): bool =
  if n1.kind == n2.kind:
    case n1.kind:
      of orgTokenKinds:
        result = n1.strVal() == n2.strVal()

      else:
        if len(n1) == len(n2):
          result = true
          for idx in 0 ..< len(n1):
            if not eqTree(n1[idx], n2[idx]):
              return false

proc runTest(text: string, tokens: seq[OrgToken], tree: OrgNode = nil) =
  let inTokens = orgLex(text)
  assert inTokens == tokens, "\n\n" & $hshow(inTokens) & "\n!=\n" & $hshow(tokens)
  if notNil(tree):
    let inTree = orgParse(inTokens)
    assert eqTree(inTree, tree), "\n\n" & $treeRepr(inTree) & "\n!=\n" & $treeRepr(tree)

func tok(k: OrgTokenKind, v: string = ""): OrgToken =
  initFakeTok(k, v)

func ast(k: OrgNodeKind, sub: seq[OrgNode] = @[]): OrgNode =
  newTree(k, sub)

func ast(k: OrgNodeKind, tk: OrgTokenKind, tok: string): OrgNode =
  newTree(k, tok(tk, tok))

func stmt(sub: varargs[OrgNode]): OrgNode = ast(orgStmtList, @sub)
func par(sub: varargs[OrgNode]): OrgNode = ast(orgParagraph, @sub)
func word(txt: string): OrgNode = ast(orgWord, OTxWord, txt)
func bold(sub: varargs[OrgNode]): OrgNode = ast(orgBold, @sub)
func link(protocol, target, description: OrgNode = newEmptyNode()): OrgNode =
  ast(orgLink, @[protocol, target, description])

func table(sub: varargs[OrgNode]): OrgNode = ast(orgTable, @sub)
func row(sub: varargs[OrgNode]): OrgNode = ast(orgTableRow, @sub)
func cell(sub: varargs[OrgNode]): OrgNode = ast(orgTableCell, @sub)

func ident(txt: string): OrgNode = ast(orgIdent, OTxWord, txt)
func raw(txt: string): OrgNode = ast(orgRawText, OTxWord, txt)

func e(): OrgNode = newEmptyNode()

func partok(toks: openarray[OrgToken]): seq[OrgToken] =
  result.add tok(OTxParagraphStart)
  result.add @toks
  result.add tok(OTxParagraphEnd)

runTest(
  "*bold*",
  partok [
    tok(OTxBoldOpen, "*"),
    tok(OTxWord, "bold"),
    tok(OTxBoldClose, "*"),
  ],
  stmt(par(bold(word("bold"))))
)

runTest(
  "*two words*",
  partok [
    tok(OTxBoldOpen, "*"),
    tok(OTxWord, "two"),
    tok(OTxSpace, " "),
    tok(OTxWord, "words"),
    tok(OTxBoldClose, "*"),
  ],
  stmt(par(bold(word("two"), word(" "), word("words"))))
)

runTest(
  "[[code:macro!matchdiff][*description*]]",
  partok [
    tok(OTxLinkOpen, "["),
    tok(OTxLinkTargetOpen, "["),
    tok(OTxLinkProtocol, "code"),
    tok(OTxLinkTarget, "macro!matchdiff"),
    tok(OTxLinkTargetClose, "]"),
    tok(OTxLinkDescriptionOpen, "["),
    tok(OTxBoldOpen, "*"),
    tok(OTxWord, "description"),
    tok(OTxBoldClose, "*"),
    tok(OTxLinkDescriptionClose, "]"),
    tok(OTxLinkClose, "]")
  ],
  stmt(par(link(
    protocol = ident("code"),
    target = raw("macro!matchdiff"),
    description = par(bold(word("description")))
  )))
)

runTest(
  r"\(\inline\)",
  partok [
    tok(OTxLatexParOpen, r"\("),
    tok(OTxLatexInlineRaw, r"\inline"),
    tok(OTxLatexParClose, r"\)")
  ],
  stmt(par(ast(orgInlineMath, @[raw(r"\inline")])))
)

runTest(
  r"\[\display\]",
  partok [
    tok(OTxLatexBraceOpen, r"\["),
    tok(OTxLatexInlineRaw, r"\display"),
    tok(OTxLatexBraceClose, r"\]")
  ],
  stmt(par(ast(orgDisplayMath, @[raw(r"\display")])))
)

runTest(
  r"\Uuml{}",
  partok [
    tok(OTxSymbol, r"\Uuml{}"),
  ],
  stmt(par(ast(orgSymbol, OTxSymbol, r"\Uuml{}")))
)

runTest(
  "{{{test(arg, other)}}}",
  partok [
    tok(OTxMacroOpen, "{{{"),
    tok(OTxMacroName, "test"),
    tok(OTxParOpen, "("),
    tok(OTxMacroArg, "arg"),
    tok(OTxComma, ","),
    tok(OTxMacroArg, "other"),
    tok(OTxParClose, ")"),
    tok(OTxMacroClose, "}}}")
  ],
  stmt(par(ast(
    orgMacro,
    @[
      ident("test"),
      raw("arg"),
      raw("other")
  ])))
)

runTest(
  """
#+begin-table :width 12cm
| r1c1 | r1c2 |
| r2c1
| r2c2
#+row
r3c1
#+cell
r3c2
#+end-table
""",
  @[
    tok(OTbTableBegin, "begin-table"),
    tok(OTbCmdArguments, ":width 12cm"),

    # row 1
    tok(OTbPipeOpen, "|"),
      tok(OTbContentStart),
        tok(OTxParagraphStart),
          tok(OTxWord, "r1c1"),
        tok(OTxParagraphEnd),
      tok(OTbContentEnd),
    tok(OTbPipeSeparator, "|"),
      tok(OTbContentStart),
        tok(OTxParagraphStart),
          tok(OTxWord, "r1c2"),
        tok(OTxParagraphEnd),
      tok(OTbContentEnd),
    tok(OTbPipeClose),

    # row 2
    tok(OTbPipeCellOpen, "|"),
      tok(OTbContentStart),
        tok(OTxParagraphStart),
          tok(OTxWord, "r2c1"),
        tok(OTxParagraphEnd),
      tok(OTbContentEnd),
    tok(OTbPipeCellOpen, "|"),
      tok(OTbContentStart),
        tok(OTxParagraphStart),
          tok(OTxWord, "r2c2"),
        tok(OTxParagraphEnd),
      tok(OTbContentEnd),

    # row 3
    tok(OTbRowSpec, "row"),
    tok(OTbCmdArguments),
      tok(OTbContentStart),
        tok(OTxParagraphStart),
          tok(OTxWord, "r3c1"),
        tok(OTxParagraphEnd),
      tok(OTbContentEnd),
    tok(OTbCellSpec, "cell"),
    tok(OTbCmdArguments),
      tok(OTbContentStart),
        tok(OTxParagraphStart),
          tok(OTxWord, "r3c2"),
        tok(OTxParagraphEnd),
      tok(OTbContentEnd),
    tok(OTbTableEnd, "end-table"),
    tok(otEof)
  ],
  stmt(table(
    e(),
    row(
      e(), e(),
      cell(e(), stmt(par(word("r1c1")))),
      cell(e(), stmt(par(word("r1c2"))))),
    row(
      e(), e(),
      cell(e(), stmt(par(word("r2c1")))),
      cell(e(), stmt(par(word("r2c2"))))),
    row(
      e(), e(),
      cell(e(), stmt(par(word("r3c1")))),
      cell(e(), stmt(par(word("r3c2")))))))
)

runTest(
  r"\*word",
  partok [
    tok(OTxEscaped, r"\*"),
    tok(OTxWord, "word")
  ],
  stmt(par(ast(orgEscaped, OTxEscaped, r"\*"), word("word"))))

runTest(
  r"\/\/word",
  partok [
    tok(OTxEscaped, r"\/"),
    tok(OTxEscaped, r"\/"),
    tok(OTxWord, "word")
  ],
  stmt(par(
    ast(orgEscaped, OTxEscaped, r"\/"),
    ast(orgEscaped, OTxEscaped, r"\/"),
    word("word"))))

runTest(
  "dashed-word",
  partok [ tok(OTxWord, "dashed-word") ],
  stmt(par(word("dashed-word")))
)

runTest(
  "@user",
  partok [ tok(OTxAtMention, "@user") ],
  stmt(par(ast(orgAtMention, OTxAtMention, "@user")))
)
