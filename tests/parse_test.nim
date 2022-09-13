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

proc runTest(text: string, tokens: seq[OrgToken], tree: OrgNode) =
  let inTokens = orgLex(text)
  assert inTokens == tokens, $hshow(inTokens) & "\n!=\n" & $hshow(tokens)
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

func ident(txt: string): OrgNode = ast(orgIdent, OTxWord, txt)
func raw(txt: string): OrgNode = ast(orgRawText, OTxWord, txt)

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
  "[[code:macro!matchdiff]]",
  partok [
    tok(OTxLinkOpen, "["),
    tok(OTxLinkTargetOpen, "["),
    tok(OTxLinkProtocol, "code"),
    tok(OTxLinkTarget, "macro!matchdiff"),
    tok(OTxLinkTargetClose, "]"),
    tok(OTxLinkClose, "]")
  ],
  stmt(par(link(
    protocol = ident("code"),
    target = raw("macro!matchdiff")
  )))
)
