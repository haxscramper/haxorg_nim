import hmisc/preludes/unittest
import haxorg/[
  parser,
  types
]

import std/[
  sequtils
]

import hmisc/algo/htemplates

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

proc runTest(text: string, tokens: seq[OrgToken], tree: OrgNode = nil): bool =
  let inTokens = orgLex(text)
  if inTokens != tokens:
    var rhs = mapIt(inTokens, hshow(it))
    var lhs = mapIt(tokens, hshow(it))
    let rmax = maxIt(rhs, 0, len(it))
    let lmax = maxIt(lhs, 0, len(it))
    var cmp: seq[ColoredText]
    for idx in 0 ..< max(len(rhs), len(lhs)):
      cmp.add(
        tern(idx < len(rhs), rhs[idx], clt("")) |<< rmax &
          " " &
          tern(idx < len(lhs), lhs[idx], clt("")) |<< lmax &
          tern(
            idx < min(len(lhs), len(rhs)) and lhs[idx] != rhs[idx],
            " <<< " + fgRed,
            clt(""))
        ,
      )

    echo cmp.join(clt("\n"))


    return false

  if notNil(tree):
    let inTree = orgParse(inTokens)
    if not eqTree(inTree, tree):
       echo "\n\n", $treeRepr(inTree), "\n!=\n", $treeRepr(tree)
       return false

  return true

func tok(k: OrgTokenKind, v: string = ""): OrgToken =
  initFakeTok(k, v)

func ast(k: OrgNodeKind, sub: seq[OrgNode] = @[]): OrgNode =
  newTree(k, sub)

func ast(k: OrgNodeKind, tk: OrgTokenKind, tok: string): OrgNode =
  newTree(k, tok(tk, tok))

func stmt(sub: varargs[OrgNode]): OrgNode = ast(orgStmtList, @sub)
func par(sub: varargs[OrgNode]): OrgNode = ast(orgParagraph, @sub)
func word(txt: string): OrgNode = ast(orgWord, OTxWord, txt)
func space(txt: string): OrgNode = ast(orgSpace, OTxSpace, txt)
func bold(sub: varargs[OrgNode]): OrgNode = ast(orgBold, @sub)
func mono(sub: varargs[OrgNode]): OrgNode = ast(orgMonospace, @sub)
func verb(sub: varargs[OrgNode]): OrgNode = ast(orgVerbatim, @sub)
func link(protocol, target, description: OrgNode = newEmptyNode()): OrgNode =
  ast(orgLink, @[protocol, target, description])

func punct(text: string): OrgNode = ast(orgPunctuation, OTxWord, text)
func table(sub: varargs[OrgNode]): OrgNode = ast(orgTable, @sub)
func row(sub: varargs[OrgNode]): OrgNode = ast(orgTableRow, @sub)
func cell(sub: varargs[OrgNode]): OrgNode = ast(orgTableCell, @sub)
func time(time: string): OrgNode = ast(orgTimeStamp, OStBracketTime, time)
func ident(txt: string): OrgNode = ast(orgIdent, OTxIdent, txt)
func bigIdent(txt: string): OrgNode = ast(orgBigIdent, OTxBigIdent, txt)
func raw(txt: string): OrgNode = ast(orgRawText, OTxWord, txt)
func hashtag(word: OrgNode, sub: openarray[OrgNode] = @[]): OrgNode =
  ast(orgHashTag, word & @sub)

func e(): OrgNode = newEmptyNode()

func partok(toks: openarray[OrgToken]): seq[OrgToken] =
  result.add tok(OTxParagraphStart)
  result.add @toks
  result.add tok(OTxParagraphEnd)

suite "Text parsing":
  test "Formatting":
    check runTest(
      "*bold*",
      partok [
        tok(OTxBoldOpen, "*"),
        tok(OTxWord, "bold"),
        tok(OTxBoldClose, "*"),
      ],
      stmt(par(bold(word("bold"))))
    )

    check runTest(
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

    check runTest(
      "dashed-word",
      partok [ tok(OTxWord, "dashed-word") ],
      stmt(par(word("dashed-word")))
    )

    check runTest(
      "@user",
      partok [ tok(OTxAtMention, "@user") ],
      stmt(par(ast(orgAtMention, OTxAtMention, "@user")))
    )

    check runTest(
      "NOTE",
      partok [ tok(OTxBigIdent, "NOTE") ],
      stmt(par(ast(orgBigIdent, OTxBigIdent, "NOTE")))
    )

  test "Links":
    check runTest(
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

  test "Slash entries":
    check runTest(
      r"\(\inline\)",
      partok [
        tok(OTxLatexParOpen, r"\("),
        tok(OTxLatexInlineRaw, r"\inline"),
        tok(OTxLatexParClose, r"\)")
      ],
      stmt(par(ast(orgInlineMath, @[raw(r"\inline")])))
    )

    check runTest(
      r"\*word",
      partok [
        tok(OTxEscaped, r"\*"),
        tok(OTxWord, "word")
      ],
      stmt(par(ast(orgEscaped, OTxEscaped, r"\*"), word("word"))))

    check runTest(
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

    check runTest(
      r"\[\display\]",
      partok [
        tok(OTxLatexBraceOpen, r"\["),
        tok(OTxLatexInlineRaw, r"\display"),
        tok(OTxLatexBraceClose, r"\]")
      ],
      stmt(par(ast(orgDisplayMath, @[raw(r"\display")])))
    )

    check runTest(
      r"\Uuml{}",
      partok [
        tok(OTxSymbolStart, r"\"),
        tok(OTxIdent, "Uuml"),
        tok(OTxMetaArgsOpen, "{"),
        tok(OTxMetaArgsBody, ""),
        tok(OTxMetaArgsClose, "}")
      ],
      stmt(par(ast(orgSymbol, @[ident(r"Uuml"), e()])))
    )

    check runTest(
      r"\sym[:arg 12]{body1}{body2}",
      partok [
        tok(OTxSymbolStart, r"\"),
        tok(OTxIdent, "sym"),
        # `[:arg 12]`
        tok(OTxMetaBraceOpen, "["),
        tok(OTxMetaBraceBody, ":arg 12"),
        tok(OTxMetaBraceClose, "]"),
        # `{body1}`
        tok(OTxMetaArgsOpen, "{"),
        tok(OTxMetaArgsBody, "body1"),
        tok(OTxMetaArgsClose, "}"),
        # `{body2}`
        tok(OTxMetaArgsOpen, "{"),
        tok(OTxMetaArgsBody, "body2"),
        tok(OTxMetaArgsClose, "}"),
      ]
    )

  test "Macros":
    check runTest(
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

  test "Tables":
    check runTest(
      """
#+begin-table :width 12cm
| r1c1 | r1c2 |
| r2c1
| r2c2
#+row
r3c1
#+cell
r3c2
#+end-table""",
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
        tok(OTbTableEnd, "end-table")
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


  test "Lists":
    check runTest("""
    - TOP #0
      - INDENT-1
      - SAME-1
        - NES-2
    - TOP #1
      - IND-1

        MULTILINE
        - NES-2 #0

          #+begin_src
          content
          #+end_src
        - NES-2 #1
      - SEC
    """,
      @[
        tok(OStListDash, "-"),
        tok(OStText, "TOP #0\n"),
        tok(OStListItemEnd, ""),
        tok(OStIndent, ""),
          tok(OStListDash, "-"),
          # Expanded from text
          tok(OTxParagraphStart), tok(OTxWord, "INDENT-1"), tok(OTxParagraphEnd),
          tok(OStListItemEnd),
          tok(OStSameIndent),
          tok(OStListDash, "-"),
          # Expanded
          tok(OTxParagraphStart), tok(OTxWord, "SAME-1"), tok(OTxParagraphEnd),
          tok(OStListItemEnd, ""),
          tok(OStIndent, ""),
            tok(OStListDash, "-"),
            # Expanded
            tok(OTxParagraphStart), tok(OTxWord, "NES-2"), tok(OTxParagraphEnd),
            tok(OStListItemEnd, ""),
          tok(OStDedent, ""),
        tok(OStDedent, ""),
        tok(OStListDash, "-"),
        tok(OStText, "TOP #1\n"),
        tok(OStListItemEnd, ""),
        tok(OStIndent, ""),
          tok(OStListDash, "-"),

          # Paragraph expanded tokens
          tok(OTxParagraphStart),
          tok(OTxWord, "NES-2"),
          tok(OTxNewline, "\n"),
          tok(OTxNewline, "\n"),
          tok(OTxSpace, "  "),
          tok(OTxWord, "MULTILINE"),
          tok(OTxParagraphEnd),

          tok(OStListItemEnd, ""),
            tok(OStIndent, ""),
            tok(OStListDash, "-"),
            tok(OStText, """NES-2 #0

          #+begin_src
          content
          #+end_src
    """),
            tok(OStListItemEnd, ""),
            tok(OStSameIndent, ""),
            tok(OStListDash, "-"),
            tok(OStText, "NES-2 #1\n"),
            tok(OStListItemEnd, ""),
          tok(OStDedent, ""),
          tok(OStListDash, "-"),
          tok(OStText, "SEC"),
          tok(OStListItemEnd, ""),
        tok(OStDedent, ""),
      ]
    )

  test "Tags":
    check runTest(
      "#tag##sub##[nested1, nested2##sub##[t1, t2]]",
      partok [
        tok(OTxHashTag, "#tag"),
        tok(OTxHashTagSub, "#"),
        tok(OTxHashTag, "#sub"),
        tok(OTxHashTagSub, "#"),
          tok(OTxHashTagOpen, "#["),
            tok(OTxHashTag, "nested1"),
            tok(OTxComma, ","),
            # `nested2##sub##[t1, t2]`
            tok(OTxHashTag, "nested2"),
              tok(OTxHashTagSub, "#"),
              tok(OTxHashTag, "#sub"),
              # `##[t1, t2]`
              tok(OTxHashTagSub, "#"),
                tok(OTxHashTagOpen, "#["),
                  tok(OTxHashTag, "t1"),
                  tok(OTxComma, ","),
                  tok(OTxHashTag, "t2"),
                tok(OTxHashTagClose, "]"),
          tok(OTxHashTagClose, "]")
      ],
      stmt(par(
        hashtag(
          raw("#tag"),
          [
          hashtag(
            raw("#sub"),
            [
              hashtag(raw("nested1")),
              hashtag(
                raw("nested2"),
                [
                  hashtag(
                    raw("#sub"),
                    [
                      hashtag(raw("t1")),
                      hashtag(raw("t2"))
                    ])
                ])
            ])
          ]))))

  test "Subtrees":
    check runTest("""
**** FAILED [2022-09-18 Sun 22:30:14] from ~notes.org:32410~ (=5937E39D=)
     CLOSED: [2022-09-18 Sun 22:31:12]
     :PROPERTIES:
     :CREATED:  [2022-09-18 Sun 22:30:14]
     :ID:       8f4e3847-daf6-4bf5-affd-dcafca4ba410
     :END:
     :LOGBOOK:
     - State "FAILED"     from              [2022-09-18 Sun 22:31:12] \\
       Failed note
     :END:
""",
      @[
        tok(OStSubtreeStars, "****"),
        tok(OStSubtreeTodoState, "FAILED"),
        tok(OTxParagraphStart),
        tok(OStBracketTime, "[2022-09-18 Sun 22:30:14]"),
        tok(OTxSpace, " "),
        tok(OTxWord, "from"),
        tok(OTxSpace, " "),
        tok(OTxMonospaceOpen, "~"),
        tok(OTxRawText, "notes.org:32410"),
        tok(OTxMonospaceClose, "~"),
        tok(OTxSpace, " "),

        # `(=5937E39D=)`
        tok(OTxParOpen, "("),
        tok(OTxVerbatimOpen, "="),
        tok(OTxRawText, "5937E39D"),
        tok(OTxVerbatimClose, "="),
        tok(OTxParClose, ")"),

        tok(OTxParagraphEnd),
        tok(OStSubtreeTime, "CLOSED"),
        tok(OStBracketTime, "[2022-09-18 Sun 22:31:12]"),
        tok(OStColonProperties, ":PROPERTIES:"),
        tok(OStColonIdent, ":CREATED:"),
        tok(OStRawProperty, "[2022-09-18 Sun 22:30:14]"),
        tok(OStColonIdent, ":ID:"),
        tok(OStRawProperty, "8f4e3847-daf6-4bf5-affd-dcafca4ba410"),
        tok(OStColonEnd, ":END:"),
        tok(OStColonLogbook, ":LOGBOOK:"),
        tok(OStLogbookStart),
        tok(OStIndent),
        tok(OStDedent),
        tok(OStListDash, "-"),
        tok(OTxParagraphStart),
        tok(OTxWord, "State"),
        tok(OTxSpace, " "),
        tok(OTxQuoteOpen, "\""),
        tok(OTxBigIdent, "FAILED"),
        tok(OTxQuoteClose, "\""),
        tok(OTxSpace, "     "),
        tok(OTxWord, "from"),
        tok(OTxSpace, "              "),
        tok(OStBracketTime, "[2022-09-18 Sun 22:31:12]"),
        tok(OTxSpace, " "),
        tok(OTxDoubleSlash, "\\\\"),
        tok(OTxNewline, "\n"),
        tok(OTxSpace, "       "),
        tok(OTxWord, "Failed"),
        tok(OTxSpace, " "),
        tok(OTxWord, "note"),
        tok(OTxNewline, "\n"),
        tok(OTxSpace, "     "),
        tok(OTxParagraphEnd),
        tok(OStListItemEnd),
        tok(OStSameIndent),
        tok(OStLogbookEnd),
        tok(OStColonEnd, ":END:"),
        tok(OStSubtreeEnd)
      ],
      stmt(
        ast(orgSubtree, @[
          # prefix
          raw("****"),
          # todo
          bigIdent("FAILED"),
          # urgency
          e(),
          # title
          par(
            time("[2022-09-18 Sun 22:30:14]"),
            space(" "),
            word("from"),
            space(" "),
            mono(raw("notes.org:32410")),
            punct("("),
            verb(raw("5937E39D")),
            punct(")")
          ),
          # completion
          e(),
          # tags
          e(),
          # times
          stmt(
            ast(orgTimeAssoc, @[
              bigIdent("CLOSED"), time("[2022-09-18 Sun 22:31:12]")
            ])
          ),
          # drawers
          ast(orgDrawer, @[
            ast(orgPropertyList, @[
              ast(orgProperty, @[
                raw(":CREATED:"), raw("[2022-09-18 Sun 22:30:14]"),
                raw(":ID:"), raw("8f4e3847-daf6-4bf5-affd-dcafca4ba410")
              ]),
              ast(orgLogbook, @[
                ast(orgLogbookStateChange, @[
                  # newstate
                  bigIdent("FAILED"),
                  # oldstate
                  e(),
                  time("[2022-09-18 Sun 22:31:12]"),
                  stmt(par(
                    word("Failed"),
                    space(" "),
                    word("note")
                  ))
                ])
              ])
            ])
          ])
        ])
      )
    )
