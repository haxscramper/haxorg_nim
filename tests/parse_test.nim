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

proc eqTree(n1, n2: OrgNode): bool =
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
  if not tokens.empty() and inTokens != tokens:
    var rhs = mapIt(tokens, hshow(it))
    var lhs = mapIt(inTokens, hshow(it))
    let rmax = maxIt(rhs, 0, len(it))
    let lmax = maxIt(lhs, 0, len(it))
    var cmp: seq[ColoredText]
    echo clt("expected") |<< lmax, "got"
    for idx in 0 ..< max(len(rhs), len(lhs)):
      cmp.add(
        tern(idx < len(lhs), lhs[idx], clt("")) |<< lmax &
          " " &
          tern(idx < len(rhs), rhs[idx], clt("")) |<< rmax &
          tern(
            idx < min(len(rhs), len(lhs)) and lhs[idx] != rhs[idx],
            " <<< " + fgRed,
            clt(""))
        ,
      )

    echo cmp.join(clt("\n"))
    return false

  if notNil(tree):
    let inTree = orgParse(inTokens)
    if not eqTree(inTree, tree):
      writeFile("/tmp/parsed.nim", treeRepr(inTree).toPlainString())
      writeFile("/tmp/expected.nim", treeRepr(tree).toPlainString())
      echo "\n\n", $treeRepr(tree), "\n!=\n", $treeRepr(inTree)

      assert false

      return false

  return true

proc runTest(text: string, tree: OrgNode): bool =
  runTest(text, @[], tree)

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

func list(sub: varargs[OrgNode]): OrgNode =
  ast(orgList, @sub)

func li(body: OrgNode = e()): OrgNode =
  ast(orgListItem, @[
    e(), # bullet
    e(), # counter
    e(), # checkbox
    e(), # tag
    e(), # header
    e(), # completion
    body, # body
  ])

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
      stmt(par(bold(word("two"), space(" "), word("words"))))
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
    check runTest("- item", stmt(list(li(stmt(par(word("item")))))))
    check runTest("- one\n- two", stmt(list(
      li(stmt(par(word("one")))),
      li(stmt(par(word("two"))))
    )))

    check runTest(
      "- top\n  - inside",
      @[
        tok(OStListDash, "-"),
        tok(OStStmtListOpen),
        tok(OTxParagraphStart),
        tok(OTxWord, "top"),
        tok(OTxParagraphEnd),
        tok(OStStmtListClose),
        tok(OStListItemEnd),
        tok(OstIndent),
          tok(OStListDash, "-"),
            tok(OStStmtListOpen),
              tok(OTxParagraphStart),
                tok(OTxWord, "inside"),
              tok(OTxParagraphEnd),
            tok(OStStmtListClose),
          tok(OStListItemEnd),
        tok(OStDedent)
      ],
      stmt(list(
        li(stmt(
          par(word("top")),
          list(li(stmt(par(word("inside"))))))))))
    
    check runTest("""
- TOP0
  - INDENT-1
  - SAME-1
    - NES-2
- TOP1
  - IND-1

    MULTILINE
    - NES-20

      #+begin_src
      content
      #+end_src
    - NES-21
  - SEC""",
      @[
        tok(OStListDash, "-"),

        tok(OStStmtListOpen), tok(OTxParagraphStart),
        tok(OTxWord, "TOP0"),
        tok(OTxParagraphEnd), tok(OstStmtListClose),

        tok(OStListItemEnd, ""),
        tok(OStIndent, ""),
          tok(OStListDash, "-"),
          # Expanded from text
          tok(OStStmtListOpen), tok(OTxParagraphStart),
          tok(OTxWord, "INDENT-1"),
          tok(OTxParagraphEnd), tok(OstStmtListClose),

          tok(OStListItemEnd),
          tok(OStSameIndent),
          tok(OStListDash, "-"),
          # Expanded
          tok(OStStmtListOpen), tok(OTxParagraphStart),
          tok(OTxWord, "SAME-1"),
          tok(OTxParagraphEnd), tok(OstStmtListClose),

          tok(OStListItemEnd, ""),
          tok(OStIndent, ""),
            tok(OStListDash, "-"),
            # Expanded
            tok(OStStmtListOpen), tok(OTxParagraphStart),
            tok(OTxWord, "NES-2"),
            tok(OTxParagraphEnd), tok(OstStmtListClose),

            tok(OStListItemEnd, ""),
          tok(OStDedent, ""),
        tok(OStDedent, ""),
        tok(OStListDash, "-"),

        tok(OStStmtListOpen), tok(OTxParagraphStart),
        tok(OTxWord, "TOP1"),
        tok(OTxParagraphEnd), tok(OstStmtListClose),

        tok(OStListItemEnd, ""),
        tok(OStIndent, ""),
          tok(OStListDash, "-"),

          # Paragraph expanded tokens
          tok(OStStmtListOpen),
            tok(OTxParagraphStart),
            tok(OTxWord, "IND-1"),
            tok(OTxParagraphEnd),
            tok(OTxParagraphStart),
            tok(OTxBigIdent, "MULTILINE"),
            tok(OTxParagraphEnd),
          tok(OstStmtListClose),

          tok(OStListItemEnd, ""),
            tok(OStIndent, ""),
            tok(OStListDash, "-"),
            tok(OStStmtListOpen),
              tok(OTxParagraphStart),
              tok(OTxWord, "NES-20"),
              tok(OTxParagraphEnd),

              tok(OStCommandPrefix, "#+"),
              tok(OStCommandBegin, "begin_src"),
              # arguments
              tok(OStCommandArgumentsBegin),
              tok(OTxRawText),
              tok(OStCommandArgumentsEnd),
              # content
              tok(OStCommandContentStart),
              tok(OStCodeContentBegin),
              tok(OStCodeText, "content\n     "),
              tok(OStCodeContentEnd),
              tok(OStCommandContentEnd),

              tok(OStCommandPrefix, "#+"),
              tok(OStCommandEnd, "end_src"),
            tok(OstStmtListClose),

            tok(OStListItemEnd, ""),
            tok(OStSameIndent, ""),
            tok(OStListDash, "-"),

            tok(OStStmtListOpen), tok(OTxParagraphStart),
            tok(OTxWord, "NES-21"),
            tok(OTxParagraphEnd), tok(OstStmtListClose),

            tok(OStListItemEnd, ""),
          tok(OStDedent, ""),
          tok(OStListDash, "-"),
            tok(OStStmtListOpen), tok(OTxParagraphStart),
            tok(OTxBigIdent, "SEC"),
            tok(OTxParagraphEnd), tok(OstStmtListClose),
          tok(OStListItemEnd, ""),
        tok(OStDedent, ""),
      ],
      stmt(
        list(
          li(stmt(
            par(word("TOP0")),
            list(
              li(stmt(par(word("INDENT-1")))),
              li(stmt(
                par(word("SAME-1")),
                list(li(stmt(par(word("NES-2")))))
              ))
            )),
          ),
          li(stmt(
            par(word("TOP1")),
            list(
              li(stmt(
                par(word("IND-1")),
                par(bigIdent("MULTILINE")),
                list(
                  li(stmt(
                    par(word("NES-20")),
                    ast(orgSrcCode, @[
                      e(),
                      e(),
                      stmt(ast(orgCodeLine, @[
                        ast(orgCodeText, "content\n     ")])),
                      e()
                    ])
                  )),
                  li(stmt(par(word("NES-21"))))
                )
              )),
              li(stmt(par(bigIdent("SEC"))))
            )
          ))
        )
      )
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
        hashtag(raw("#tag"), [
          hashtag(raw("#sub"), [
            hashtag(raw("nested1")),
            hashtag(raw("nested2"), [
              hashtag(raw("#sub"), [
                hashtag(raw("t1")),
                hashtag(raw("t2"))
              ])
            ])
          ])
        ]))))

  test "Subtree mixed drawer, logbook":
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
        tok(OStStmtListOpen),
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
        tok(OStStmtListClose),
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
            space(" "),
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
              ast(orgProperty, @[raw(":CREATED:"), e(), raw("[2022-09-18 Sun 22:30:14]")]),
              ast(orgProperty, @[raw(":ID:"), e(), raw("8f4e3847-daf6-4bf5-affd-dcafca4ba410")]),
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
          ]),
          stmt()
        ])
      )
    )

  test "Subtree multi-entry logbook":
    check runTest("""
* Name
  :LOGBOOK:
  - Refiled on [2022-07-03 Sun 00:40:47] from [[id:ID-T][Name]]
  CLOCK: [2022-07-03 Sun 08:38:19]--[2022-07-03 Sun 09:10:34] =>  0:32
  - State "WIP"        from "TODO"       [2022-07-03 Sun 08:38:19]
  - State "COMPLETED"  from "WIP"        [2022-07-03 Sun 09:10:34]
  :END:
""",
    stmt(
      ast(orgSubtree, @[
        raw("*"), # prefix
        e(), # todo
        e(), # urgency
        par(word("Title")), # title
        e(), # completion
        e(), # tags
        e(), # times
        ast(orgDrawer, @[
          e(), # property list
          ast(orgLogbook, @[
            ast(orgLogbookRefile, {
              "on": time("[2022-07-03 Sun 00:40:47]"),
              "from": link(
                protocol = ident("id"),
                target = raw("ID-T"),
                description = par(word("Name"))
              )
            }),
            ast(orgLogbookClock, {
              "time": ast(orgTimeRange, {
                "from": time("[2022-07-03 Sun 08:38:19]"),
                "to": time("[2022-07-03 Sun 09:10:34]"),
                "diff": ast(orgSimpleTime, "0:32")
              })
            }),
            ast(orgLogbookStateChange, {
              "oldstate": bigIdent("TODO"),
              "newstate": bigIdent("WIP"),
              "time": time("[2022-07-03 Sun 08:38:19]"),
              "note": e()
            }),
            ast(orgLogbookStateChange, {
              "oldstate": bigIdent("WIP"),
              "newstate": bigIdent("COMPLETED"),
              "time": time("[2022-07-03 Sun 09:10:34]"),
              "note": e()
            })
          ])
        ])
      ])
    ))
