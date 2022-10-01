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

proc runTest(
    text: string,
    tokens: seq[OrgToken],
    tree: OrgNode = nil,
    printTokens: bool = false
  ): bool =

  let inTokens = orgLex(text)

  if tokens.empty() and printTokens:
    echov hshow(inTokens)

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

proc runTest(text: string, tree: OrgNode, printTokens: bool = false): bool =
  runTest(text, @[], tree, printTokens)

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
func word(txt: string): OrgNode = ast(orgWord, OTkWord, txt)
func space(txt: string): OrgNode = ast(orgSpace, OTkSpace, txt)
func bold(sub: varargs[OrgNode]): OrgNode = ast(orgBold, @sub)
func mono(sub: varargs[OrgNode]): OrgNode = ast(orgMonospace, @sub)
func verb(sub: varargs[OrgNode]): OrgNode = ast(orgVerbatim, @sub)
func link(protocol, target, description: OrgNode = newEmptyNode()): OrgNode =
  ast(orgLink, @[protocol, target, description])

func punct(text: string): OrgNode = ast(orgPunctuation, OTkWord, text)
func table(sub: varargs[OrgNode]): OrgNode = ast(orgTable, @sub)
func row(sub: varargs[OrgNode]): OrgNode = ast(orgTableRow, @sub)
func cell(sub: varargs[OrgNode]): OrgNode = ast(orgTableCell, @sub)
func time(time: string): OrgNode = ast(orgTimeStamp, OTkBracketTime, time)
func ident(txt: string): OrgNode = ast(orgIdent, OTkIdent, txt)
func bigIdent(txt: string): OrgNode = ast(orgBigIdent, OTkBigIdent, txt)
func raw(txt: string): OrgNode = ast(orgRawText, OTkWord, txt)
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
  result.add tok(OTkParagraphStart)
  result.add @toks
  result.add tok(OTkParagraphEnd)

suite "Text parsing":
  test "Formatting":
    check runTest(
      "*bold*",
      partok [
        tok(OTkBoldOpen, "*"),
        tok(OTkWord, "bold"),
        tok(OTkBoldClose, "*"),
      ],
      stmt(par(bold(word("bold"))))
    )

    check runTest(
      "*two words*",
      partok [
        tok(OTkBoldOpen, "*"),
        tok(OTkWord, "two"),
        tok(OTkSpace, " "),
        tok(OTkWord, "words"),
        tok(OTkBoldClose, "*"),
      ],
      stmt(par(bold(word("two"), space(" "), word("words"))))
    )

    check runTest(
      "dashed-word",
      partok [ tok(OTkWord, "dashed-word") ],
      stmt(par(word("dashed-word")))
    )

    check runTest(
      "@user",
      partok [ tok(OTkAtMention, "@user") ],
      stmt(par(ast(orgAtMention, OTkAtMention, "@user")))
    )

    check runTest(
      "NOTE",
      partok [ tok(OTkBigIdent, "NOTE") ],
      stmt(par(ast(orgBigIdent, OTkBigIdent, "NOTE")))
    )

  test "Links":
    check runTest(
      "[[code:macro!matchdiff][*description*]]",
      partok [
        tok(OTkLinkOpen, "["),
        tok(OTkLinkTargetOpen, "["),
        tok(OTkLinkProtocol, "code"),
        tok(OTkLinkTarget, "macro!matchdiff"),
        tok(OTkLinkTargetClose, "]"),
        tok(OTkLinkDescriptionOpen, "["),
        tok(OTkBoldOpen, "*"),
        tok(OTkWord, "description"),
        tok(OTkBoldClose, "*"),
        tok(OTkLinkDescriptionClose, "]"),
        tok(OTkLinkClose, "]")
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
        tok(OTkLatexParOpen, r"\("),
        tok(OTkLatexInlineRaw, r"\inline"),
        tok(OTkLatexParClose, r"\)")
      ],
      stmt(par(ast(orgInlineMath, @[raw(r"\inline")])))
    )

    check runTest(
      r"\*word",
      partok [
        tok(OTkEscaped, r"\*"),
        tok(OTkWord, "word")
      ],
      stmt(par(ast(orgEscaped, OTkEscaped, r"\*"), word("word"))))

    check runTest(
      r"\/\/word",
      partok [
        tok(OTkEscaped, r"\/"),
        tok(OTkEscaped, r"\/"),
        tok(OTkWord, "word")
      ],
      stmt(par(
        ast(orgEscaped, OTkEscaped, r"\/"),
        ast(orgEscaped, OTkEscaped, r"\/"),
        word("word"))))

    check runTest(
      r"\[\display\]",
      partok [
        tok(OTkLatexBraceOpen, r"\["),
        tok(OTkLatexInlineRaw, r"\display"),
        tok(OTkLatexBraceClose, r"\]")
      ],
      stmt(par(ast(orgDisplayMath, @[raw(r"\display")])))
    )

    check runTest(
      r"\Uuml{}",
      partok [
        tok(OTkSymbolStart, r"\"),
        tok(OTkIdent, "Uuml"),
        tok(OTkMetaArgsOpen, "{"),
        tok(OTkMetaArgsBody, ""),
        tok(OTkMetaArgsClose, "}")
      ],
      stmt(par(ast(orgSymbol, @[ident(r"Uuml"), e()])))
    )

    check runTest(
      r"\sym[:arg 12]{body1}{body2}",
      partok [
        tok(OTkSymbolStart, r"\"),
        tok(OTkIdent, "sym"),
        # `[:arg 12]`
        tok(OTkMetaBraceOpen, "["),
        tok(OTkMetaBraceBody, ":arg 12"),
        tok(OTkMetaBraceClose, "]"),
        # `{body1}`
        tok(OTkMetaArgsOpen, "{"),
        tok(OTkMetaArgsBody, "body1"),
        tok(OTkMetaArgsClose, "}"),
        # `{body2}`
        tok(OTkMetaArgsOpen, "{"),
        tok(OTkMetaArgsBody, "body2"),
        tok(OTkMetaArgsClose, "}"),
      ]
    )

  test "Macros":
    check runTest(
      "{{{test(arg, other)}}}",
      partok [
        tok(OTkMacroOpen, "{{{"),
        tok(OTkIdent, "test"),
        tok(OTkParOpen, "("),
        tok(OTkRawText, "arg"),
        tok(OTkComma, ","),
        tok(OTkRawText, "other"),
        tok(OTkParClose, ")"),
        tok(OTkMacroClose, "}}}")
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
        tok(OTkTableBegin, "begin-table"),
        tok(OTkCmdArguments, ":width 12cm"),

        # row 1
        tok(OTkPipeOpen, "|"),
          tok(OTkContentStart),
            tok(OTkParagraphStart),
              tok(OTkWord, "r1c1"),
            tok(OTkParagraphEnd),
          tok(OTkContentEnd),
        tok(OTkPipeSeparator, "|"),
          tok(OTkContentStart),
            tok(OTkParagraphStart),
              tok(OTkWord, "r1c2"),
            tok(OTkParagraphEnd),
          tok(OTkContentEnd),
        tok(OTkPipeClose),

        # row 2
        tok(OTkPipeCellOpen, "|"),
          tok(OTkContentStart),
            tok(OTkParagraphStart),
              tok(OTkWord, "r2c1"),
            tok(OTkParagraphEnd),
          tok(OTkContentEnd),
        tok(OTkPipeCellOpen, "|"),
          tok(OTkContentStart),
            tok(OTkParagraphStart),
              tok(OTkWord, "r2c2"),
            tok(OTkParagraphEnd),
          tok(OTkContentEnd),

        # row 3
        tok(OTkRowSpec, "row"),
        tok(OTkCmdArguments),
          tok(OTkContentStart),
            tok(OTkParagraphStart),
              tok(OTkWord, "r3c1"),
            tok(OTkParagraphEnd),
          tok(OTkContentEnd),
        tok(OTkCellSpec, "cell"),
        tok(OTkCmdArguments),
          tok(OTkContentStart),
            tok(OTkParagraphStart),
              tok(OTkWord, "r3c2"),
            tok(OTkParagraphEnd),
          tok(OTkContentEnd),
        tok(OTkTableEnd, "end-table")
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

  test "Source code blocks":
    check runTest("""
#+begin_src sh -r :file /tmp/file.sh
<<tangle>> # (refs:callout)
#+end_src
""", stmt(
      ast(orgSrcCode, {
        "lang": ident("sh"),
        "header-args": ast(orgCmdArguments, {
          "flags": ast(orgInlineStmtList, @[ast(orgCmdFlag, "-r")]),
          "args": ast(orgInlineStmtList, @[ast(orgCmdValue, {
            "name": ident(":file"),
            "value": raw("/tmp/file.sh")
          })])
        }),
        "body": stmt(
          ast(orgCodeLine, @[
            ast(orgCodeTangle, @[ident("tangle")]),
            ast(orgCodeText, " # "),
            ast(orgCodeCallout, @[ident("callout")])
          ])
        ),
        "result": e()
      })
    ))


  test "Title":
    check runTest(
      "#+title: *b*",
      stmt(ast(orgCommandTitle, @[par(bold(word("b")))])))

    check runTest(
      "#+options: broken-links:mark",
      stmt(ast(orgCommandOptions, @[
        ast(orgInlineStmtList, @[raw("broken-links:mark")])
      ]))
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
        tok(OTkListDash, "-"),
        tok(OTkStmtListOpen),
        tok(OTkParagraphStart),
        tok(OTkWord, "top"),
        tok(OTkParagraphEnd),
        tok(OTkStmtListClose),
        tok(OTkListItemEnd),
        tok(OTkIndent),
          tok(OTkListDash, "-"),
            tok(OTkStmtListOpen),
              tok(OTkParagraphStart),
                tok(OTkWord, "inside"),
              tok(OTkParagraphEnd),
            tok(OTkStmtListClose),
          tok(OTkListItemEnd),
        tok(OTkDedent)
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
        tok(OTkListDash, "-"),

        tok(OTkStmtListOpen), tok(OTkParagraphStart),
        tok(OTkWord, "TOP0"),
        tok(OTkParagraphEnd), tok(OTkStmtListClose),

        tok(OTkListItemEnd, ""),
        tok(OTkIndent, ""),
          tok(OTkListDash, "-"),
          # Expanded from text
          tok(OTkStmtListOpen), tok(OTkParagraphStart),
          tok(OTkWord, "INDENT-1"),
          tok(OTkParagraphEnd), tok(OTkStmtListClose),

          tok(OTkListItemEnd),
          tok(OTkSameIndent),
          tok(OTkListDash, "-"),
          # Expanded
          tok(OTkStmtListOpen), tok(OTkParagraphStart),
          tok(OTkWord, "SAME-1"),
          tok(OTkParagraphEnd), tok(OTkStmtListClose),

          tok(OTkListItemEnd, ""),
          tok(OTkIndent, ""),
            tok(OTkListDash, "-"),
            # Expanded
            tok(OTkStmtListOpen), tok(OTkParagraphStart),
            tok(OTkWord, "NES-2"),
            tok(OTkParagraphEnd), tok(OTkStmtListClose),

            tok(OTkListItemEnd, ""),
          tok(OTkDedent, ""),
        tok(OTkDedent, ""),
        tok(OTkListDash, "-"),

        tok(OTkStmtListOpen), tok(OTkParagraphStart),
        tok(OTkWord, "TOP1"),
        tok(OTkParagraphEnd), tok(OTkStmtListClose),

        tok(OTkListItemEnd, ""),
        tok(OTkIndent, ""),
          tok(OTkListDash, "-"),

          # Paragraph expanded tokens
          tok(OTkStmtListOpen),
            tok(OTkParagraphStart),
            tok(OTkWord, "IND-1"),
            tok(OTkParagraphEnd),
            tok(OTkParagraphStart),
            tok(OTkBigIdent, "MULTILINE"),
            tok(OTkParagraphEnd),
          tok(OTkStmtListClose),

          tok(OTkListItemEnd, ""),
            tok(OTkIndent, ""),
            tok(OTkListDash, "-"),
            tok(OTkStmtListOpen),
              tok(OTkParagraphStart),
              tok(OTkWord, "NES-20"),
              tok(OTkParagraphEnd),

              tok(OTkCommandPrefix, "#+"),
              tok(OTkCommandBegin, "begin_src"),
              # arguments
              tok(OTkCommandArgumentsBegin),
              tok(OTkWord),
              tok(OTkCommandArgumentsEnd),
              # content
              tok(OTkCommandContentStart),
              tok(OTkCodeContentBegin),
              tok(OTkCodeText, "content"),
              tok(OTkCodeContentEnd),
              tok(OTkCommandContentEnd),

              tok(OTkCommandPrefix, "#+"),
              tok(OTkCommandEnd, "end_src"),
            tok(OTkStmtListClose),

            tok(OTkListItemEnd, ""),
            tok(OTkSameIndent, ""),
            tok(OTkListDash, "-"),

            tok(OTkStmtListOpen), tok(OTkParagraphStart),
            tok(OTkWord, "NES-21"),
            tok(OTkParagraphEnd), tok(OTkStmtListClose),

            tok(OTkListItemEnd, ""),
          tok(OTkDedent, ""),
          tok(OTkListDash, "-"),
            tok(OTkStmtListOpen), tok(OTkParagraphStart),
            tok(OTkBigIdent, "SEC"),
            tok(OTkParagraphEnd), tok(OTkStmtListClose),
          tok(OTkListItemEnd, ""),
        tok(OTkDedent, ""),
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
                      ast(orgCmdArguments, @[
                        ast(orgInlineStmtList),
                        ast(orgInlineStmtList)
                      ]),
                      stmt(ast(orgCodeLine, @[
                        ast(orgCodeText, "content")])),
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
        tok(OTkHashTag, "#tag"),
        tok(OTkHashTagSub, "#"),
        tok(OTkHashTag, "#sub"),
        tok(OTkHashTagSub, "#"),
          tok(OTkHashTagOpen, "#["),
            tok(OTkHashTag, "nested1"),
            tok(OTkComma, ","),
            # `nested2##sub##[t1, t2]`
            tok(OTkHashTag, "nested2"),
              tok(OTkHashTagSub, "#"),
              tok(OTkHashTag, "#sub"),
              # `##[t1, t2]`
              tok(OTkHashTagSub, "#"),
                tok(OTkHashTagOpen, "#["),
                  tok(OTkHashTag, "t1"),
                  tok(OTkComma, ","),
                  tok(OTkHashTag, "t2"),
                tok(OTkHashTagClose, "]"),
          tok(OTkHashTagClose, "]")
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
        tok(OTkSubtreeStars, "****"),
        tok(OTkSubtreeTodoState, "FAILED"),
        tok(OTkParagraphStart),
        tok(OTkBracketTime, "[2022-09-18 Sun 22:30:14]"),
        tok(OTkSpace, " "),
        tok(OTkWord, "from"),
        tok(OTkSpace, " "),
        tok(OTkMonospaceOpen, "~"),
        tok(OTkRawText, "notes.org:32410"),
        tok(OTkMonospaceClose, "~"),
        tok(OTkSpace, " "),

        # `(=5937E39D=)`
        tok(OTkParOpen, "("),
        tok(OTkVerbatimOpen, "="),
        tok(OTkRawText, "5937E39D"),
        tok(OTkVerbatimClose, "="),
        tok(OTkParClose, ")"),

        tok(OTkParagraphEnd),
        tok(OTkSubtreeTime, "CLOSED"),
        tok(OTkBracketTime, "[2022-09-18 Sun 22:31:12]"),
        tok(OTkColonProperties, ":PROPERTIES:"),
        tok(OTkColonIdent, ":CREATED:"),
        tok(OTkRawProperty, "[2022-09-18 Sun 22:30:14]"),
        tok(OTkColonIdent, ":ID:"),
        tok(OTkRawProperty, "8f4e3847-daf6-4bf5-affd-dcafca4ba410"),
        tok(OTkColonEnd, ":END:"),
        tok(OTkColonLogbook, ":LOGBOOK:"),
        tok(OTkLogbookStart),
        tok(OTkIndent),
        tok(OTkDedent),
        tok(OTkListDash, "-"),
        tok(OTkStmtListOpen),
        tok(OTkParagraphStart),
        tok(OTkWord, "State"),
        tok(OTkSpace, " "),
        tok(OTkQuoteOpen, "\""),
        tok(OTkBigIdent, "FAILED"),
        tok(OTkQuoteClose, "\""),
        tok(OTkSpace, "     "),
        tok(OTkWord, "from"),
        tok(OTkSpace, "              "),
        tok(OTkBracketTime, "[2022-09-18 Sun 22:31:12]"),
        tok(OTkSpace, " "),
        tok(OTkDoubleSlash, "\\\\"),
        tok(OTkNewline, "\n"),
        tok(OTkSpace, "       "),
        tok(OTkWord, "Failed"),
        tok(OTkSpace, " "),
        tok(OTkWord, "note"),
        tok(OTkNewline, "\n"),
        tok(OTkSpace, "     "),
        tok(OTkParagraphEnd),
        tok(OTkStmtListClose),
        tok(OTkListItemEnd),
        tok(OTkSameIndent),
        tok(OTkLogbookEnd),
        tok(OTkColonEnd, ":END:"),
        tok(OTkSubtreeEnd)
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

  test "Subtree extended properties":
    check runTest("""
* Extended properties
  :properties:
  :thing: value
  :thing+: value
  :end:
""", stmt(
      ast(orgSubtree, @[
        raw("*"),
        e(),
        e(),
        par(word("Extended"), space(" "), word("properties")),
        e(),
        e(),
        e(),
        ast(orgDrawer, @[
          ast(orgPropertyList, @[
            ast(orgProperty, @[raw(":thing:"), e(), raw("value")]),
            ast(orgPropertyAdd, @[raw(":thing+:"), e(), raw("value")])
          ]),
          e(),
        ]),
        stmt()
      ])
    ))

  test "Subtree multi-entry logbook":
    check runTest("""
* Title
  :LOGBOOK:
  - Refiled on [2022-07-03 Sun 00:40:47] from [[id:ID-T][Name]]
  CLOCK: [2022-07-03 Sun 08:38:19]--[2022-07-03 Sun 09:10:34] =>  0:32
  - State "WIP"        from "TODO"       [2022-07-03 Sun 08:38:19]
  - State "COMPLETED"  from "WIP"        [2022-07-03 Sun 09:10:34]
  CLOCK: [2022-07-03 Sun 08:38:19]
  CLOCK: [2022-07-03 Sun 08:38:19]--[2022-07-03 Sun 09:10:34]
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
              ),
              "note": e()
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
            }),
            ast(orgLogbookClock, {
              "time": time("[2022-07-03 Sun 08:38:19]")
            }),
            ast(orgLogbookClock, {
              "time": ast(orgTimeRange, {
                "from": time("[2022-07-03 Sun 08:38:19]"),
                "to": time("[2022-07-03 Sun 09:10:34]")
              })
            }),
          ])
        ]),
        stmt() # body
      ])
    ))
