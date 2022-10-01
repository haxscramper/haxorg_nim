import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  haxorg/lexer

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexStructure())


suite "lex subtrees":
  test "basic subtree":
    check:
      matchdiff @(kind, strVal), [
        l("*** [#A] Hello"): [
          (OTkSubtreeStars, "***"),
          (OTkSubtreeUrgency, "[#A]"),
          (OTkText, "Hello"),
          (OTkSubtreeEnd)
        ],
        l("*** COMPLETED subtree name 18:00-21:00"): [
          (OTkSubtreeStars, "***"),
          (OTkSubtreeTodoState, "COMPLETED"),
          # more detailed parsing of the subtree content will be done when
          # text token is parsed
          (OTkText, "subtree name 18:00-21:00"),
          (OTkSubtreeEnd)
        ]
      ]

  test "drawer":
    let tokens = l("""
*** COMPLETED Tokens 18:00-21:00
    CLOSED: [2022-07-15 Fri 23:57:36]
    :PROPERTIES:
    :ID:       97af3d5c-8ddc-408e-a665-822f16db051e
    :END:
    :LOGBOOK:
    - Refiled on [2022-07-06 Wed 00:03:53] from [[file:inbox.org][inbox:inbox.org]]
    - Refiled on [2022-07-12 Tue 13:18:07] from [[id:6fa0500c-80ca-43d2-a894-c71452b55ef5][main:Node]]
    - State "COMPLETED"  from "TODO"       [2022-07-15 Fri 23:57:36]
    :END:
    :PROPERTIES:
    :CREATED: [2022-05-22 Sun 17:44:21]
    :ID: d3cb6ab8-ac9a-45bd-912e-d86e26908d64
    :ID+: VALUE
    :END:
""")

    check: matchdiff @(kind, strVal), [
      tokens: [
        (OTkSubtreeStars, "***") ,
        (OTkSubtreeTodoState, "COMPLETED"),
        (OTkText, "Tokens 18:00-21:00"),
        (OTkSubtreeTime, "CLOSED"),
        (OTkBracketTime, "[2022-07-15 Fri 23:57:36]"),
        (OTkColonProperties, ":PROPERTIES:"),
        (OTkColonIdent, ":ID:"),
        (OTkRawProperty, "97af3d5c-8ddc-408e-a665-822f16db051e"),
        (OTkColonEnd, ":END:"),
        (OTkColonLogbook, ":LOGBOOK:"),
        (OTkRawLogbook, """
    - Refiled on [2022-07-06 Wed 00:03:53] from [[file:inbox.org][inbox:inbox.org]]
    - Refiled on [2022-07-12 Tue 13:18:07] from [[id:6fa0500c-80ca-43d2-a894-c71452b55ef5][main:Node]]
    - State "COMPLETED"  from "TODO"       [2022-07-15 Fri 23:57:36]
    """),
        (OTkColonEnd, ":END:"),
        (OTkColonProperties, ":PROPERTIES:"),
        (OTkColonIdent, ":CREATED:"),
        (OTkRawProperty, "[2022-05-22 Sun 17:44:21]"),
        (OTkColonIdent, ":ID:"),
        (OTkRawProperty, "d3cb6ab8-ac9a-45bd-912e-d86e26908d64"),
        (OTkColonAddIdent, ":ID+:"),
        (OTkRawProperty, "VALUE"),
        (OTkColonEnd, ":END:"),
        (OTkSubtreeEnd)
      ]
    ]

suite "lex commands":
  test "caption":
    check:
      matchdiff @(kind, strVal), [
        l("#+caption: *bold*"): [
          (OTkCommandPrefix, "#+"),
          (OTkLineCommand, "caption"),
          (OTkColon, ":"),
          (OTkCommandArgumentsBegin),
          (OTkText, "*bold*"),
          (OTkCommandArgumentsEnd)
        ],
        l("#+begin_quote\ntest\n#+end_quote"): [
          (OTkCommandPrefix, "#+"),
          (OTkCommandBegin, "begin_quote"),
          (OTkCommandArgumentsBegin),
          (OTkCommandArgumentsEnd),
          (OTkCommandContentStart),
          (OTkText, "test"),
          (OTkCommandContentEnd),
          (OTkCommandPrefix, "#+"),
          (OTkCommandEnd, "end_quote")
        ],
        l("""
#+begin: NAME PARAMETERS
CONTENTS
#+end:"""): [
          (OTkCommandPrefix, "#+"),
          (OTkCommandBegin, "begin"),
          (OTkCommandArgumentsBegin, ""),
          (OTkRawText, "NAME PARAMETERS"),
          (OTkCommandArgumentsEnd, ""),
          (OTkCommandContentStart, ""),
          (OTkText, "CONTENTS"),
          (OTkCommandContentEnd, ""),
          (OTkCommandPrefix, "#+"),
          (OTkCommandEnd, "end"),
        ]
      ]




suite "Lex lists":
  test "Full input lexing":
    let tokens = l("""
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
  - SEC""")

    check:
      matchdiff @(kind, strVal), [
        tokens: [
          (OTkListDash, "-"),
          (OTkStmtList, "TOP #0\n"),
          (OTkListItemEnd, ""),
          (OTkIndent, ""),
            (OTkListDash, "-"),
            (OTkStmtList, "INDENT-1\n"),
            (OTkListItemEnd, ""),
            (OTkSameIndent, ""),
            (OTkListDash, "-"),
            (OTkStmtList, "SAME-1\n"),
            (OTkListItemEnd, ""),
            (OTkIndent, ""),
              (OTkListDash, "-"),
              (OTkStmtList, "NES-2\n"),
              (OTkListItemEnd, ""),
            (OTkDedent, ""),
          (OTkDedent, ""),
          (OTkListDash, "-"),
          (OTkStmtList, "TOP #1\n"),
          (OTkListItemEnd, ""),
          (OTkIndent, ""),
            (OTkListDash, "-"),
            (OTkStmtList, "IND-1\n\n    MULTILINE\n"),
            (OTkListItemEnd, ""),
              (OTkIndent, ""),
              (OTkListDash, "-"),
              (OTkStmtList, """NES-2 #0

      #+begin_src
      content
      #+end_src
"""),
              (OTkListItemEnd, ""),
              (OTkSameIndent, ""),
              (OTkListDash, "-"),
              (OTkStmtList, "NES-2 #1\n"),
              (OTkListItemEnd, ""),
            (OTkDedent, ""),
            (OTkListDash, "-"),
            (OTkStmtList, "SEC"),
            (OTkListItemEnd, ""),
          (OTkDedent, ""),
        ]
      ]

    # for tok in tokens:
    #   echo &"({tok.kind}, \"{tok.strVal()}\"),"
