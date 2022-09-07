import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  lex_all

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
          (OStSubtreeStars, "***"),
          (OStSubtreeImportance, "[#A]"),
          (OStText, "Hello")
        ],
        l("*** COMPLETED subtree name 18:00-21:00"): [
          (OStSubtreeStars, "***"),
          (OStSubtreeTodoState, "COMPLETED"),
          # more detailed parsing of the subtree content will be done when
          # text token is parsed
          (OStText, "subtree name 18:00-21:00")
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
        (OStSubtreeStars, "***") ,
        (OStSubtreeTodoState, "COMPLETED"),
        (OStText, "Tokens 18:00-21:00"),
        (OStSubtreeTime, "CLOSED"),
        (OStBracketTime, "[2022-07-15 Fri 23:57:36]"),
        (OStColonProperties, ":PROPERTIES:"),
        (OStColonIdent, ":ID:"),
        (OStRawProperty, "97af3d5c-8ddc-408e-a665-822f16db051e"),
        (OStColonEnd, ":END:"),
        (OStColonLogbook, ":LOGBOOK:"),
        (OStRawLogbook, """
    - Refiled on [2022-07-06 Wed 00:03:53] from [[file:inbox.org][inbox:inbox.org]]
    - Refiled on [2022-07-12 Tue 13:18:07] from [[id:6fa0500c-80ca-43d2-a894-c71452b55ef5][main:Node]]
    - State "COMPLETED"  from "TODO"       [2022-07-15 Fri 23:57:36]
    :END:"""),
        (OStColonEnd, ":END:"),
        (OStColonProperties, ":PROPERTIES:"),
        (OStColonIdent, ":CREATED:"),
        (OStRawProperty, "[2022-05-22 Sun 17:44:21]"),
        (OStColonIdent, ":ID:"),
        (OStRawProperty, "d3cb6ab8-ac9a-45bd-912e-d86e26908d64"),
        (OStColonAddIdent, ":ID+:"),
        (OStRawProperty, "VALUE"),
        (OStColonEnd, ":END:"),
      ]
    ]

suite "lex commands":
  test "caption":
    check:
      matchdiff @(kind, strVal), [
        l("#+caption: *bold*"): [
          (OStCommandPrefix, "#+"),
          (OStLineCommand, "caption"),
          (OStColon, ":"),
          (OStCommandArgumentsBegin),
          (OStText, "*bold*"),
          (OStCommandArgumentsEnd)
        ],
        l("#+begin_quote\ntest\n#+end_quote"): [
          (OStCommandPrefix, "#+"),
          (OStCommandBegin, "begin_quote"),
          (OStCommandArgumentsBegin),
          (OStCommandArgumentsEnd),
          (OStCommandContentStart),
          (OStText, "test"),
          (OStCommandContentEnd),
          (OStCommandPrefix, "#+"),
          (OStCommandEnd, "end_quote")
        ],
        l("""
#+begin: NAME PARAMETERS
CONTENTS
#+end:"""): [
          (OStCommandPrefix, "#+"),
          (OStCommandBegin, "begin"),
          (OStCommandArgumentsBegin, ""),
          (OTxRawText, "NAME PARAMETERS"),
          (OStCommandArgumentsEnd, ""),
          (OStCommandContentStart, ""),
          (OStText, "CONTENTS"),
          (OStCommandContentEnd, ""),
          (OStCommandPrefix, "#+"),
          (OStCommandEnd, "end"),
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
          (OStListDash, "-"),
          (OStText, "TOP #0\n"),
          (OStListItemEnd, ""),
          (OStIndent, ""),
            (OStListDash, "-"),
            (OStText, "INDENT-1\n"),
            (OStListItemEnd, ""),
            (OStSameIndent, ""),
            (OStListDash, "-"),
            (OStText, "SAME-1\n"),
            (OStListItemEnd, ""),
            (OStIndent, ""),
              (OStListDash, "-"),
              (OStText, "NES-2\n"),
              (OStListItemEnd, ""),
            (OStDedent, ""),
          (OStDedent, ""),
          (OStListDash, "-"),
          (OStText, "TOP #1\n"),
          (OStListItemEnd, ""),
          (OStIndent, ""),
            (OStListDash, "-"),
            (OStText, "IND-1\n\n    MULTILINE\n"),
            (OStListItemEnd, ""),
              (OStIndent, ""),
              (OStListDash, "-"),
              (OStText, """NES-2 #0

      #+begin_src
      content
      #+end_src
"""),
              (OStListItemEnd, ""),
              (OStSameIndent, ""),
              (OStListDash, "-"),
              (OStText, "NES-2 #1\n"),
              (OStListItemEnd, ""),
            (OStDedent, ""),
            (OStListDash, "-"),
            (OStText, "SEC"),
            (OStListItemEnd, ""),
          (OStDedent, ""),
        ]
      ]

    # for tok in tokens:
    #   echo &"({tok.kind}, \"{tok.strVal()}\"),"
