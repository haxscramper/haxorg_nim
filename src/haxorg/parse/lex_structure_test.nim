import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base, clformat],
  hmisc/types/colorstring,
  hmisc/other/[hpprint, blockfmt],
  lex_all

import std/strformat

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
          (ostSubtreeStars, "***"),
          (ostSubtreeImportance, "[#A]"),
          (ostText, "Hello")
        ],
        l("*** COMPLETED subtree name 18:00-21:00"): [
          (ostSubtreeStars, "***"),
          (ostSubtreeTodoState, "COMPLETED"),
          # more detailed parsing of the subtree content will be done when
          # text token is parsed
          (ostText, "subtree name 18:00-21:00")
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
    :END:
""")

    check: matchdiff @(kind, strVal), [
      tokens: [
        (ostSubtreeStars, "***") ,
        (ostSubtreeTodoState, "COMPLETED"),
        (ostText, "Tokens 18:00-21:00"),
        (ostSubtreeTime, "CLOSED"),
        (ostBracketTime, "[2022-07-15 Fri 23:57:36]"),
        (ostColonProperties, ":PROPERTIES:"),
        (ostColonIdent, ":ID:"),
        (ostRawProperty, "97af3d5c-8ddc-408e-a665-822f16db051e"),
        (ostColonEnd, ":END:"),
        (ostColonLogbook, ":LOGBOOK:"),
        (ostRawLogbook, """
    - Refiled on [2022-07-06 Wed 00:03:53] from [[file:inbox.org][inbox:inbox.org]]
    - Refiled on [2022-07-12 Tue 13:18:07] from [[id:6fa0500c-80ca-43d2-a894-c71452b55ef5][main:Node]]
    - State "COMPLETED"  from "TODO"       [2022-07-15 Fri 23:57:36]
    :END:"""),
        (ostColonEnd, ":END:"),
        (ostColonProperties, ":PROPERTIES:"),
        (ostColonIdent, ":CREATED:"),
        (ostRawProperty, "[2022-05-22 Sun 17:44:21]"),
        (ostColonIdent, ":ID:"),
        (ostRawProperty, "d3cb6ab8-ac9a-45bd-912e-d86e26908d64"),
        (ostColonEnd, ":END:"),
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
          (ostListDash, "-"),
          (ostText, "TOP #0\n"),
          (ostListItemEnd, ""),
          (ostIndent, ""),
            (ostListDash, "-"),
            (ostText, "INDENT-1\n"),
            (ostListItemEnd, ""),
            (ostSameIndent, ""),
            (ostListDash, "-"),
            (ostText, "SAME-1\n"),
            (ostListItemEnd, ""),
            (ostIndent, ""),
              (ostListDash, "-"),
              (ostText, "NES-2\n"),
              (ostListItemEnd, ""),
            (ostDedent, ""),
          (ostDedent, ""),
          (ostListDash, "-"),
          (ostText, "TOP #1\n"),
          (ostListItemEnd, ""),
          (ostIndent, ""),
            (ostListDash, "-"),
            (ostText, "IND-1\n\n    MULTILINE\n"),
            (ostListItemEnd, ""),
              (ostIndent, ""),
              (ostListDash, "-"),
              (ostText, """NES-2 #0

      #+begin_src
      content
      #+end_src
"""),
              (ostListItemEnd, ""),
              (ostSameIndent, ""),
              (ostListDash, "-"),
              (ostText, "NES-2 #1\n"),
              (ostListItemEnd, ""),
            (ostDedent, ""),
            (ostListDash, "-"),
            (ostText, "SEC"),
            (ostListItemEnd, ""),
          (ostDedent, ""),
        ]
      ]

    # for tok in tokens:
    #   echo &"({tok.kind}, \"{tok.strVal()}\"),"

      # echo hshow(tok.kind) |<< 16, hshow(tok.strVal())

    # let blc = ppblock(
    #   tokens,
    #   pconf(
    #     ignorePaths = matchField("baseStr", "extra", "isSlice"),
    #     forceLayouts = @{ matchType("OrgStructure"): forceLine() },
    #     extraFields = @[
    #       pprintExtraField(
    #         OrgToken,
    #         "strVal",
    #         newPPrintConst(
    #           "\"" & it.strVal() & "\"",
    #           fgYellow + bgDefault))]))

    # writeFile("/tmp/zz", blc.pyCodegenRepr(indent = 2, nimpref = "make"))
    # echo toString(blc)
