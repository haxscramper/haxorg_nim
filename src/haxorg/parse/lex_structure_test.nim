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
