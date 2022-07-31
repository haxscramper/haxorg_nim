import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base, clformat],
  hmisc/types/colorstring,
  hmisc/other/[hpprint, blockfmt],
  lex_all

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexStructure())


suite "Lex subtree":
  test "Simple subtree":
#     let tokens = l("""
# - N0#0
#   - N1
#     - N2#0
#     - N2#1
# - N0#1
# """)
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

    for tok in tokens:
      echo hshow(tok.kind) |<< 16, hshow(tok.strVal())

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
