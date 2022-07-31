import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/other/[hpprint, blockfmt],
  lex_all

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexStructure())


suite "Lex subtree":
  test "Simple subtree":
    let tokens = l("""
- list
  - indented
  - second item

- list2
  - indented2

    multiline text block
    - nested items

      #+caption: nested list *caption*
      #+begin_src
      content
      #+end_src

  - second item2""")

    for tok in tokens:
      echov tok.kind, tok

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
