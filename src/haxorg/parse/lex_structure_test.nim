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
  - second item2""")

    let blc = ppblock(
      tokens,
      pconf(
        ignorePaths = matchField("baseStr", "extra", "isSlice"),
        forceLayouts = @{ matchType("OrgStructure"): forceLine() },
        extraFields = @[
          pprintExtraField(
            OrgToken,
            "strVal",
            newPPrintConst(
              "\"" & it.strVal() & "\"",
              fgYellow + bgDefault))]))

    # writeFile("/tmp/zz", blc.pyCodegenRepr(indent = 2, nimpref = "make"))
    # echo toString(blc)
