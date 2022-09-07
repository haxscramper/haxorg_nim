import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  lex_all

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexGlobal())

suite "full lists":
  test "with links":
    let tokens = l("""
- list item with [[file:relative.txt][description *bold*]]
  - nested item
""")

    # for tok in tokens:
    #   echov tok
