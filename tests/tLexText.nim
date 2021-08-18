import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  haxorg/parse/parse_org_text


template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexText)

suite "Lex regular text":
  test "Markup":
    block word:
      check:
        matchdiff l("word"), [
          (kind: ottWord)
        ]

when false:
  "*bold*"
  "/italic/"
  "_underline_"
  "+strike+"
  "`backtick`"
  "~verb~"
  "**B**old"
  "//IT//alic"
  "~not *bold*~"
  "~~**VERBATIM NOT BOLD**~~"
  "**~~BOLD VERBATIM~~**"
