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
        matchdiff @(kind), [
          l("word"): [(ottWord, strVal: "word")],
          l("*bold*"): [
            (ottBoldOpen,  strVal: "*"),
            (ottWord,      strVal: "bold"),
            (ottBoldClose, strVal: "*")
          ],
          l("/italic/"):    [(ottItalicOpen),    _, (ottItalicClose)],
          l("_underline_"): [(ottUnderlineOpen), _, (ottUnderlineClose)],
          l("+strike+"):    [(ottStrikeOpen),    _, (ottStrikeClose)],
          l("`backtick`"):  [(ottBacktickOpen),  _, (ottBacktickClose)],
          l("~verb~"):      [(ottMonospaceOpen), _, (ottMonospaceClose)],
          l("**B**old"): [
            (ottBoldInline, strVal: "**"),
            (ottBigIdent,   strVal: "B"),
            (ottBoldInline, strVal: "**"),
            (ottWord,       strVal: "old")
          ],
          l("//IT//alic"): [
            (ottItalicInline, strVal: "//"),
            (ottBigIdent,     strVal: "IT"),
            (ottItalicInline, strVal: "//"),
            (ottWord,         strVal: "alic")
          ],
          l("~not *bold*~"): [
            (ottMonospaceOpen,  strVal: "~"),
            (ottRawText,        strVal: "not *bold*"),
            (ottMonospaceClose, strVal: "~")
          ],
          l("~~**MONOSPACE NOT BOLD**~~"): [
            (ottMonospaceInline, strVal: "~~"),
            (ottRawText,         strVal: "**MONOSPACE NOT BOLD**"),
            (ottMonospaceInline, strVal: "~~")
          ],
          l("**~~BOLD MONOSPACE~~**"): [
            (ottBoldInline,      strVal: "**"),
            (ottMonospaceInline, strVal: "~~"),
            (ottRawText,         strVal: "BOLD MONOSPACE"),
            (ottMonospaceInline, strVal: "~~"),
            (ottBoldInline,      strVal: "**")
          ]
        ]
