import hmisc/preludes/unittest
import hmisc/algo/[hlex_base, hparse_base]
import lex_all

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexText)

suite "WIP test":
  echo l("_underline_")

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

suite "Lex embedded structures":
  test "Macros":
    check:
      matchdiff @(kind, strVal), [
        l("{{{test}}}"): [
          (ottMacroOpen, "{{{"),
          (ottMacroBody, "test"),
          (ottMacroClose, "}}}")
        ]
      ]

  test "Inline source code":
    check:
      matchdiff @(kind, strVal), [
        l("src_nim{echo 12}"): [
          (ottSrcOpen, "src"),
          (ottSrcName, "nim"),
          (ottSrcBody, "echo 12"),
          (ottSrcClose)
        ],
        l("""src_sh{echo "ee\}"}"""): [
          (ottSrcOpen, "src"),
          (ottSrcName, "sh"),
          (ottSrcBody, "echo \"ee\\}\""),
          (ottSrcClose)
        ],
        l("src_sh[:exports both]{echo 12} {{{results(=12=)}}}"): [
          (ottSrcOpen, "src"),
          (ottSrcName, "sh"),
          (ottSrcArgs, ":exports both"),
          (ottSrcBody, "echo 12"),
          (ottSrcClose, ""),
          (ottSpace, " "),
          (ottMacroOpen, "{{{"),
          (ottMacroBody, "results(=12=)"),
          (ottMacroClose, "}}}")
        ]
      ]



  test "Inline call":
    check:
      matchdiff @(kind, strVal), [
        l("call_test[:session special](arg=12)"): [
          (ottCallOpen, "call"),
          (ottCallName, "test"),
          (ottCallInsideHeader, ":session special"),
          (ottCallArgs, "arg=12"),
          (ottCallClose)
        ]
      ]

  test "general brackets":
    check:
      matchdiff @(kind, strVal), [
        l("[[code:macro!matchdiff]]"): [
          (ottLinkOpen),
          (ottLinkTargetOpen),
          (ottRawText, "code:macro!matchdiff"),
          (ottLinkTargetClose),
          (ottLinkClose)
        ],
        l("word [1934-02-02] after"): [
          (ottWord, "word"),
          (ottSpace),
          (ostBracketTime, "[1934-02-02]"),
          (ottSpace),
          (ottWord, "after"),
        ],
        l("pref [1962-11-16]--[1962-11-28]"): [
          (ottWord, "pref"),
          (ottSpace),
          (ostBracketTime, "[1962-11-16]"),
          (ostTimeDash, "--"),
          (ostBracketTime, "[1962-11-28]"),
        ],
        l("pref <%%(diary-block-d 2022 7 4 17)>"): [
          (ottWord, "pref"),
          (ottSpace),
          (ostDiaryTime, "<%%(diary-block-d 2022 7 4 17)>")
        ]
      ]

  test "Metatags":
    check:
      matchdiff @(kind, strVal), [
        l("@arg{test}"): [
          (ottMetaOpen),
          (ottMetaName, "arg"),
          (ottMetaBody, "test"),
          (ottMetaClose)
        ],
        l("@arg{test}{test2}"): [
          (ottMetaOpen),
          (ottMetaName, "arg"),
          (ottMetaBody, "test"),
          (ottMetaBody, "test2"),
          (ottMetaClose)
        ],
        l("@edsl{{<ch1>, <ch2>, ...}}"): [
          (ottMetaOpen),
          (ottMetaName, "edsl"),
          (ottMetaBody, "{<ch1>, <ch2>, ...}"),
          (ottMetaClose)
        ],
      ]
