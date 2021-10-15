import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  haxorg/parse/parse_org_text

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

  test "Inline link":
    check:
      matchdiff @(kind, strVal), [
        l("[[code:macro!matchdiff]]"): [
          (ottLinkOpen),
          (ottLinkTargetOpen),
          (ottRawText, "code:macro!matchdiff"),
          (ottLinkTargetClose),
          (ottLinkClose)
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
