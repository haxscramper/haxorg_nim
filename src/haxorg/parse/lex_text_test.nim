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
          l("word"): [(OTxWord, strVal: "word")],
          l("*bold*"): [
            (OTxBoldOpen,  strVal: "*"),
            (OTxWord,      strVal: "bold"),
            (OTxBoldClose, strVal: "*")
          ],
          l("/italic/"):    [(OTxItalicOpen),    _, (OTxItalicClose)],
          l("_underline_"): [(OTxUnderlineOpen), _, (OTxUnderlineClose)],
          l("+strike+"):    [(OTxStrikeOpen),    _, (OTxStrikeClose)],
          l("`backtick`"):  [(OTxBacktickOpen),  _, (OTxBacktickClose)],
          l("~verb~"):      [(OTxMonospaceOpen), _, (OTxMonospaceClose)],
          l("**B**old"): [
            (OTxBoldInline, strVal: "**"),
            (OTxBigIdent,   strVal: "B"),
            (OTxBoldInline, strVal: "**"),
            (OTxWord,       strVal: "old")
          ],
          l("//IT//alic"): [
            (OTxItalicInline, strVal: "//"),
            (OTxBigIdent,     strVal: "IT"),
            (OTxItalicInline, strVal: "//"),
            (OTxWord,         strVal: "alic")
          ],
          l("~not *bold*~"): [
            (OTxMonospaceOpen,  strVal: "~"),
            (OTxRawText,        strVal: "not *bold*"),
            (OTxMonospaceClose, strVal: "~")
          ],
          l("~~**MONOSPACE NOT BOLD**~~"): [
            (OTxMonospaceInline, strVal: "~~"),
            (OTxRawText,         strVal: "**MONOSPACE NOT BOLD**"),
            (OTxMonospaceInline, strVal: "~~")
          ],
          l("**~~BOLD MONOSPACE~~**"): [
            (OTxBoldInline,      strVal: "**"),
            (OTxMonospaceInline, strVal: "~~"),
            (OTxRawText,         strVal: "BOLD MONOSPACE"),
            (OTxMonospaceInline, strVal: "~~"),
            (OTxBoldInline,      strVal: "**")
          ]
        ]

suite "Lex embedded structures":
  test "Macros":
    check:
      matchdiff @(kind, strVal), [
        l("{{{test}}}"): [
          (OTxMacroOpen, "{{{"),
          (OTxMacroBody, "test"),
          (OTxMacroClose, "}}}")
        ]
      ]

  test "Inline source code":
    check:
      matchdiff @(kind, strVal), [
        l("src_nim{echo 12}"): [
          (OTxSrcOpen, "src"),
          (OTxSrcName, "nim"),
          (OTxSrcBody, "echo 12"),
          (OTxSrcClose)
        ],
        l("""src_sh{echo "ee\}"}"""): [
          (OTxSrcOpen, "src"),
          (OTxSrcName, "sh"),
          (OTxSrcBody, "echo \"ee\\}\""),
          (OTxSrcClose)
        ],
        l("src_sh[:exports both]{echo 12} {{{results(=12=)}}}"): [
          (OTxSrcOpen, "src"),
          (OTxSrcName, "sh"),
          (OTxSrcArgs, ":exports both"),
          (OTxSrcBody, "echo 12"),
          (OTxSrcClose, ""),
          (OTxSpace, " "),
          (OTxMacroOpen, "{{{"),
          (OTxMacroBody, "results(=12=)"),
          (OTxMacroClose, "}}}")
        ]
      ]



  test "Inline call":
    check:
      matchdiff @(kind, strVal), [
        l("call_test[:session special](arg=12)"): [
          (OTxCallOpen, "call"),
          (OTxCallName, "test"),
          (OTxCallInsideHeader, ":session special"),
          (OTxCallArgs, "arg=12"),
          (OTxCallClose)
        ]
      ]

  test "general brackets":
    check:
      matchdiff @(kind, strVal), [
        l("[[code:macro!matchdiff]]"): [
          (OTxLinkOpen),
          (OTxLinkTargetOpen),
          (OTxRawText, "code:macro!matchdiff"),
          (OTxLinkTargetClose),
          (OTxLinkClose)
        ],
        l("word [1934-02-02] after"): [
          (OTxWord, "word"),
          (OTxSpace),
          (OStBracketTime, "[1934-02-02]"),
          (OTxSpace),
          (OTxWord, "after"),
        ],
        l("pref [1962-11-16]--[1962-11-28]"): [
          (OTxWord, "pref"),
          (OTxSpace),
          (OStBracketTime, "[1962-11-16]"),
          (OStTimeDash, "--"),
          (OStBracketTime, "[1962-11-28]"),
        ],
        l("pref <%%(diary-block-d 2022 7 4 17)>"): [
          (OTxWord, "pref"),
          (OTxSpace),
          (OStDiaryTime, "<%%(diary-block-d 2022 7 4 17)>")
        ]
      ]

  test "link protocols":
    check:
      matchdiff @(kind, strVal), [
        l("[[http://staff.science.uva.nl/c.dominik/]]"): [
          (OTxLinkOpen),
          (OTxLinkTargetOpen),
          (OTxLinkFull, "http://staff.science.uva.nl/c.dominik/"),
          (OTxLinkTargetClose),
          (OTxLinkClose)
        ],
        l("[[doi:10.1000/182]]"): [
          _, _, (OTxLinkProtocol, "doi"), (OTxLinkTarget, "10.1000/182"), _, _
        ],
         l("[[file:/home/images/jupiter.jpg]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "/home/images/jupiter.jpg"), _, _
        ],
        l("[[/home/images/jupiter.jpg]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "/home/images/jupiter.jpg"), _, _
        ],
        l("[[file:papers/last.pdf]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "papers/last.pdf"), _, _
        ],
        l("[[./papers/last.pdf]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "./papers/last.pdf"), _, _
        ],
        l("[[file:/ssh:me@some.where:papers/last.pdf]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "/ssh:me@some.where:papers/last.pdf"), _, _
        ],
        l("[[/ssh:me@some.where:papers/last.pdf]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "/ssh:me@some.where:papers/last.pdf"), _, _
        ],
        l("[[file:sometextfile::NNN]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "sometextfile"),
          (OTxLinkExtraSeparator, "::"), (OTxLinkExtra, "NNN"), _, _
        ],
        l("[[file:projects.org]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "projects.org"), _, _
        ],
        l("[[file:projects.org::some words]]"): [
          _, _,
          (OTxLinkProtocol, "file"),
          (OTxLinkTarget, "projects.org"),
          (OTxLinkExtraSeparator, "::"),
          (OTxLinkExtra, "some words"), _, _
        ],
        l("[[attachment:projects.org]]"): [
          _, _,
          (OTxLinkProtocol, "attachment"),
          (OTxLinkTarget, "projects.org"), _, _
        ],
        l("[[id:B7423F4D-2E8A-471B-8810-C40F074717E9]]"): [
          _, _,
          (OTxLinkProtocol, "id"),
          (OTxLinkTarget, "B7423F4D-2E8A-471B-8810-C40F074717E9"), _, _
        ],
        l("[[shell:ls *.org]]"): [
          _, _,
          (OTxLinkProtocol, "shell"),
          (OTxLinkTarget, "ls *.org"), _, _
        ],
        l("[[target]]"): [
          _, _, (OTxLinkInternal, "target"), _, _
        ]
      ]

  test "Metatags":
    check:
      matchdiff @(kind, strVal), [
        l("@arg{test}"): [
          (OTxMetaOpen),
          (OTxMetaName, "arg"),
          (OTxMetaBody, "test"),
          (OTxMetaClose)
        ],
        l("@arg{test}{test2}"): [
          (OTxMetaOpen),
          (OTxMetaName, "arg"),
          (OTxMetaBody, "test"),
          (OTxMetaBody, "test2"),
          (OTxMetaClose)
        ],
        l("@edsl{{<ch1>, <ch2>, ...}}"): [
          (OTxMetaOpen),
          (OTxMetaName, "edsl"),
          (OTxMetaBody, "{<ch1>, <ch2>, ...}"),
          (OTxMetaClose)
        ],
      ]
