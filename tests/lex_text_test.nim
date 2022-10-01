import hmisc/preludes/unittest
import hmisc/algo/[hlex_base, hparse_base]
import haxorg/lexer

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
          l("word"): [(OTkWord, strVal: "word")],
          l("*bold*"): [
            (OTkBoldOpen,  strVal: "*"),
            (OTkWord,      strVal: "bold"),
            (OTkBoldClose, strVal: "*")
          ],
          l("/italic/"):    [(OTkItalicOpen),    _, (OTkItalicClose)],
          l("_underline_"): [(OTkUnderlineOpen), _, (OTkUnderlineClose)],
          l("+strike+"):    [(OTkStrikeOpen),    _, (OTkStrikeClose)],
          l("`backtick`"):  [(OTkBacktickOpen),  _, (OTkBacktickClose)],
          l("~verb~"):      [(OTkMonospaceOpen), _, (OTkMonospaceClose)],
          l("**B**old"): [
            (OTkBoldInline, strVal: "**"),
            (OTkBigIdent,   strVal: "B"),
            (OTkBoldInline, strVal: "**"),
            (OTkWord,       strVal: "old")
          ],
          l("//IT//alic"): [
            (OTkItalicInline, strVal: "//"),
            (OTkBigIdent,     strVal: "IT"),
            (OTkItalicInline, strVal: "//"),
            (OTkWord,         strVal: "alic")
          ],
          l("~not *bold*~"): [
            (OTkMonospaceOpen,  strVal: "~"),
            (OTkRawText,        strVal: "not *bold*"),
            (OTkMonospaceClose, strVal: "~")
          ],
          l("~~**MONOSPACE NOT BOLD**~~"): [
            (OTkMonospaceInline, strVal: "~~"),
            (OTkRawText,         strVal: "**MONOSPACE NOT BOLD**"),
            (OTkMonospaceInline, strVal: "~~")
          ],
          l("**~~BOLD MONOSPACE~~**"): [
            (OTkBoldInline,      strVal: "**"),
            (OTkMonospaceInline, strVal: "~~"),
            (OTkRawText,         strVal: "BOLD MONOSPACE"),
            (OTkMonospaceInline, strVal: "~~"),
            (OTkBoldInline,      strVal: "**")
          ]
        ]

suite "Lex embedded structures":
  test "Macros":
    check:
      matchdiff @(kind, strVal), [
        l("{{{test}}}"): [
          (OTkMacroOpen, "{{{"),
          (OTkMacroName, "test"),
          (OTkMacroClose, "}}}")
        ]
      ]

  test "Inline source code":
    check:
      matchdiff @(kind, strVal), [
        l("src_nim{echo 12}"): [
          (OTkSrcOpen, "src"),
          (OTkSrcName, "nim"),
          (OTkSrcBody, "echo 12"),
          (OTkSrcClose)
        ],
        l("""src_sh{echo "ee\}"}"""): [
          (OTkSrcOpen, "src"),
          (OTkSrcName, "sh"),
          (OTkSrcBody, "echo \"ee\\}\""),
          (OTkSrcClose)
        ],
        l("src_sh[:exports both]{echo 12} {{{results(=12=)}}}"): [
          (OTkSrcOpen, "src"),
          (OTkSrcName, "sh"),
          (OTkSrcArgs, ":exports both"),
          (OTkSrcBody, "echo 12"),
          (OTkSrcClose, ""),
          (OTkSpace, " "),
          (OTkMacroOpen, "{{{"),
          (OTkMacroName, "results"),
          (OTkParOpen),
          (OTkMacroArg, "=12="),
          (OTkParClose),
          (OTkMacroClose, "}}}")
        ]
      ]



  test "Inline call":
    check:
      matchdiff @(kind, strVal), [
        l("call_test[:session special](arg=12)"): [
          (OTkCallOpen, "call"),
          (OTkCallName, "test"),
          (OTkCallInsideHeader, ":session special"),
          (OTkCallArgs, "arg=12"),
          (OTkCallClose)
        ]
      ]

  test "general brackets":
    check:
      matchdiff @(kind, strVal), [
        l("[[code:macro!matchdiff]]"): [
          (OTkLinkOpen),
          (OTkLinkTargetOpen),
          (OTkLinkProtocol, "code"),
          (OTkLinkTarget, "macro!matchdiff"),
          (OTkLinkTargetClose),
          (OTkLinkClose)
        ],
        l("word [1934-02-02] after"): [
          (OTkWord, "word"),
          (OTkSpace),
          (OTkBracketTime, "[1934-02-02]"),
          (OTkSpace),
          (OTkWord, "after"),
        ],
        l("pref [1962-11-16]--[1962-11-28]"): [
          (OTkWord, "pref"),
          (OTkSpace),
          (OTkBracketTime, "[1962-11-16]"),
          (OTkTimeDash, "--"),
          (OTkBracketTime, "[1962-11-28]"),
        ],
        l("pref <%%(diary-block-d 2022 7 4 17)>"): [
          (OTkWord, "pref"),
          (OTkSpace),
          (OTkDiaryTime, "<%%(diary-block-d 2022 7 4 17)>")
        ],
        l("[fn:name]"): [
          (OTkFootnoteStart, "["),
          (OTkColon, ":"),
          (OTkIdent, "name"),
          (OTkFootnoteEnd, "]")
        ],
        l("[fn::inline]"): [
          (OTkFootnoteStart, "["),
          (OTkDoubleColon, "::"),
          (OTkText, "inline"),
          (OTkFootnoteEnd)
        ]
      ]

  test "link protocols":
    check:
      matchdiff @(kind, strVal), [
        l("[[http://staff.science.uva.nl/c.dominik/]]"): [
          (OTkLinkOpen),
          (OTkLinkTargetOpen),
          (OTkLinkFull, "http://staff.science.uva.nl/c.dominik/"),
          (OTkLinkTargetClose),
          (OTkLinkClose)
        ],
        l("[[doi:10.1000/182]]"): [
          _, _, (OTkLinkProtocol, "doi"), (OTkLinkTarget, "10.1000/182"), _, _
        ],
         l("[[file:/home/images/jupiter.jpg]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "/home/images/jupiter.jpg"), _, _
        ],
        l("[[/home/images/jupiter.jpg]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "/home/images/jupiter.jpg"), _, _
        ],
        l("[[file:papers/last.pdf]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "papers/last.pdf"), _, _
        ],
        l("[[./papers/last.pdf]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "./papers/last.pdf"), _, _
        ],
        l("[[file:/ssh:me@some.where:papers/last.pdf]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "/ssh:me@some.where:papers/last.pdf"), _, _
        ],
        l("[[/ssh:me@some.where:papers/last.pdf]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "/ssh:me@some.where:papers/last.pdf"), _, _
        ],
        l("[[file:sometextfile::NNN]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "sometextfile"),
          (OTkLinkExtraSeparator, "::"), (OTkLinkExtra, "NNN"), _, _
        ],
        l("[[file:projects.org]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "projects.org"), _, _
        ],
        l("[[file:projects.org::some words]]"): [
          _, _,
          (OTkLinkProtocol, "file"),
          (OTkLinkTarget, "projects.org"),
          (OTkLinkExtraSeparator, "::"),
          (OTkLinkExtra, "some words"), _, _
        ],
        l("[[attachment:projects.org]]"): [
          _, _,
          (OTkLinkProtocol, "attachment"),
          (OTkLinkTarget, "projects.org"), _, _
        ],
        l("[[id:B7423F4D-2E8A-471B-8810-C40F074717E9]]"): [
          _, _,
          (OTkLinkProtocol, "id"),
          (OTkLinkTarget, "B7423F4D-2E8A-471B-8810-C40F074717E9"), _, _
        ],
        l("[[shell:ls *.org]]"): [
          _, _,
          (OTkLinkProtocol, "shell"),
          (OTkLinkTarget, "ls *.org"), _, _
        ],
        l("[[target]]"): [
          _, _, (OTkLinkInternal, "target"), _, _
        ]
      ]

  test "Metatags":
    check:
      matchdiff @(kind, strVal), [
        l(r"\arg{test}"): [
          (OTkSymbolStart, r"\"),
          (OTkIdent, "arg"),
          (OTkMetaArgsOpen),
          (OTkMetaArgsBody, "test"),
          (OTkMetaArgsClose)
        ],
        l(r"\arg{test}{test2}"): [
          (OTkSymbolStart, r"\"),
          (OTkIdent, "arg"),
          (OTkMetaArgsOpen), (OTkMetaArgsBody, "test"), (OTkMetaArgsClose),
          (OTkMetaArgsOpen), (OTkMetaArgsBody, "test2"), (OTkMetaArgsClose)
        ],
        l(r"\edsl{{<ch1>, <ch2>, ...}}"): [
          (OTkSymbolStart, r"\"),
          (OTkIdent, "edsl"),
          (OTkMetaArgsOpen),
          (OTkMetaArgsBody, "{<ch1>, <ch2>, ...}"),
          (OTkMetaArgsClose)
        ],
      ]
