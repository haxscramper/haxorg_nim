import hmisc/preludes/unittest
import haxorg/lexer

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), initLexTable())

suite "Extended table syntax":
  test "Basic lexer":
    let toks = l("""
#+begin-table :width 12cm
| r1c1 | r1c2 |
| r2c1
| r2c2
#+row
r3c1
#+cell
r3c2
#+end-table""")

    check:
      matchdiff @(kind, strVal), [
        l("#+begin-table"): [(OTbTableBegin)],
        l("#+begin-table\n|item1|item2|\n"): [
          (OTbTableBegin),
          (OTbCmdArguments),
          (OTbPipeOpen),
          (OTbContent, "item1"),
          (OTbPipeSeparator),
          (OTbContent, "item2"),
          (OTbPipeClose)
        ],
        l("#+begin-table\n|cell content\n"): [
          (OTbTableBegin),
          (OTbCmdArguments),
          (OTbPipeCellOpen),
          (OTbContent, "cell content")
        ],
        l("#+begin-table\n|row1\n|row2\n"): [
          (OTbTableBegin), (OTbCmdArguments),
          (OTbPipeCellOpen), (OTbContent, "row1"),
          (OTbPipeCellOpen), (OTbContent, "row2")
        ],
        toks: [
          #[ 01 ]# (OTbTableBegin),
          #[ 02 ]# (OTbCmdArguments, ":width 12cm"),

          #[ __ ]# # row 1
          #[ 03 ]# (OTbPipeOpen),
          #[ 04 ]# (OTbContent, "r1c1"),
          #[ 05 ]# (OTbPipeSeparator),
          #[ 06 ]# (OTbContent, "r1c2"),
          #[ 07 ]# (OTbPipeClose),

          #[ __ ]# # row 2
          #[ 08 ]# (OTbPipeCellOpen),
          #[ 09 ]# (OTbContent, "r2c1"),
          #[ 10 ]# (OTbPipeCellOpen),
          #[ 11 ]# (OTbContent, "r2c2"),

          #[ __ ]# # row 3
          #[ 00 ]# (OTbRowSpec),
          #[ 00 ]# (OTbCmdArguments),
          #[ 00 ]# (OTbContent, "r3c1"),
          #[ 00 ]# (OTbCellSpec),
          #[ 00 ]# (OTbCmdArguments),
          #[ 00 ]# (OTbContent, "r3c2"),

          #[ 00 ]# (OTbTableEnd)
        ],
        l("""
#+begin-table
|1|2|
#+end-table"""
        ): [
          #[ 01 ]# (OTbTableBegin),
          #[ 02 ]# (OTbCmdArguments),

          #[ __ ]# # row 1
          #[ 03 ]# (OTbPipeOpen),
          #[ 04 ]# (OTbContent, "1"),
          #[ 05 ]# (OTbPipeSeparator),
          #[ 06 ]# (OTbContent, "2"),
          #[ 07 ]# (OTbPipeClose),

          #[ 08 ]# (OTbTableEnd)
        ],
        l("""
#+begin-table :format 40px,40px
|1|2|
#+end-table
"""
        ): [
          #[ 01 ]# (OTbTableBegin),
          #[ 02 ]# (OTbCmdArguments, ":format 40px,40px"),

          #[ __ ]# # row 1
          #[ 03 ]# (OTbPipeOpen),
          #[ 04 ]# (OTbContent, "1"),
          #[ 05 ]# (OTbPipeSeparator),
          #[ 06 ]# (OTbContent, "2"),
          #[ 07 ]# (OTbPipeClose),

          #[ 08 ]# (OTbTableEnd)
        ]
      ]

