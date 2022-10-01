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
        l("#+begin-table"): [(OTkTableBegin)],
        l("#+begin-table\n|item1|item2|\n"): [
          (OTkTableBegin),
          (OTkCmdArguments),
          (OTkPipeOpen),
          (OTkContent, "item1"),
          (OTkPipeSeparator),
          (OTkContent, "item2"),
          (OTkPipeClose)
        ],
        l("#+begin-table\n|cell content\n"): [
          (OTkTableBegin),
          (OTkCmdArguments),
          (OTkPipeCellOpen),
          (OTkContent, "cell content")
        ],
        l("#+begin-table\n|row1\n|row2\n"): [
          (OTkTableBegin), (OTkCmdArguments),
          (OTkPipeCellOpen), (OTkContent, "row1"),
          (OTkPipeCellOpen), (OTkContent, "row2")
        ],
        toks: [
          #[ 01 ]# (OTkTableBegin),
          #[ 02 ]# (OTkCmdArguments, ":width 12cm"),

          #[ __ ]# # row 1
          #[ 03 ]# (OTkPipeOpen),
          #[ 04 ]# (OTkContent, "r1c1"),
          #[ 05 ]# (OTkPipeSeparator),
          #[ 06 ]# (OTkContent, "r1c2"),
          #[ 07 ]# (OTkPipeClose),

          #[ __ ]# # row 2
          #[ 08 ]# (OTkPipeCellOpen),
          #[ 09 ]# (OTkContent, "r2c1"),
          #[ 10 ]# (OTkPipeCellOpen),
          #[ 11 ]# (OTkContent, "r2c2"),

          #[ __ ]# # row 3
          #[ 00 ]# (OTkRowSpec),
          #[ 00 ]# (OTkCmdArguments),
          #[ 00 ]# (OTkContent, "r3c1"),
          #[ 00 ]# (OTkCellSpec),
          #[ 00 ]# (OTkCmdArguments),
          #[ 00 ]# (OTkContent, "r3c2"),

          #[ 00 ]# (OTkTableEnd)
        ],
        l("""
#+begin-table
|1|2|
#+end-table"""
        ): [
          #[ 01 ]# (OTkTableBegin),
          #[ 02 ]# (OTkCmdArguments),

          #[ __ ]# # row 1
          #[ 03 ]# (OTkPipeOpen),
          #[ 04 ]# (OTkContent, "1"),
          #[ 05 ]# (OTkPipeSeparator),
          #[ 06 ]# (OTkContent, "2"),
          #[ 07 ]# (OTkPipeClose),

          #[ 08 ]# (OTkTableEnd)
        ],
        l("""
#+begin-table :format 40px,40px
|1|2|
#+end-table
"""
        ): [
          #[ 01 ]# (OTkTableBegin),
          #[ 02 ]# (OTkCmdArguments, ":format 40px,40px"),

          #[ __ ]# # row 1
          #[ 03 ]# (OTkPipeOpen),
          #[ 04 ]# (OTkContent, "1"),
          #[ 05 ]# (OTkPipeSeparator),
          #[ 06 ]# (OTkContent, "2"),
          #[ 07 ]# (OTkPipeClose),

          #[ 08 ]# (OTkTableEnd)
        ]
      ]

