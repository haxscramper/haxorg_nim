import hmisc/preludes/unittest
import haxorg/parse/parse_org_table
import hmisc/algo/[hlex_base, hparse_base]

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexTable)

suite "Extended table syntax":
  test "No cells":
    check:
      matchdiff @(kind, strVal), [
        l("#+begin-table"): [
          (otaTableBegin),
          (otaCmdArguments)
        ],
        l("#+begin-table\n|item1|item2|\n"): [
          (otaTableBegin),
          (otaCmdArguments),
          (otaPipeOpen),
          (otaContent, "item1"),
          (otaPipeSeparator),
          (otaContent, "item2"),
          (otaPipeClose)
        ],
#         l("#+begin-table\n|cell content\n"): [
#           (otaTableBegin),
#           (otaCmdArguments),
#           (otaPipeCellOpen),
#           (otaContent, "cell content")
#         ],
#         l("#+begin-table\n|row1\n|row2\n"): [
#           (otaTableBegin),
#           (otaCmdArguments),
#           (otaPipeCellOpen),
#           (otaContent, "row1"),
#           (otaContent, "row2")
#         ],
#         l("""
# #+begin-table :width 12cm
# | r1c1 | r1c2 |
# | r2c1
# | r2c2
# #+row
# r3c1
# #+cell
# r3c2
# #+table-end
# """
#         ): [
#           #[ 00 ]# (otaTableBegin),
#           #[ 00 ]# (otaCmdArguments, ":width 12cm"),

#           #[ __ ]# # row 1
#           #[ 00 ]# (otaPipeOpen),
#           #[ 00 ]# (otaContent, "r1c1"),
#           #[ 00 ]# (otaPipeSeparator),
#           #[ 00 ]# (otaContent, "r1c2"),
#           #[ 00 ]# (otaPipeClose),

#           #[ __ ]# # row 2
#           #[ 00 ]# (otaPipeCellOpen),
#           #[ 00 ]# (otaContent, "r2c1"),
#           #[ 00 ]# (otaPipeCellOpen),
#           #[ 00 ]# (otaContent, "r2c2"),

#           #[ __ ]# # row 3
#           #[ 00 ]# (otaRowSpec),
#           #[ 00 ]# (otaCmdArguments),
#           #[ 00 ]# (otaContent, "r3c1"),
#           #[ 00 ]# (otaCellSpec),
#           #[ 00 ]# (otaCmdArguments),
#           #[ 00 ]# (otaContent, "r3c2"),

#           #[ 00 ]# (otaTableEnd)
#         ]
      ]
