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
          (otaCommandOpen),
          (otaTableBegin)
        ]
      ]
