import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/types/colorstring

import
  haxorg/parse/parse_org_structure,
  haxorg/defs/impl_org_node

suite "tmp parse":
  test "text":
    let tree = parseOrg(varPosStr "- *list*")
    echo tree.treeRepr()
