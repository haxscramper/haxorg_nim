import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/types/colorstring

import
  haxorg/parse/[parse_org_structure, convert_semorg],
  haxorg/defs/[impl_org_node, impl_sem_org]

suite "Convert to semorg":
  test "Full document":
    let tree = parseOrg(varPosStr asConst slurp"assets/input-1.txt")
    echo tree.treeRepr()
    let sem = tree.toSem()
    echo sem.treeRepr()
