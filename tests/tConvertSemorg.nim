import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/types/colorstring

import
  haxorg/parse/[parse_org_structure, convert_semorg],
  haxorg/defs/[impl_org_node, impl_sem_org, org_types],
  haxorg/runcode/[runcode_nim]

suite "Convert to semorg":
  test "Full document":
    let tree = parseOrg(varPosStr asConst slurp"assets/input-1.txt")
    echo tree.treeRepr()
    var conf = RunConf()
    conf["nim"] = newNimCodeBlock

    let sem = tree.toSem(conf, @[])
    echo sem.treeRepr()
