import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/types/colorstring

import
  haxorg/parse/parse_org_structure,
  haxorg/defs/impl_org_node

suite "tmp parse":
  test "text":
    let tree = parseOrg(varPosStr lit3"""
    #+begin-src nim -n :cmdline --hints:off
    for i in 0 .. 10:
      echo i
    #+end-src
    """)
    echo tree.treeRepr()
