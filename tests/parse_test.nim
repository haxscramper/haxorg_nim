import hmisc/preludes/unittest
import haxorg/[
  parser,
  types
]

let tree = orgParse("*bold*")

echo tree.treeRepr()
