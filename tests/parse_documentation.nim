import haxorg/[types, parser, semorg]
import hmisc/core/all
startHax()
let
  tree = orgParse(readFile(relToSource"assets/documentation.org"))
  sem = toSemOrg(tree, nil)
