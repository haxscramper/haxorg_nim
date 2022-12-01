import haxorg/[types, parser, semorg]
import hmisc/core/all
startHax()
let
  tree = orgParse(readFile(relToSource"documentation.org"))
  sem = toSemOrg(tree, nil)
