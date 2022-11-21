import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[types, parser, semorg]
import std/strutils


let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline.org"))
# let tree = orgParse(readFile("/tmp/timeline.org"))

let sem = toSemOrg(tree)

writeFile("/tmp/res", tree.treeRepr().toString(false))
