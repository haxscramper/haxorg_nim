import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[types, parser, semorg]
import std/strutils


let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline.org"))
# let tree = orgParse(readFile("/tmp/timeline.org"))

let sem = toSemOrg(tree, nil)

writeFile("/tmp/res", tree.treeRepr().toString(false))

for node in itemsDFS(sem):
  if node of orgSubtree and node.subtree.level == 1:
    let tree = node.subtree
    echov node.treeRepr()
    # echov tree.title.treeRepr()
