import haxorg/[types, parser, semorg]
import hmisc/core/all
import parse_todo_graph
startHax()

proc recTree(doc: SemDocument, sem: SemOrg): seq[string] =
  discard
  # echo sem.treeRepr()

when isMainModule:
  let
    tree = orgParse(readFile(relToSource"assets/mind_map.org"))
    sem = toSemOrg(tree, nil)
    doc = toDocument(sem)

  let dot = doc.recTree(sem)
  # echo tree.treeRepr()
