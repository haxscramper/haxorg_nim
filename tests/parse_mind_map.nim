import haxorg/[types, parser, semorg]
import hmisc/core/all
import std/strutils
import hmisc/other/oswrap
import parse_todo_graph
import haxorg/exporter/exporter_ultraplain
startHax()


proc text(node: SemOrg): string =
  newUltraplainTextExporter().withExporter(node).res

proc isLeafSubtree*(sem: SemOrg): bool =
  result = true
  for sub in sem:
    let nested = sub.nestedLeavesDfs(
      allowed = proc(node: SemOrg): bool = node of orgSubtree
    )

    if not nested.empty():
      return false

proc recTree(doc: SemDocument, sem: SemOrg): seq[string] =
  case sem.kind:
    of orgContainerLikeKinds:
      for node in sem:
        result.add recTree(doc, node)

    of orgListItem:
      discard

    of orgSubtree:
      let leaf = sem.isLeafSubtree()
      if leaf:
        result.add "$id[label=\"$title\"];" % {
          "title": sem.subtree.title.text(),
          "id": sem.getSafeTreeIdImage()
        }

      else:
        result.add "subgraph cluster_$id {\n  label=\"$label\";" % {
          "id": sem.getSafeTreeIdImage(),
          "label": sem.subtree.title.text()
        }

      let inLinks = sem.nestedLeavesDfs(
        allowed = proc(node: SemOrg): bool = node of orgLink,
        endrecurse = proc(node: SemOrg): bool =
                       node != sem and node of orgSubtree
      )

      for link in inLinks:
        if doc.getLinked(link.link).canGet(it):
          result.add "$src -> $dst;" % {
            "src": it.getSafeTreeIdImage(),
            "dst": sem.getSafeTreeIdImage()
          }


      for sub in sem:
        result.add recTree(doc, sub)

      if not leaf:
        result.add "}"

    of orgTokenLikeKinds + orgTokenKinds:
      discard

    else:
      discard
      # raise newUnexpectedKindError(sem)

when isMainModule:
  let
    # tree = orgParse(readFile("/tmp/fic.org"))
    tree = orgParse(readFile(
      "/mnt/workspace/repos/fic/wiki/places/academy_city.org"))
    sem = toSemOrg(tree, nil)
    doc = toDocument(sem)

  writeFile("/tmp/parsed.nim", tree.treeRepr().toString(false))

  let dot = doc.recTree(sem)

  # echo join(dot, "\n")
  # echo tree.treeRepr()
  mkDir getAppTempDir()

  compileDot("""
    digraph G {
        node[shape=rect,fontname=iosevka,penwidth=2];
        edge[fontname=iosevka,penwidth=2];
        graph[fontname=iosevka];
        nodesep=0.8;
        rankdir=LR;
        splines=polyline;
    $#
    }
    """ % [
      dot.join("\n")
  ])
