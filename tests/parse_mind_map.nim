import haxorg/[types, parser, semorg]
import hmisc/core/all
import std/[strutils, tables]
import hmisc/other/oswrap
import parse_todo_graph
import haxorg/exporter/[exporter_ultraplain, exporter_dot_html]
startHax()


proc text(node: SemOrg): string =
  newUltraplainTextExporter().withExporter(node).res

proc exp(node: SemOrg): DotHtmlExporter =
  newDotHtmlExporter().withExporter(node)

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
      let
        title = sem.subtree.title
        leaf = sem.isLeafSubtree()
        desc = sem.getTreeDescription().get(newEmptySem())

      if leaf:
        let
          # main = title.exp()
          desc: Option[DotHtmlExporter] = tern(
            not(desc of orgEmpty), desc.exp())

          # main.res
        result.add "$id[label=$title];" % {
          "title": tern(desc.isSome(), "<$#>" % desc.get().res, "\"\""),
          "id": sem.getSafeTreeIdImage()
        }

        if desc.canGet(desc):
          for row, links in desc.rowLinks:
            echov row

      else:
        result.add "$id[label=$label, color=red];" % {
          "label": tern(desc of orgEmpty, "\"\"", "<$#>" % desc.exp().res),
          "id": sem.getSafeTreeIdImage(),
        }
        
        result.add "subgraph cluster_c$id {\n  label=\"$label\";" % {
          "id": sem.getSafeTreeIdImage(),
          "label": title.text().escapeDot()
        }

      let inLinks = sem.nestedLeavesDfs(
        allowed = proc(node: SemOrg): bool = node of orgLink,
        endrecurse = proc(node: SemOrg): bool =
                       node != sem and node of orgSubtree
      )

      for link in inLinks:
        if doc.getLinked(link.link).canGet(it):
          var label = ""
          if link.parent.notNil():
            # echov "found parent for link entry"
            # echov link.parent.text()
            label = link.parent.text().escapeDot()

          # else:
          #   if not link.isGenerated():
          #     echov link.node.
            
            
          result.add "$src -> $dst[label=\"$label\"];" % {
            "src": it.getSafeTreeIdImage(),
            "dst": sem.getSafeTreeIdImage(),
            "label": label
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
    tree = orgParse(readFile(relToSource"assets/mind_map.org"))
    # tree = orgParse(readFile(
    #   "/mnt/workspace/repos/fic/wiki/places/academy_city.org"))
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
