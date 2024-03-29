import haxorg/[types, parser, semorg]
import hmisc/core/all
import std/[strutils, tables]
import hmisc/other/oswrap
import parse_todo_graph
import haxorg/exporter/[exporter_ultraplain, exporter_dot_html]
startHax()


proc text(node: SemOrg): string =
  newUltraplainTextExporter().withExporter(node).res

proc isFootnoteParagraph*(node: SemOrg): bool =
  node of orgAnnotatedParagraph and
  node.paragraph of aopFootnote

proc isLinkDescriptionItem*(node: SemOrg): bool =
  # Find all annotated paragraphs
  node of orgAnnotatedParagraph and
  # That were used as description list headers
  node.paragraph of aopListText and
  # With links
  nestedLeavesDfs(
    node.paragraph.itemTag,
    inSetPredicate({orgLink})).notEmpty()


proc exp(node: SemOrg, isSubtree: bool = true): DotHtmlExporter =
  var conv = newDotHtmlExporter()
  conv.allowNode = proc(sem: SemOrg, conv: Exporter): bool =
    result = true
    if isSubtree:
      if isFootnoteParagraph(sem):
        result = false

      elif sem of orgList and sem.anySubnode(isLinkDescriptionItem):
        result = false

  result = conv.withExporter(node)

proc isLeafSubtree*(sem: SemOrg): bool =
  result = true
  for sub in sem:
    let nested = sub.nestedLeavesDfs(
      allowed = proc(node: SemOrg): bool = node of orgSubtree
    )

    if not nested.empty():
      return false

type
  MindMapExportConf* = object
    clusterParents*: bool

proc recTree(
    conf: MindMapExportConf,
    doc: SemDocument,
    sem: SemOrg
  ): seq[string] =

  case sem.kind:
    of orgContainerLikeKinds:
      for node in sem:
        result.add conf.recTree(doc, node)

    of orgListItem:
      discard

    of orgSubtree:
      let
        title = sem.subtree.title
        leaf = sem.isLeafSubtree()
        desc = sem.getTreeBody()
        hasDesc = desc.notEmpty()

      if leaf or not conf.clusterParents:
        let desc: Option[DotHtmlExporter] = tern(hasDesc, desc.exp())
        result.add "$id[label=<$title>,shape=plaintext];" % {
          "title": tern(hasDesc, desc.get().res, title.exp().res),
          "id": sem.getSafeTreeIdImage(),
          "shape": tern(hasDesc, "plaintext", "rect")
        }

        if desc.canGet(desc):
          for row, links in desc.rowLinks:
            for link in links:
              if doc.getLinked(link).canGet(target):
                result.add "$src:row$row -> $dst;" % {
                  "src": sem.getSafeTreeIdImage(),
                  "dst": target.getSafeTreeIdImage(),
                  "row": $row,
                }

          proc aux(
            id: string, table: Table[int, seq[OrgFootnote]]): seq[string] =
            for row, notes in table:
              for note in notes:
                if doc.getLinked(note).canGet(target) and
                   target of orgAnnotatedParagraph:
                  result.add "$#:row$# -> $#;" % [
                    id,
                    $row,
                    getSafeIdImage(note)
                  ]

                  let conv = target.exp(isSubtree = false)
                  result.add "$id[label=<$label>, shape=plaintext];" % {
                    "id": note.getSafeIdImage(),
                    "label": conv.res,
                  }

                  result.add aux(
                    note.getSafeIdImage(), conv.rowFootnotes)

          result.add aux(sem.getSafeTreeIdImage(), desc.rowFootnotes)

      else:
        result.add "$id[label=<$label>, color=red,shape=plaintext];" % {
          "label": tern(hasDesc, desc.exp().res, title.exp().res),
          "id": sem.getSafeTreeIdImage(),
          "shape": tern(hasDesc, "plaintext", "rect")
        }
        
        result.add "subgraph cluster_c$id {\n  label=\"$label\";" % {
          "id": sem.getSafeTreeIdImage(),
          "label": title.text().escapeDot()
        }

      let annotatedLinks = sem.nestedLeavesDfs(
        allowed = isLinkDescriptionItem,
        endrecurse = proc(node: SemOrg): bool =
                       node != sem and node of orgSubtree
      )

      for node in annotatedLinks:
        for link in node.paragraph.itemTag.itemsDfs({orgLink}):
          if doc.getLinked(link.link).canGet(it):
            let label = node.paragraph.body.text().escapeDot()
            result.add "$src -> $dst[label=\"$label\"];" % {
              "src": it.getSafeTreeIdImage(),
              "dst": sem.getSafeTreeIdImage(),
              "label": label
            }


      for sub in sem:
        result.add conf.recTree(doc, sub)

      if not leaf and conf.clusterParents:
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

  var conf = MindMapExportConf()
  let dot = conf.recTree(doc, sem)

  # echo join(dot, "\n")
  # echo tree.treeRepr()
  mkDir getAppTempDir()

  compileDot("""
    digraph G {
        node[fontname=iosevka,penwidth=2];
        edge[fontname=iosevka,penwidth=2];
        graph[fontname=iosevka];
        nodesep=0.8;
    $#
    }
    """ % [
      dot.join("\n")
  ])
