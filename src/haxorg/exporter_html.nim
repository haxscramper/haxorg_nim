import exporter, semorg, ast, buf
import std/[xmltree, strtabs, sugar, macros, strformat,
            strutils, options, tables]

import hasts/html_ast
import hmisc/[hdebug_misc, base_errors, helpers]
import hmisc/algo/halgorithm
import fusion/matching

type
  OrgHtmlExporter = ref object of OrgExporter
    impl: ExportDispatcher[OrgHtmlExporter, Xml]


using
  exp: OrgHtmlExporter
  conf: RunConfig
  tree: SemOrg

proc exportUsing(exp, tree, conf): Option[Xml] =
  exp.exportUsing(exp.impl, tree, conf)

proc exportAllUsing*(exp, tree, conf): seq[Xml] =
  for node in tree:
    if Some(@exported) ?= exportUsing(exp, node, conf):
      result.add exported

proc exportParagraphItems*(exp, tree, conf): seq[Xml] =
  for node in tree:
    result.add exp.exportUsing(exp.impl, node, conf)

proc exportParagraph*(exp, tree, conf): Xml =
  newXml("p", exportAllUsing(exp, tree, conf))

proc exportSrcCode*(exp, tree, conf): Xml =
  result = newHtmlCode(tree.codeBlock.code.strip())

proc exportSubtree*(exp, tree, conf): Xml =
  result = newXmlSeq()
  var head = newXml(
  "h" & $tree.subtLevel,
    exportParagraphItems(exp, tree["title"], conf)
  )

  result.add head

  for node in tree["body"]:
    result.add exp.exportUsing(exp.impl, node, conf)

proc exportWord*(exp, tree, conf): Xml = newText($tree.node.text)

proc htmlTagsForItem*(tree): tuple[list, item: string] =
  result = ("ul", "li")
  if tree["tag"].kind != onkEmptyNode:
    result = ("dl", "dd")

  elif tree.itemBullet notin ["-", "+", ">", "*"]:
    result = ("ol", "li")


proc exportList*(exp, tree, conf): Xml =
  newXml(
    tree[0].htmlTagsForItem().list,
    exp.exportAllUsing(tree, conf)
  )

proc exportListItem*(exp, tree, conf): Xml =
  assert tree.kind == onkListItem
  let (list, item) = htmlTagsForItem(tree)
  let text = @[
    newXml("p", exp.exportAllUsing(tree["header"], conf)),
    newXml("p", exp.exportAllUsing(tree["body"], conf))
  ]

  if item == "dd":
    result = newXmlSeq()
    result.add newXml("dt", exp.exportAllUsing(tree["tag"], conf))
    result.add newXml(item, text)

  else:
    result = newXmlSeq(text)


proc exportStmtList*(exp, tree, conf): Xml =
  result = newXmlSeq()
  for node in tree:
    result.add exp.exportUsing(exp.impl, node, conf)

proc exportDocument*(exp, tree, conf): Xml =
  var body: seq[Xml]
  var head = newXml("head")

  if { "title" : @title } ?= tree.properties:
    head.add newXml("title").withIt do:
      it.add exp.exportUsing(tree, conf)

  for node in tree:
    body.add exp.exportUsing(exp.impl, node, conf)

  result = newXmlTree("html", @[head, newXmlTree("body", body)])

proc newOrgHtmlExporter*(): OrgHtmlExporter =
  result = OrgHtmlExporter(
    name: "html-base",
    fileExt: "html",
    description: "Base html exporter"
  )

  result.impl[orgAllKinds] =
    proc(exp; tree; conf): auto = some newXml($tree.kind)

  result.impl[onkSubtree] = exportSubtree
  result.impl[onkDocument] = exportDocument
  result.impl[onkStmtList] = exportStmtList
  result.impl[onkWord] = exportWord
  result.impl[onkSrcCode] = exportSrcCode
  result.impl[onkParagraph] = exportParagraph
  result.impl[onkList] = exportList
  result.impl[onkListItem] = exportListItem

register(newOrgHtmlExporter())

method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  echo treeRepr(tree)
  let tmp = exp.exportUsing(exp.impl, tree, conf)
  if tmp.isSome():
    target = toPrettyStr(tmp.get())

  echo target
