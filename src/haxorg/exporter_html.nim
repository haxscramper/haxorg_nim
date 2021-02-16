import exporter, semorg, ast, buf
import std/[xmltree, strtabs, sugar, macros, strformat,
            strutils, options, tables, sequtils]

import hasts/html_ast
import hmisc/[hdebug_misc, base_errors, helpers]
import hmisc/algo/halgorithm
import fusion/matching

import cmd_pygmentize

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
  return pygmentizeToHtml(
    tree.codeBlock.code.strip(), tree.codeBlock.langName
  )
  # result = newXml("code", @[])

  # result["classs"] = "highlight"

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

proc exportListItem(exp, tree, conf; listKind: OrgNodeSubKind): Xml =
  var body = mapIt(tree["body"], exp.exportAllUsing(it, conf)).concat()
  if body.len > 0:
    body = @[newText(" ")] & body

  let text = @[newXml("p", exp.exportAllUsing(tree["header"], conf) & body)]

  case listKind:
    of oskPartialDescList, oskFullDescList:
      result = newXmlSeq(@[
        newXml("dt", @[newXml("b", exp.exportAllUsing(tree["tag"], conf))]),
        newXml("dd", text)
      ])

    of oskOrderedList, oskUnorderedList, oskMixedList:
      result = newXml("li", text)

    else:
      discard



proc exportList*(exp, tree, conf): Xml =
  let tag =
    case tree.subKind:
      of oskPartialDescList, oskFullDescList: "dl"
      of oskOrderedList: "ol"
      of oskUnorderedList, oskMixedList: "ul"
      else: subKindErr(tree.subKind)

  result = newXml(tag, mapIt(tree, exportListItem(exp, it, conf, tree.subKind)))


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

proc exportMarkup*(exp, tree, conf): Xml =
  let subnodes = exportAllUsing(exp, tree, conf)
  case tree.subKind:
    of oskBold: newXml("b", subnodes)
    of oskItalic: newXml("i", subnodes)
    else:
      newXml("zz", subnodes)

proc newDiv*(xml: Xml, class: string): Xml =
  result = newXml("div", @[xml])
  result["class"] = class


proc newSpan*(xml: Xml, class: string): Xml =
  result = newXml("span", @[xml])
  result["class"] = class

proc exportMetaTag*(exp, tree, conf): Xml =
  let tag =
    case tree.metaTag.kind:
      of smtEnv:
        newText("$" & $tree["body"][0].node.text)
      else:
        newText($tree.node.text)

  result = newSpan(tag, "mt-" & toLowerAscii($tree["name"].node.text))



proc newOrgHtmlExporter*(): OrgHtmlExporter =
  result = OrgHtmlExporter(
    name: "html-base",
    fileExt: "html",
    description: "Base html exporter",
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
  # result.impl[onkListItem] = exportListItem
  result.impl[onkMarkup] = exportMarkup
  result.impl[onkMetaTag] = exportMetaTag

register(newOrgHtmlExporter())

method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  # echo treeRepr(tree)
  let tmp = exp.exportUsing(exp.impl, tree, conf)
  target = """
<!DOCTYPE html>
<style>

.highlight .hll { background-color: #49483e }
.highlight  { background: #272822; color: #f8f8f2 }
.highlight .c { color: #75715e } /* Comment */
.highlight .err { color: #960050; background-color: #1e0010 } /* Error */
.highlight .k { color: #66d9ef } /* Keyword */
.highlight .l { color: #ae81ff } /* Literal */
.highlight .n { color: #f8f8f2 } /* Name */
.highlight .o { color: #f92672 } /* Operator */
.highlight .p { color: #f8f8f2 } /* Punctuation */
</style>
"""

  if tmp.isSome():
    target &= toPrettyStr(tmp.get())
