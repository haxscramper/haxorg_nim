import exporter, semorg, ast, buf
import std/[xmltree, strtabs, sugar, macros, strformat, strutils, options]
import hasts/html_ast
import hmisc/[hdebug_misc, base_errors, helpers]
import hmisc/algo/halgorithm

type
  OrgHtmlExporter = ref object of OrgExporter


register(OrgHtmlExporter(
  name: "html-base",
  fileExt: "html",
  description: "Base html exporter"
))

using
  exp: OrgHtmlExporter
  conf: RunConfig
  tree: SemOrg

proc newHtml*(tag: string): HtmlElem = newElementHtml(tag)

proc newHtml2*[T](tag: string): HtmlElem = newHtml(tag)

proc exportMain*(exp, tree, conf): seq[HtmlElem]

proc exportSrcCode*(exp, tree, conf): HtmlElem =
  result = newHtmlCode(tree.codeBlock.code.strip())

proc exportSubtree*(exp, tree, conf): seq[HtmlElem] =
  echov "Exporting subtree"
  var head = newHtml("h1")
  head.add exportMain(exp, tree["title"], conf)
  result.add head

  for node in tree["body"]:
    result.add exportMain(exp, node, conf)

proc exportDocument*(exp, tree, conf): HtmlElem =
  result = newHtml("html")

  var body = collect(newHtml2("body")):
    for node in tree:
      exportMain(exp, node, conf)

  result.add body


proc exportStmtList*(exp, tree, conf): seq[HtmlElem] =
  for node in tree:
    result.add exportMain(exp, node, conf)

template expectItArg*(arg, expr: untyped): untyped =
  let it {.inject.} = arg
  if not expr:
    raise ArgumentError(
      msg: "Argument " & astToStr(arg) & " does not match for assertion " &
        astToStr(expr))

proc exportLink*(exp, tree, conf): HtmlElem =
  expectItArg tree, it.kind == onkLink
  if tree.linkTarget.kind in {olkWeb}:
    result = newHtmlLink(
      tree.linkTarget.webUrl.string,
      mapSomeIt(tree.linkDescription, exportMain(exp, it, conf))
    )

  else:
    raiseImplementError("Unsupported link kind - " & $tree.linkTarget.kind)



proc exportParagraph*(exp, tree, conf): HtmlElem =
  collect(newHtml2("p")):
    for node in tree:
      exportMain(exp, node, conf)

proc exportMain*(exp, tree, conf): seq[HtmlElem] =
  case tree.kind:
    of onkSubtree:   result.add exportSubtree(exp,   tree, conf)
    of onkDocument:  result.add exportDocument(exp,  tree, conf)
    of onkStmtList:  result.add exportStmtList(exp,  tree, conf)
    of onkSrcCode:   result.add exportSrcCode(exp,   tree, conf)
    of onkParagraph: result.add exportParagraph(exp, tree, conf)
    of onkLink:      result.add exportLink(exp,      tree, conf)
    of onkWord:      result.add toHtmlText(strip($tree.node.text))

    else:
      raiseImplementError(&"Implement for node {tree.kind}")

method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  target = toPrettyStr(exportMain(exp, tree, conf)[0])
  echo target
