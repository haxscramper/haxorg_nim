import hmisc/core/all

import
  ./exporter_root,
  ../defs/defs_all,
  ../external/cmd_pygmentize

import std/[
  xmltree, strtabs, sugar, macros, strformat,
  strutils, options, tables, sequtils
]

import
  hmisc/hasts/[xml_ast],
  hmisc/other/hshell,
  hmisc/algo/hstring_algo

type
  OrgHtmlExporter = ref object of RootExporter
    impl: ExportDispatcher[OrgHtmlExporter, XmlWriter]


createOnExport(OrgHtmlExporter, XmlWriter)

using
  exp: OrgHtmlExporter
  w: var XmlWriter
  conf: RunConf
  tree: SemOrg

proc exportUsing(exp, w, tree, conf) =
  exp.exportUsing(w, exp.impl, tree, conf)

proc exportAllUsing*(exp, w, tree, conf) =
  for node in tree:
    exportUsing(exp, w, node, conf)

proc exportParagraph*(exp, w, tree, conf) =
  w.ewrap("p", []):
    exportAllUsing(exp, w, tree, conf)

proc exportSrcCode*(exp, w, tree, conf) =
  let cb = tree.codeBlock
  w.writeRaw pygmentizeToHtml(cb.code.strip(), cb.langName)

  if cb.execResult.canGet(codeRes):
    if codeRes.execResult.canGet(eres):
      w.writeRaw("Execution returned")
      w.ewrapl("code", []):
        w.ewrapl("pre", []):
          w.writeRaw(eres.getStdout())



proc exportSubtree*(exp, w, tree, conf) =
  let name = tree.getName().toDashedCase()
  w.ewrapl("section", {"id": name}):
    w.eindent():
      w.ewrapl1("h" & $tree.subtree.level, []):
        w.ewrap("a", {"href": "#" & name}):
          exportAllUsing(exp, w, tree[semfTitle], conf)

      exportAllUsing(exp, w, tree[semfBody], conf)

proc exportWord*(exp, w, tree, conf) = w.writeRaw(tree.node.text.strVal())

proc exportListItem(exp, w, tree, conf; listKind: OrgNodeSubKind) =
  case listKind:
    of oskPartialDescList, oskFullDescList:
      w.ewrapl("dt", []):
        w.ewrapl("b", []):
          exp.exportAllUsing(w, tree[semfTag], conf)

      w.ewrapl("dd", []):
        w.ewrapl("p", []):
          exp.exportAllUsing(w, tree[semfHeader], conf)

    of oskOrderedList, oskUnorderedList, oskMixedList:
      w.ewrapl("li", []):
        exp.exportAllUsing(w, tree[semfHeader], conf)

    else:
      discard



proc exportList*(exp, w, tree, conf) =
  let tag =
    case tree.subKind:
      of oskPartialDescList, oskFullDescList: "dl"
      of oskOrderedList: "ol"
      of oskUnorderedList, oskMixedList: "ul"
      else: subKindErr(tree.subKind)

  w.ewrapl(tag, []):
    for item in tree:
      exportListItem(exp, w, item, conf, tree.subKind)

proc exportDocument*(exp, w, tree, conf) =
  w.writeRaw "<!DOCTYPE html>\n"
  w.ewrapl("html", []): w.eindent:

    w.ewrapl("style", []): w.eindent:
      w.writeRaw(pygmentizeGetHtmlStyle())

    w.ewrapl("head", []): w.eindent:
      w.writeRaw("")

    w.ewrapl("body", []): w.eindent:
      exportAllUsing(exp, w, tree, conf)

proc exportMarkup*(exp, w, tree, conf) =
  let tag = case tree.kind:
    of orgBold: "b"
    of orgItalic: "i"
    of orgMonospace: "code"
    else: raise newUnexpectedKindError(tree)

  w.ewrap(tag, [], exp.exportAllUsing(w, tree, conf))

proc exportTable*(exp, w, tree, conf) =
  let t = tree.table

  w.ewrapl("table", []): w.eindent:
    for row in t.rows:
      w.ewrapl("tr", []): w.eindent:
        for cell in row.cells:
          w.writeIndent()
          w.ewrap("th", []):
            exportUsing(exp, w, cell.body, conf)
          w.line()

proc newOrgHtmlExporter*(): OrgHtmlExporter =
  result = OrgHtmlExporter(
    name: "html-base",
    fileExt: "html",
    description: "Base html exporter",
  )

  result.onExport orgAllKinds:
    w.writeRaw("<!-- UNHANDLED KIND" & $tree.kind & "-->")

  result.onExport orgNewline:
    w.writeRaw(" \n")

  result.onExport orgRawText:
    w.writeRaw(tree.strVal())

  result.onExport orgLink:
    let l = tree.link
    case l.kind:
      of olkFile:
        let f = l.linkFile
        case f.category:
          of ofcBitmapImage:
            var ops: seq[(string, string)]
            ops.add("src", f.getAbs().getStr())
            var iw, ih: Option[OrgDimensions]
            for p in tree.properties:
              case p.kind:
                of opkAttrImg:
                  iw = p.image.width 
                  ih = p.image.height

                of opkColumnSpec:
                  iw = p.cellSpec.width
                  ih = p.cellSpec.height

                of opkToplevel: discard

                else:
                  raise newImplementKindError(p)

            if iw.canGet(w): ops.add("width", $w.toUnits(odkPx).int)
            if ih.canGet(h): ops.add("height", $h.toUNits(odkPx).int)

            w.etag("img", ops)

          else:
            discard

      else:
        w.writeRaw($l.kind)

  result.impl[orgTable] = exportTable
  result.impl[orgSubtree]     = exportSubtree
  result.impl[orgDocument]    = exportDocument
  result.onExport orgStmtList:
    for item in tree:
      if item of {orgLink}: w.writeIndent()
      exportUsing(exp, w, item, conf)

      if item of {orgLink}: w.line()

  result.impl[orgWord]        = exportWord
  result.impl[orgSrcCode]     = exportSrcCode
  result.impl[orgParagraph]   = exportParagraph
  result.impl[orgList]        = exportList
  result.impl[orgMarkupKinds] = exportMarkup

method exportTo*(exp, tree; target: AbsFile; conf: RunConf) =
  var w = newXmlWriter(target)
  exportUsing(exp, w, tree, conf)
  w.close()
