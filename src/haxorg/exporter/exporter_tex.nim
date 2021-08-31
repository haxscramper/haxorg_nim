import hmisc/core/all

importx:
  std/[options, strformat, strutils]

  ../[
    defs/[org_types, impl_org_node, impl_sem_org],
    external/[cmd_pygmentize, cmd_texall]
  ]

  ./exporter_root

  hmisc/[hasts/latex_writer, other/oswrap]

#===========================  Type defintions  ===========================#

type
  TexEngine = enum
    tePdfLatex
    teLuaLatex


  OrgTexExporter = ref object of RootExporter
    engine: TexEngine
    impl: ExportDispatcher[OrgTexExporter, LatexWriter]
    level: int

  OrgTexPdfExporter = ref object of OrgTexExporter

#=============================  Boilerplate  =============================#

using
  exp: OrgTexExporter
  w: var LatexWriter
  tree: SemOrg
  conf: RunConf

proc exportUsing(exp, w, tree, conf) =
  exp.exportUsing(w, exp.impl, tree, conf)

proc exportAllUsing*(exp, w, tree, conf) =
  for node in tree:
    exportUsing(exp, w, node, conf)

#======================  Exporter implementations  =======================#

proc exportDocument*(exp, w, tree, conf) =
  w.cmd "documentclass", ["12pt"], "article"
  w.use [], "pygmentex"

  w.raw pygmentizeGetTexStyle()
  w.line()

  w.flatEnv "document", []:
    exportAllUsing(exp, w, tree, conf)

proc exportStmtList*(exp, w, tree, conf) = exportAllUsing(exp, w, tree, conf)

proc exportSubtree*(exp, w, tree, conf) =
  let tag =
    case tree.subtree.level:
      of 1: "section"
      of 2: "subsection"
      of 3: "subsubsection"
      of 4: "paragraph"
      else: ""

  w.raw "\\" & tag
  w.raw "{"
  exportUsing(exp, w, tree["title"], conf)
  w.raw "}\n"
  exportUsing(exp, w, tree["body"], conf)
  w.raw "\n"


proc exportParagraph*(exp, w, tree, conf) =
  exportAllUsing(exp, w, tree, conf)

proc exportWord*(exp, w, tree, conf) = w.raw $tree.node.text
proc exportLink*(exp, w, tree, conf) = w.raw $tree.linkTarget.kind
proc exportList*(exp, w, tree, conf) =
  w.env "itemize", []:
    for item in tree:
      exportUsing(exp, w, item, conf)
      w.line()

proc exportListItem*(exp, w, tree, conf) =
  w.raw r"\item "
  exportAllUsing(exp, w, tree["header"], conf)
  w.line()
  exportAllUsing(exp, w, tree["body"], conf)

proc exportMarkup*(exp, w, tree, conf) =
  if tree.kind in { orgMonospace, orgBacktick }:
    w.raw"\verb!"
    exportAllUsing(exp, w, tree, conf)
    w.raw"!"

  else:
    let tag =
      case tree.kind:
        of orgBold: "textbf"
        of orgItalic: "textit"
        else: raise newUnexpectedKindError(tree)

    w.raw "\\", tag, "{"
    exportAllUsing(exp, w, tree, conf)
    w.raw "}"

proc exportMetaTag*(exp, w, tree, conf) =
  w.raw r" \verb!", tree["body"][0].node.strVal(), "!"

proc exportSrcCode*(exp, w, tree, conf) =
  # https://ctan.org/pkg/pygmentex?lang=en
  w.raw pygmentizeToTex(tree.codeBlock.code.strip(), "nim")


#============================  Constructors  =============================#

proc newOrgTexExporter*(): OrgTexExporter =
  result = OrgTexExporter(
    name: "html-base",
    fileExt: "html",
    description: "Export document to tex",
  )


  result.impl[orgAllKinds] =
    proc(exp, w, tree, conf) =
      w.env "verbatim", []:
        w.raw $tree.kind

  result.impl[orgWord] = exportWord
  result.impl[orgParagraph] = exportParagraph
  result.impl[orgDocument] = exportDocument
  result.impl[orgStmtList] = exportStmtList
  result.impl[orgSubtree] = exportSubtree
  result.impl[orgLink] = exportLink
  result.impl[orgList] = exportList
  result.impl[orgListItem] = exportListItem
  result.impl[orgMarkupKinds] = exportMarkup
  result.impl[orgMetaTag] = exportMetaTag
  result.impl[orgSrcCode] = exportSrcCode

proc newOrgTexPdfExporter*(): OrgTexPdfExporter =
  result = OrgTexPdfExporter(
    name: "tex-pdf",
    fileExt: "pdf",
    description: "Export PDF using latex"
  )

  result.impl = newOrgTexExporter().impl

method exportTo*(exp, tree; target: AbsFile; conf: RunConf) =
  var w = newLatexWriter(target)
  exportUsing(exp, w, tree, conf)
