import hmisc/core/all

import
  std/[options, strformat, strutils, osproc, streams, sequtils],

  ../defs/[org_types, impl_org_node, impl_sem_org, impl_context, impl_org_types],
  ../external/[cmd_pygmentize, cmd_texall, cmd_images],
  ./exporter_root,

  hmisc/hasts/latex_writer,
  hmisc/other/[oswrap, hshell, hpprint],
  hmisc/algo/tree/tree_selector,
  hmisc/algo/halgorithm

#===========================  Type defintions  ===========================#

type
  TexEngine = enum
    tePdfLatex
    teLuaLatex

  TexColorize = enum
    tcNoColor
    tcColorPygmentize
    tcColorBat

  OrgTexExporter = ref object of RootExporter
    engine: TexEngine
    colorize: TexColorize
    impl: ExportDispatcher[OrgTexExporter, LatexWriter]
    level: int

  OrgTexPdfExporter = ref object of OrgTexExporter

#=============================  Boilerplate  =============================#

createOnExport(OrgTexExporter, LatexWriter)

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

proc getTarget(tree): string =
  &"{tree.kind}_at_{tree.node.line}_{tree.node.column}"


proc exportDocument*(exp, w, tree, conf) =
  w.cmd "documentclass", ["12pt"], ["article"]
  w.cmd "usepackage", ["float"]
  w.cmd "usepackage", ["hyperref"]
  w.cmd "usepackage", ["graphicx"]


  case exp.colorize:
    of tcColorPygmentize:
      w.cmd "usepackage", [], ["pygmentex"]
      w.writeRaw pygmentizeGetTexStyle()
      w.line()

    else:
      discard

  w.env "document", []:
    exportAllUsing(exp, w, tree, conf)

    # let listings = tree.execWithCtx(
    #   semTreeSelector,
    #   predicate(ctx, it.kind == orgSrcCode))

    # # TODO construct list of org-mode items instead of hacking together
    # # output
    # for code in listings:
    #   w.writeRaw r"\hyperref[", code.getTarget(), "]{"
    #   exportALlUsing(exp, w, code.getAssoc({orgCommandCaption}), conf)
    #   w.writeRaw "}"



proc exportStmtList*(exp, w, tree, conf) = exportAllUsing(exp, w, tree, conf)

proc exportSubtree*(exp, w, tree, conf) =
  w.line()
  let tag =
    case tree.subtree.level:
      of 1: "section"
      of 2: "subsection"
      of 3: "subsubsection"
      of 4: "paragraph"
      else: raise newImplementError($tree.subtree.level)

  w.writeIndent()
  w.writeRaw r"\", tag, "{"
  exportAllUsing(exp, w, tree["title"], conf)
  w.writeRaw "}"
  w.line()

  w.indent()
  exportAllUsing(exp, w, tree["body"], conf)
  w.dedent()

  w.line()
  w.line()


proc exportParagraph*(exp, w, tree, conf) =
  w.writeIndent()
  exportAllUsing(exp, w, tree, conf)
  w.line()

proc escapeLatex*(str: string): string =
  for ch in str:
    case ch:
      of '_', '#', '@', '$':
        result.add "\\"
        result.add ch

      else:
        result.add ch

proc exportWord*(exp, w, tree, conf) =
  w.writeRaw tree.node.strVal().escapeLatex()

proc exportLink*(exp, w, tree, conf) =
  let l = tree.link
  case l.kind:
    of olkFile:
      let f = l.linkFile
      case f.category:
        of ofcBitmapImage:
          var ops: seq[string]
          var iw, ih: Option[OrgDimensions]
          for p in tree.properties:
            case p.kind:
              of opkAttrImg:
                iw = p.image.width
                ih = p.image.height

              of opkColumnSpec:
                iw = p.cellSpec.width
                ih = p.cellSpec.height

              of opkToplevel:
                discard

              else:
                raise newImplementKindError(p)

          if iw.canGet(w): ops.add(&"width = {w.toUnits(odkCm, some 96):.3f}cm")
          if ih.canGet(h): ops.add(&"height = {h.toUnits(odkCm, some 96):.3f}cm")

          let top = tree.properties.anyIt(it of opkToplevel)
          if ?ops:
            w.cmd("includegraphics", [ops.join(", ")], [$f.getAbs()], inline = not top)

          else:
            w.cmd("includegraphics", [$f.getAbs()], inline = not top)


        else:
          discard

    else:
      w.writeRaw($l.kind)

proc exportList*(exp, w, tree, conf) =
  w.env "itemize", []:
    for item in tree:
      exportUsing(exp, w, item, conf)
      w.line()

proc exportListItem*(exp, w, tree, conf) =
  w.writeRaw r"\item "
  exportAllUsing(exp, w, tree["header"], conf)
  w.line()
  exportAllUsing(exp, w, tree["body"], conf)

proc exportMarkup*(exp, w, tree, conf) =
  if tree.kind in { orgMonospace, orgBacktick }:
    w.writeRaw"\verb!"
    exportAllUsing(exp, w, tree, conf)
    w.writeRaw"!"

  else:
    let tag =
      case tree.kind:
        of orgBold: "textbf"
        of orgItalic: "textit"
        else: raise newUnexpectedKindError(tree)

    w.writeRaw "\\", tag, "{"
    exportAllUsing(exp, w, tree, conf)
    w.writeRaw "}"

proc exportSrcCode*(exp, w, tree, conf) =
  let caption = tree.getAssoc({orgCommandCaption})
  if caption.len > 0:
    w.cmd "begin", [], ["figure"], "[H]"
    w.line()

  case exp.colorize:
    of tcNoColor:
      w.env "verbatim", []:
        w.writeRaw tree.codeBlock.code.strip()
        w.line()

    of tcColorPygmentize:
      # https://ctan.org/pkg/pygmentex?lang=en
      w.writeRaw pygmentizeToTex(tree.codeBlock.code.strip(), "nim")
      w.line()

    of tcColorBat:
      discard

  if caption.len > 0:
    w.writeRaw"\caption{"
    for cap in caption:
      exportAllUsing(exp, w, cap, conf)
    w.writeRaw"}"

    w.cmd "end", ["figure"]
    w.line()

  # w.writeRaw r"\phantomsection\label{", tree.getTarget(), "}THE DESTINATION\n"


  if ?tree["eval-result"]:
    w.env "verbatim", []:
      w.writeRaw tree["eval-result"].strVal().strip()
      w.line()


  # if tree.codeBlock.
  # w.env "verbatim", []:



#============================  Constructors  =============================#

proc newOrgTexExporter*(): OrgTexExporter =
  result = OrgTexExporter(
    name: "html-base",
    fileExt: "html",
    description: "Export document to tex",
  )

  result.onExport orgAllKinds:
    w.writeRaw $tree.kind

  result.onExport orgCommandCaption:
    exportUsing(exp, w, tree[0], conf)

  result.onExport orgNewline:
    w.writeRaw " "

  result.onExport orgRawText:
    w.writeRaw tree.strVal().escapeLatex()

  result.onExport orgEmpty:
    discard



  result.onExport orgCommandInclude:
    let i = tree.includeSpec
    case i.kind:
      of oikExportInclude:
        if "tex" in i.backend:
          raise newImplementError()

        else:
          discard

      else:
        raise newImplementKindError(i)


  result.onExport orgTable:
    let t = tree.table

    var format: string

    for idx in 0 .. t.columnCount:
      let last = idx == t.columnCount
      format.add case t.intercols[idx]:
        of ocsNone: tern(last, "", "c")
        of ocsSingleLine: tern(last, "|", "|c")
        of ocsDoubleLine: tern(last, "||", "||c")

    w.flatEnv "center", []:
      w.env "tabular", [format]:
        for row in t.rows:
          for isLast, cell in itemsIsLast(row.cells):
            w.indent()
            exportUsing(exp, w, cell.body, conf)
            w.dedent()

            if not isLast:
              w.writeIndRaw("&")
              w.line()

          w.writeIndRaw(r"\\")
          w.line()

          case row.afterrow:
            of ocsNone: discard
            of ocsSingleLine: w.cmd "hline"
            of ocsDoubleLine:
              w.cmd "hline"
              w.cmd "hline"

  result.impl[orgWord]        = exportWord
  result.impl[orgParagraph]   = exportParagraph
  result.impl[orgDocument]    = exportDocument
  result.impl[orgStmtList]    = exportStmtList
  result.impl[orgSubtree]     = exportSubtree
  result.impl[orgList]        = exportList
  result.impl[orgLink]        = exportLink
  result.impl[orgListItem]    = exportListItem
  result.impl[orgMarkupKinds] = exportMarkup
  result.impl[orgSrcCode]     = exportSrcCode

proc newOrgTexPdfExporter*(): OrgTexPdfExporter =
  result = OrgTexPdfExporter(
    name: "latex_pdf",
    fileExt: "pdf",
    description: "Export PDF using latex")

  result.impl = newOrgTexExporter().impl

method exportTo*(exp, tree; target: AbsFile; conf: RunConf) =
  var w = newLatexWriter(target)
  exportUsing(exp, w, tree, conf)
  w.close()

method exportTo*(exp: OrgTexPdfExporter, tree; target: AbsFile; conf: RunConf) =

  let
    name = "tmp_tex.tex"
    tmpDir = conf.getBackendDir(exp)
    tmpFile = tmpDir /. name
    tmpRes = tmpDir /. "tmp_tex.pdf"

  mkDir tmpDir
  echov tmpFile
  var w = newLatexWriter(tmpFile)
  exportUsing(exp, w, tree, conf)
  w.close()

  let cmd = makeShellCmd("latexmk", "-", "=").withIt do:
    it.opt "interaction", "nonstopmode"
    it.flag "pdf"
    it.flag "shell-escape"
    it.arg $tmpFile


  withDir tmpDir:
    let compile = shellResult(cmd)
    let code = compile.execResult.code

    # https://tex.stackexchange.com/questions/225005/pdflatex-exitcode-when-running-with-timeout
    #
    # The codes reported by latexmk are those reported by Perl's system
    # function. As stated in the documentation of this function,
    # http://perldoc.perl.org/functions/system.html, these codes are the
    # exit code of the executed program shifted left by 8 bits. (There is
    # some extra information in the low order bits.) Therefore, to get the
    # actual exit code of the executed program, just divide the exit code
    # reported by latexmk by 256.

    if code == 0:
      echov target
      cpFile tmpRes, target

    else:
      raise compile.exception
