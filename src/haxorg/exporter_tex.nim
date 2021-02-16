import exporter, semorg
import hmisc/other/[oswrap, hshell]
import hmisc/hdebug_misc
import hmisc/helpers
import fusion/matching
import std/[options, strformat]
import ast, buf

import cmd_texall, cmd_pygmentize

#===========================  Type defintions  ===========================#

type
  TexEngine = enum
    tePdfLatex
    teLuaLatex


  OrgTexExporter = ref object of OrgExporter
    engine: TexEngine
    impl: ExportDispatcher[OrgTexExporter, string]
    level: int

  OrgTexPdfExporter = ref object of OrgTexExporter

#=============================  Boilerplate  =============================#

using
  exp: OrgTexExporter
  tree: SemOrg
  conf: RunConfig

proc exportUsing(exp, tree, conf): Option[string] =
  exp.exportUsing(exp.impl, tree, conf)

proc `add`*(str: var string, opt: Option[string]) =
  if opt.isSome():
    str.add opt.get()

proc exportAllUsing*(exp, tree, conf): string =
  inc exp.level

  for node in tree:
    if Some(@exported) ?= exportUsing(exp, node, conf):
      result &= exported

  dec exp.level

#======================  Exporter implementations  =======================#

proc exportDocument*(exp, tree, conf): string =
  result = """
\documentclass[12pt]{article}
\usepackage{pygmentex}
"""

  result &= pygmentizeGetTexStyle() & "\n"

  result &= """
\begin{document}
"""

  result &= exportAllUsing(exp, tree, conf)

  result &= """
\end{document}
"""

proc exportStmtList*(exp, tree, conf): string =
  result = exportAllUsing(exp, tree, conf)

proc exportSubtree*(exp, tree, conf): string =
  let tag =
    case tree.subtLevel:
      of 1: "section"
      of 2: "subsection"
      of 3: "subsubsection"
      of 4: "paragraph"
      else: ""

  result &= "\\" & tag
  result &= "{"
  result &= exportUsing(exp, tree["title"], conf)
  result &= "}\n"
  result &= exportUsing(exp, tree["body"], conf)
  result &= "\n"

  # echo treeRepr(tree)

proc exportParagraph*(exp, tree, conf): string =
  exportAllUsing(exp, tree, conf)
  # var buf: seq[string]
  # for node in tree:
  #   buf.add

  # result = join(buf, " ")

proc exportWord*(exp, tree, conf): string = $tree.node.text
proc exportLink*(exp, tree, conf): string = $tree.linkTarget.kind
proc exportList*(exp, tree, conf): string =
  result = "\n\\begin{itemize}\n"

  for item in tree:
    result &= exportUsing(exp, item, conf)
    result &= "\n"

  result &= "\\end{itemize}\n"

proc exportListItem*(exp, tree, conf): string =
  "  \\item " & exportAllUsing(exp, tree["header"], conf) & " " &
    exportAllusing(exp, tree["body"], conf)

proc exportMarkup*(exp, tree, conf): string =
  if tree.subKind  in {oskMonospaced, oskBacktick}:
    result = "\\verb!" & exportAllUsing(exp, tree, conf) & "!"

  else:
    let tag =
      case tree.subKind:
        of oskBold: "textbf"
        of oskItalic: "textit"
        else: subKindErr(tree.subKind)

    result = "\\" & tag & "{" & exportAllUsing(exp, tree, conf) & "}"

proc exportMetaTag*(exp, tree, conf): string =
  " \\verb!" & $tree["body"][0].node.text & "!"

proc exportSrcCode*(exp, tree, conf): string =
  # https://ctan.org/pkg/pygmentex?lang=en
  result = "\n" & pygmentizeToTex(tree.codeBlock.code.strip(), "nim") & "\n"


#============================  Constructors  =============================#

proc newOrgTexExporter*(): OrgTexExporter =
  result = OrgTexExporter(
    name: "html-base",
    fileExt: "html",
    description: "Export document to tex",
  )


  result.impl[orgAllKinds] =
    proc(exp; tree; conf): auto =
      if tree.len > 0:
        some &"""
\begin{{verbatim}}
{treeRepr(tree, colored = false)}
\end{{verbatim}}
"""
      else:
        some $tree.kind

  result.impl[onkWord] = exportWord
  result.impl[onkParagraph] = exportParagraph
  result.impl[onkDocument] = exportDocument
  result.impl[onkStmtList] = exportStmtList
  result.impl[onkSubtree] = exportSubtree
  result.impl[onkLink] = exportLink
  result.impl[onkList] = exportList
  result.impl[onkListItem] = exportListItem
  result.impl[onkMarkup] = exportMarkup
  result.impl[onkMetaTag] = exportMetaTag
  result.impl[onkSrcCode] = exportSrcCode

proc newOrgTexPdfExporter*(): OrgTexPdfExporter =
  result = OrgTexPdfExporter(
    name: "tex-pdf",
    fileExt: "pdf",
    description: "Export PDF using latex"
  )

  result.impl = newOrgTexExporter().impl

#=========================  Register exporters  ==========================#

register(newOrgTexExporter())
register(newOrgTexPdfExporter())


method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  target &= exportUsing(exp, tree, conf)

method exportTo*(
  exp, tree; target: AbsFile; conf: RunConfig = defaultRunConfig) =

  var buf: string
  exp.exportTo(tree, buf, conf)
  target.writeFile(buf)

method exportTo*(
  exp: OrgTexPdfExporter, tree; target: AbsFile; conf: RunConfig = defaultRunConfig) =

  let target = target.withExt("tex")

  var buf: string
  procCall exportTo(OrgTexExporter(exp), tree, target, conf)


  echo readFile(target)

  texCompile(target)



  # echov "Exporting to pdf"
  # let tmpDir: AbsDir = getNewTempDir()
  # let tmpFile: AbsFile = tmpDir.getTempFile("XXXXXX.tex")

  # withDir tmpDir:
  #   procCall exportTo(OrgTexExporter(exp), tree, tmpFile, conf)

  #   let cmd = makeShellCmd("pdflatex", "-", "=").withIt do:
  #     it - ("interaction", "nonstopmode")
  #     it.arg tmpFile

  #   try:
  #     cmd.execShell()
  #   except ShellError:
  #     echo readFile(tmpFile)
  #     raise
