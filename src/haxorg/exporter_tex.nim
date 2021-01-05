import exporter, semorg
import hmisc/other/[oswrap, hshell]
import hmisc/hdebug_misc
import hmisc/helpers

type
  TexEngine = enum
    tePdfLatex
    teLuaLatex


  OrgTexExporter = ref object of OrgExporter
    engine: TexEngine

  OrgTexPdfExporter = ref object of OrgTexExporter

register(OrgTexPdfExporter(fileExt: "pdf", description: "Pdf exporter"))
register(OrgTexExporter(fileExt: "tex", description: "Latex exporter"))

using
  exp: OrgTexExporter
  conf: RunConfig
  tree: SemOrg

method exportMain(exp, tree; target: var string; conf) =
  target = """
\documentclass[12pt]{article}
\begin{document}
Hello world
\end{document}
"""

method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  echov "Exporting tree to string"
  exportMain(exp, tree, target, conf)


method exportTo*(
  exp, tree; target: AbsFile; conf: RunConfig = defaultRunConfig) =

  var buf: string
  exp.exportTo(tree, buf, conf)
  target.writeFile(buf)

method exportTo*(
  exp: OrgTexPdfExporter, tree; target: AbsFile; conf: RunConfig = defaultRunConfig) =

  echov "Exporting to pdf"
  let tmpDir: AbsDir = getNewTempDir()
  let tmpFile: AbsFile = tmpDir.getTempFile("XXXXXX.tex")

  withDir tmpDir:
    procCall exportTo(OrgTexExporter(exp), tree, tmpFile, conf)

    let cmd = makeShellCmd("pdflatex", "-", "=").withIt do:
      it - ("interaction", "nonstopmode")
      it.arg tmpFile

    try:
      cmd.execShell()
    except ShellError:
      echo readFile(tmpFile)
      raise
