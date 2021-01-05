import exporter

type
  TexEngine = enum
    tePdfLatex
    teLuaLatex


  OrgTexExporter = ref object of OrgExporter
    engine: TexEngine


  OrgTexPdfExporter = ref object of OrgTexExporter




method exportTo(
    exporter: OrgTexExporter,
    runConfig: RunConfig,
    target: var string
  ) =

  raiseAssert("#[ IMPLEMENT ]#")


method exportTo(
    exporter: OrgTexExporter,
    runConfig: RunConfig,
    target: AbsFile
  ) =

  var buf: string

  exporter.exportTo(buf)

  writeFile(buf, target)

method exportTo(
    exporter: OrgTexPdfExporter,
    runConfig: RunConfg,
    target: AbsFile
  )

  let tmpDir: AbsDir = runConfig.getTmpDir()
  let tmpFile: AbsFile = tmpDir.getTmpFile()

  withDir tmpDir:
    OrgTexExporter(exporter).exportTo(tmpFile)

    let cmd = makeX11ShellCmd("pdflatex").withIt do:
      it - tmpFile



    cmd.exec()
