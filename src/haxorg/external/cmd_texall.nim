import hmisc/other/[hshell, oswrap]
import std/[htmlparser, strformat]

const baseCmd = makeShellCmd("latexmk", "-", "=")

proc texCompile*(infile: AbsFile) =

  var cmd = baseCmd
  # cmd - ("interaction", "nonstopmode")
  cmd - "pdf"
  # cmd - "c"
  cmd.arg $infile

  echo cmd.toStr()
  withDir infile.dir:
    execShell cmd
