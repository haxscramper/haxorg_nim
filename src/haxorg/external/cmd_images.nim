import hmisc/other/[hshell, oswrap]
import hmisc/core/all
import std/strutils

proc getImageSize*(image: AbsFile): tuple[pixw, pixh: int] =
  let res = evalShellStdout shellCmdX11("identify").withIt do:
    it - "format"
    it.arg "%w %h"
    it.arg $image

  let split = res.split(" ")
  result.pixw = split[0].parseInt()
  result.pixh = split[1].parseInt()
