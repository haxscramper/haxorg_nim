version       = "0.1.2"
author        = "haxscramper"
description   = "Org-mode parser for nim"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.6.0"
requires "hmisc >= 0.12.4"

task test, "Run tests":
  exec "nim r tests/runall.nim"
