import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[org_diff, types, parser]
import hmisc/other/oswrap

let assets = relToSource"corpus"

var specs: seq[TestFile]

startHax()

for file in walkDir(AbsDir(assets), AbsFile):
  echov file
  var spec = parseTestFile(file.readFile())
  let lex = orgLex(spec.givenRaw)
  let outf = getAppTempDir() /. (file.name() & ".el")
  echov outf
  let f = open(outf.string, fmWrite)
  for idx, tok in lex:
    f.writeline($idx, " ", $tok)
  f.close()


  spec.parsed = orgParse(lex)
  specs.add(spec)
  echov "done"

for spec in specs:
  if spec.expected.isNil():
    discard
    # echo spec.parsed.treeRepr()

  else:
    let cmp = diff(spec.parsed, spec.expected)
    echo explainDiff(cmp, fromDst = true)

