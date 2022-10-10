import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[org_diff, types, parser]
import hmisc/other/oswrap

let assets = relToSource"corpus"

var specs: seq[TestFile]

startHax()

for file in walkDir(AbsDir(assets), AbsFile):
  var spec = parseTestFile(file.readFile())
  spec.parsed = orgParse(spec.givenRaw)
  specs.add(spec)

for spec in specs:
  if not spec.expected.isNil():
    let cmp = diff(spec.parsed, spec.expected)
    echo explainDiff(cmp, fromDst = true)
