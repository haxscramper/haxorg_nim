import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[org_diff, types, parser]
import hmisc/other/oswrap

let assets = relToSource"corpus"

var specs: seq[TestFile]

startHax()

for file in walkDir(AbsDir(assets), AbsFile):
  var spec = parseTestFile(file.readFile())
  let
    lex = orgLex(spec.givenRaw)
    outf = getAppTempDir() /. (file.name() & ".el")
    f = open(outf.string, fmWrite)

  for idx, tok in lex:
    f.writeline($idx, " ", $tok)
  f.close()


  spec.parsed = orgParse(lex)
  specs.add(spec)

for spec in specs:
  if spec.expected.isNil():
    discard
    # echo spec.parsed.treeRepr()

  else:
    let cmp = diff(spec.parsed, spec.expected)
    echo explainDiff(cmp, fromDst = true)
    let data = explainGraphvizDiff(cmp)
    var conf = defaultGraphvizFormat
    conf.formatKind = proc(kind: int): string = $OrgNodeKind(kind)
    let format = formatGraphvizDiff(cmp, data, conf)
    writeFile("/tmp/file.dot", format)
