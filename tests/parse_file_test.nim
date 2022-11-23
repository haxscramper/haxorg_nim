import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[org_diff, types, parser]
import hmisc/other/[oswrap, hshell]
import std/strutils

let assets = relToSource"corpus"

var specs: seq[TestFile]

startHax()

for file in walkDir(AbsDir(assets), AbsFile):
  var spec = parseTestFile(file.readFile())
  spec.filename = file.splitFile().name
  let
    lex = orgLex(spec.givenRaw)
    outf = getAppTempDir() /. (file.name() & ".el")
    f = open(outf.string, fmWrite)

  for idx, tok in lex:
    f.writeline($idx, " ", $tok)
  f.close()


  # echov spec.filename, "-----------"
  spec.parsed = orgParse(lex)
  specs.add(spec)

mkdir getAppTempDir()

for spec in specs:
  echov spec.name
  if spec.expected.isNil():
    let file = getAppTempDir() /. (spec.filename & ".el")
    writeFile(file, spec.parsed.toSexp().toOrgCompact())
    echov "wrote expected to ", file

  else:
    let cmp = diff(spec.parsed, spec.expected, minHeight = 2)
    if cmp.hasChanges():
      proc aux(n: OrgNode): string =
        result = "RealNode{\"$#\", $#" % [
          if n of orgTokenKinds: n.strVal() else: "",
          $n.kind.int
        ]

        if not (n of orgTokenKinds):
          result.add ", {"
          for idx, sub in n:
            if 0 < idx:
              result.add ", "
            result.add aux(sub)
          result.add "}"
        result.add "}"

      if false:
        echo "parsed: ",  spec.parsed.aux()
        echo "expected: ", spec.expected.aux()

      if false:
        echov spec.parsed.treeRepr()
        echov spec.expected.treeRepr()

      let image = getAppTempDir() /. (spec.filename & ".png")
      diffOrg(
        spec.parsed,
        spec.expected,
        image,
        true
      )
      echov "wrote difference to", image

    else:
      echov "Parse ok, no difference"
