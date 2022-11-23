import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[org_diff, types, parser]
import hmisc/other/[oswrap, hshell]
import std/strutils

let assets = relToSource"corpus"

var specs: seq[TestFile]

startHax()

let target = ""

for file in walkDir(AbsDir(assets), AbsFile):
  if not target.empty() and target notin file.string:
    continue

  var spec = parseTestFile(file.readFile())
  spec.filename = file.splitFile().name
  echov spec.filename
  let
    lex = orgLex(spec.givenRaw)
    outf = getAppTempDir() /. (file.name() & ".el")
    f = open(outf.string, fmWrite)

  for idx, tok in lex:
    f.writeline($idx, " ", $tok)
  f.close()

  spec.parsed = orgParse(lex)
  specs.add(spec)

mkdir getAppTempDir()

for spec in specs:
  echov spec.name
  let file = getAppTempDir() /. (spec.filename & "_parsed" & ".el")
  writeFile(file, spec.parsed.toSexp().toOrgCompact())

  if spec.expected.notNil():
    let file = getAppTempDir() /. (spec.filename & "_expected" & ".el")
    writeFile(file, spec.expected.toSexp().toOrgCompact())

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


echo "completed"
