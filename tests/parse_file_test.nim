import hmisc/preludes/unittest
import hmisc/core/all
import haxorg/[org_diff, types, parser]
import hmisc/other/[oswrap, hshell]
import std/strutils

let assets = relToSource"corpus"

var specs: seq[TestFile]

startHax()

for file in walkDir(AbsDir(assets), AbsFile):
  if "small_subtree" notin $file:
    continue

  var spec = parseTestFile(file.readFile())
  spec.filename = file.splitFile().name
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
  if spec.expected.isNil():
    let file = getAppTempDir() /. (spec.filename & ".el")
    writeFile(file, spec.parsed.toSexp().pretty())
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

      echov spec.name
      let data = explainGraphvizDiff(cmp)
      var conf = initGraphvizFormat[OrgNode]()
      conf.formatKind = proc(kind: int): string = $OrgNodeKind(kind)
      conf.formatLink = proc(node: OrgNode, idx: int): Option[string] =
        orgSubnodeFieldName(node, idx)

      conf.formatValue = proc(value: OrgNode): string =
        if value of orgTokenKinds and
           not (value of { orgEmpty }):

          let str = value.strVal()
          if '\n' in str:
            result.add "\l" & str.replace("\n", "\l")

          else:
            result.add "\\\""
            result.add str
            result.add "\\\""

      let
        format = formatGraphvizDiff(cmp, data, conf)
        file = getAppTempDir() /. (spec.filename & ".dot")
        image = getAppTempDir() /. (spec.filename & ".png")

      writeFile(file, format)
      shellCmd("dot", "-Tpng", $file, "-o", $image).execShell()
      echov "wrote difference to", image
