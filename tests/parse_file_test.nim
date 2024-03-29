import hmisc/preludes/unittest
import std/algorithm
import hmisc/core/all
import hmisc/algo/hlex_base
import std/sequtils
import haxorg/[org_diff, types, parser]
import haxorg/lexer
import hmisc/other/[oswrap, hshell]
import hmisc/algo/hseq_distance
import std/strutils
import std/strformat

let assets = relToSource"corpus"

var specs: seq[TestFile]

startHax()

let target = "subtrees/"

var relFiles: seq[RelFile]
for relFile in walkDir(AbsDir(assets), RelFile, recurse = true):
  relFiles.add relFile

relFiles.sort()



proc resFile(spec: TestFile, extra: string): AbsFile =
  result = getAppTempDir() /. (spec.filename & extra)
  # echov result
  # echov result.dir()
  mkDir(result.dir())
    
for relFile in relFiles:
  let file = AbsDir(assets) / relFile
  if not target.empty() and target notin file.string:
    continue

  var spec = parseTestFile(file.readFile())
  spec.filename = relFile.withoutExt().string
  mkDir(getAppTempDir() / relFile.dir())
  specs.add(spec)


for spec in mitems(specs):
  echov "lexing", spec.filename
  case spec.lexerTestMode:
    of TLFull:
      for token in orgLex(
        spec.givenRaw, getLexConf(spec.resFile("_lexer.log"))):
        spec.lexed.add toTest(token)

    of TLStructure:
      var str = initPosStr(spec.givenRaw)
      for token in lexAll(
        str, lexStructure(getLexConf(spec.resFile("_lexer.log")))):
        spec.lexed.add toTest(token)

  var file = open(spec.resFile("_lexed.txt").string(), fmWrite)
  for idx, token in spec.lexed:
    file.writeLine(&"[{idx:<4}]: {token.toSexp()}")

  file.close()


for spec in mitems(specs):
  echov "parsing", spec.filename
  spec.parsed = orgParse(
    spec.givenRaw,
    spec.resFile("_parser.log").getParseConf(),
    spec.resFile("_lexer_full.log").getLexConf()
  )

  writeFile(
    spec.resFile("_parsed.el"),
    spec.parsed.toSexp().toOrgCompact())


mkdir getAppTempDir()

for spec in specs:
  echov spec.name, spec.filename
  if not spec.tokens.empty():
    if spec.lexed != spec.tokens:
      let diff = myersDiff[TestToken](
        spec.tokens,
        spec.lexed,
        proc(lhs, rhs: TestToken): bool = lhs == rhs
      )

      let shifted = shiftDiffed[TestToken](diff, spec.tokens, spec.lexed)
      let format = formatDiffed[TestToken](
        shifted = shifted,
        oldSeq = spec.tokens,
        newSeq = spec.lexed,
        strConv = proc(tok: TestToken): string = $tok,
        showLines = true
      )

      writeFile(
        spec.resFile("_tokens.diff"), format.toString(color = false))
      echov "lexer content difference"

  if spec.expected.notNil():
    writeFile(
      spec.resFile("_expected.el"),
      spec.expected.toSexp().toOrgCompact())

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
