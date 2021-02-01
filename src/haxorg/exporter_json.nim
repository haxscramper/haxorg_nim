import exporter, semorg, ast, buf
import std/[uri]
import hmisc/other/[hjson, hshell, oswrap]

type
  OrgJsonExporter = ref object of OrgExporter


register(OrgJsonExporter(
  name: "json-base",
  fileExt: "json",
  description: "Base json exporter"
))


using
  exp: OrgJsonExporter
  conf: RunConfig
  tree: SemOrg

proc toJson*(sbuf: StrSlice): JsonNode = newJString($sbuf)
proc toJson*(input: OrgProperty | OrgNode): JsonNode =
  toJson(input[])

proc toJson*(input: ShellError): JsonNode =
  toJson({
    "cmd" :     toJson(input.cmd),
    "cwd" :     toJson(input.cwd.getStr()),
    "retcode" : toJson(input.retcode),
    "errstr" :  toJson(input.errstr),
    "outstr" :  toJson(input.outstr)
  })

proc toJson*(url: Url): JsonNode = toJson(url.string)

method toJson*(cblock: CodeBlock): JsonNode = toJson(cblock[])


proc toJson*(tree): JsonNode =
  if tree.isNil():
    result = newJNull()

  else:
    result = toJson(
      tree[], @["symTable", "isGenerated", "node", "subnodes", "kind"])

  if tree.subnodes.len > 0:
    result["tree"] = %[toJson(tree.kind), toJson(tree.subnodes)]


  if not tree.isGenerated:
    case tree.kind:
      of orgTokenKinds:
        let text = $tree.node.text
        if text.len > 0:
          result["text"] = toJson(text)

      of {onkNowebMultilineBlock, onkSnippetMultilineBlock}:
        discard

      else:
        if tree.node.str.len > 0:
          result["str"] = toJson(tree.node.str)

method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  target = $toJson(target)
