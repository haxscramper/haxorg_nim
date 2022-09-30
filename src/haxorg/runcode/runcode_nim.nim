import std/[strformat, strutils, with, tables, algorithm]

import
  ./runcode_root,
  ../defs/[org_types, impl_org_node]

import
  hmisc/core/all,
  hmisc/other/[oswrap, hshell, hpprint, hargparse]

type
  NimBackendKind = enum
    nbkC
    nbkCpp
    nbkJs
    nbkNims

  NimCodeBlock = ref object of RootCodeBlock
    backend: NimBackendKind


proc newNimCodeBlock*(): NimCodeBlock =
  result = NimCodeBlock(
    langName: "nim",
    blockArgs: newBlockCliParser("nim", "Execute nim code block"))

  addRootBlockArgs(result.blockArgs)

  with result.blockArgs:
    add opt(":fail", "Failure modes", check = cliCheckFor(
      CodeEvalFailPolicy, {
        "t": "Code is expected to fail"
    }))

proc assembleNimCode(
    cb: RootCodeBlock, node: OrgNode,
    scope: var SemOrgCtx
  ) =


  var code: seq[string]
  for line in node:
    code.add ""
    for part in line:
      case part.kind:
        of orgCodeText:
          code.last().add part.strVal()

        of orgCodeCallout, orgEmpty:
          discard

        else:
          raise newImplementKindError(part)

  cb.code = code.join("\n")


proc parseNimBlockArgs*(cb: NimCodeBlock, noparent: CliOptionsTable): CliOptionsTable =
  for name, opt in noparent:
    case name:
      of ":fail": echov opt.treeRepr()
      else: result[name] = opt

method parseFrom*(
    codeBlock: NimCodeBlock, node: OrgNode,
    scope: var SemOrgCtx
  ) =
  readBlockArgs(codeBlock, node, scope)
  let extra = parseNimBlockArgs(codeBlock, parseBaseBlockArgs(codeBlock))
  chainSession(codeBlock, scope)
  assembleNimCode(codeBlock, node["body"], scope)

type
  JoinedSrc = object
    idxInSession: int
    code: string
    sections: seq[(string, CodeBlock)]


proc getFullCode(cb: RootCodeBlock): JoinedSrc =
  var blocks: seq[CodeBlock]
  var prev = CodeBlock(cb)
  while notNil(prev):
    blocks.add prev
    prev = prev.prevInSession

  reverse(blocks)

  result.idxInSession = blocks.high

  for idx, bl in pairs(blocks):
    result.sections.add($idx, bl)
    result.code.add &"""
#
static: echo {idxBeginStr(idx, "c")}
echo {idxBeginStr(idx, "r")}
{bl.code}
static: echo {idxEndStr(idx, "c")}
echo {idxEndStr(idx, "r")}
#
"""

method runCode*(
    cb: NimCodeBlock,
    context: var CodeRunContext,
    conf: OrgConf
  ) =

  let dir = conf.getLangDir(cb)

  let
    src = cb.getFullCode()
    outFile = dir /. &"block_file_{src.idxInSession}.nim"
    outBin = dir /. &"block_file_{src.idxInSession}.bin"
    outCache = dir / "cache"

  let cmd = makeNimShellCmd("nim").withIt do:
    it.cmd "compile"
    it.opt "skipUserCfg", "on"
    it.opt "skipProjCfg", "on"
    it.opt "skipParentCfg", "on"
    it.opt "hints", "off"
    it.opt "verbosity", "0"
    it.opt "nimcache", outCache
    it.opt "o", $outBin
    it.arg $outFile

  mkDir dir
  writeFile outFile, src.code

  var compileRes = shellResult(cmd)
  compileRes.cutForSession(src.idxInSession, "c")

  cb.execResult = some CodeResult(
    compileResult: some compileRes
  )

  if compileRes.resultOk:
    let evalCmd = makeNimShellCmd($outBin)
    var evalRes = shellResult(evalCmd)
    evalRes.cutForSession(src.idxInSession, "r")
    cb.execResult.get().execResult = some evalRes


method blockPPtree*(
    codeBlock: NimCodeBlock, conf: var PPrintConf): PPrintTree =
  pptree(codeBlock, conf)
