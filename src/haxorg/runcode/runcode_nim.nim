import std/[strformat, strutils]

import
  ./runcode_root,
  ../defs/[org_types, impl_org_node]

import
  hmisc/core/all,
  hmisc/other/[oswrap, hshell, hpprint]

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

proc assembleNimCode(cb: RootCodeBlock, node: OrgNode, scope: seq[TreeScope]) =
  var code: seq[string]
  for line in node:
    code.add ""
    for part in line:
      case part.kind:
        of orgCodeText:
          code.last().add part.strVal()

        else:
          raise newImplementKindError(part)

  cb.code = code.join("\n")

method parseFrom*(
  codeBlock: NimCodeBlock, node: OrgNode, scope: seq[TreeScope]) =
  procCall parseFrom(RootCodeBlock(codeBlock), node, scope)
  assembleNimCode(codeBlock, node["body"], scope)

method runCode*(
    cb: NimCodeBlock,
    context: var CodeRunContext,
    conf: RunConf
  ) =

  let dir = conf.getLangDir(cb)

  let
    outFile = dir /. "block_file.nim"
    outBin = dir /. "block_file.bin"
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

  echo cmd

  mkDir dir
  writeFile outFile, cb.code

  let compileRes = shellResult(cmd)

  cb.execResult = some CodeResult(
    compileResult: some compileRes
  )

  if compileRes.resultOk:
    let evalCmd = makeNimShellCmd($outBin)
    let evalRes = shellResult(evalCmd)
    cb.execResult.get().execResult = some evalRes


method blockPPtree*(
    codeBlock: NimCodeBlock, conf: var PPrintConf): PPrintTree =
  pptree(codeBlock, conf)
