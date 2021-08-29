import std/strformat

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
  NimCodeBlock(langName: "nim")

method parseFrom*(
  codeBlock: NimCodeBlock, semorg: OrgNode, scope: seq[TreeScope]) =
  echov "Parsing nim code block"
  procCall parseFrom(RootCodeBlock(codeBlock), semorg, scope)

method runCode*(codeBlock: NimCodeBlock, context: var CodeRunContext) =
  echo "running nim code block"

method blockPPtree*(
    codeBlock: NimCodeBlock, conf: var PPrintConf): PPrintTree =
  pptree(codeBlock, conf)
