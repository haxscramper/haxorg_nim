import semorg
import std/[tables]

type
  OrgRunner = ref object of RootObj
    lang*: string
    hasSession*: bool

  CodeRunDispatcher* = object
    implTable: Table[string, OrgRunner]

method run(
    runner: OrgRunner, runConf: RunConfig, code: SemOrg
  ): CodeResult {.base.} =

  raiseAssert("#[ IMPLEMENT ]#")

proc runCodeBlocks*(
    tree: var SemOrg, conf: RunConfig, runDispater: CodeRunDispatcher
  ) =

  ## Execute all code blocks in tree and set result fields for each node.

  discard

var defaultRunDispatcher*: CodeRunDispatcher
