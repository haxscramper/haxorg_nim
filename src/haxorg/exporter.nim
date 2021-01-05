import hmisc/other/oswrap
import semorg

type
  OrgExporter* = ref object of RootObj
    description*: string ## Target backend description
    name*: string
    fileExt*: string

  OrgExportDispatcher* = object
    exporters: seq[OrgExporter] ## List of exporters. Each MUST have unique
    ## `name` field.



method exportTo(
    exporter: OrgExporter,
    tree: SemOrg,
    runConfig: RunConfig,
    target: var string
  ) {.base.} =

  raiseAssert("#[ IMPLEMENT ]#")


method exportTo(
    exporter: OrgExporter,
    tree: SemOrg,
    runConfig: RunConfig,
    target: AbsFile
  ) {.base.} =

  raiseAssert("#[ IMPLEMENT ]#")


var defaultExportDispatcher*: OrgExportDispatcher


proc exportTo*(
    dispatcher: OrgExportDispatcher,
    tree: SemOrg,
    config: RunConfig,
    target: AbsFile
  ) =
  ## Automatically dispatch
  let ext = target.ext()
  for exp in dispatcher.exporters:
    if exp.fileExt == ext:
      exp.exportTo(tree, config, target)
