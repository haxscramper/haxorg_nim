import hmisc/other/oswrap
import std/[strformat]
import semorg
import hmisc/hdebug_misc

type
  OrgExporter* = ref object of RootObj
    description*: string ## Target backend description
    name*: string
    fileExt*: string

  OrgExportDispatcher* = object
    exporters: seq[OrgExporter] ## List of exporters. Each MUST have unique
    ## `name` field.



method exportTo*(
    exporter: OrgExporter,
    tree: SemOrg,
    target: var string,
    runConfig: RunConfig = defaultRunConfig,
  ) {.base.} =

  raiseAssert(
    "No overide for string exporting has been implemented for " &
      &"exporter. name: '{exporter.name}', fileExt: '{exporter.fileExt}', " &
      &"description: '{exporter.description}'."
  )


method exportTo*(
    exporter: OrgExporter,
    tree: SemOrg,
    target: AbsFile,
    runConfig: RunConfig = defaultRunConfig,
  ) {.base.} =

  raiseAssert(
    "No overide for file exporting has been implemented for " &
      &"exporter. name: '{exporter.name}', fileExt: '{exporter.fileExt}', " &
      &"description: '{exporter.description}'."
  )


var defaultExportDispatcher*: OrgExportDispatcher

proc register*(exp: OrgExporter) =
  defaultExportDispatcher.exporters.add exp

proc exportTo*(
    dispatcher: OrgExportDispatcher,
    tree: SemOrg,
    target: AbsFile,
    config: RunConfig = defaultRunConfig,
  ) =

  ## Automatically dispatch
  let ext = target.ext()
  echov "Exporting to ext", ext
  echov dispatcher.exporters.len
  for exp in dispatcher.exporters:
    echov exp.fileExt
    if exp.fileExt == ext:
      exp.exportTo(tree, target, config)
