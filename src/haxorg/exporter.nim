import hmisc/other/oswrap
import std/[strformat, options]
import semorg, ast
import hmisc/hdebug_misc

type
  OrgExporter* = ref object of RootObj
    description*: string ## Target backend description
    name*: string
    fileExt*: string

  OrgExportDispatcher* = object
    exporters: seq[OrgExporter] ## List of exporters. Each MUST have unique
    ## `name` field.

  ConverterCb*[Res] = proc(
    exp: OrgExporter, node: SemOrg, runConfig: RunConfig): Option[Res]

  ExportDispatcher*[Res] = object
    exports: array[OrgNodeKind, ConverterCb[Res]]
    fallback: seq[tuple[capture: set[OrgNodeKind], cb: ConverterCb[Res]]]


proc exportUsing*[R](
    exp: OrgExporter, disp: ExportDispatcher[R],
    tree: SemOrg, config: RunConfig
  ): Option[R] =

  if not isNil(disp.exports[tree.kind]):
    return disp.exports[tree.kind](exp, tree, config)

  else:
    for (capture, cb) in disp.fallback:
      if tree.kind in capture:
        return cb(exp, tree, config)

proc `[]=`*[R](disp: var ExportDispatcher[R], kind: OrgNodeKind, cb: ConverterCb[R]) =
  disp.exports[kind] = cb

proc `[]=`*[R](disp: var ExportDispatcher[R], kinds: set[OrgNodeKind], cb: ConverterCb[R]) =
  for kind in kinds:
    disp.exports[kind] = cb

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

  var str: string

  exportTo(exporter, tree, str, runConfig)
  target.writeFile(str)

  # raiseAssert(
  #   "No overide for file exporting has been implemented for " &
  #     &"exporter. name: '{exporter.name}', fileExt: '{exporter.fileExt}', " &
  #     &"description: '{exporter.description}'."
  # )


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
