import hmisc/other/oswrap
import std/[strformat, options, macros]
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

  ConverterCb*[Exp, Res] = proc(
    exp: Exp, node: SemOrg, runConfig: RunConfig): Option[Res]

  ExportDispatcher*[E, R] = object
    exports: array[OrgNodeKind, ConverterCb[E, R]]
    fallback: seq[tuple[capture: set[OrgNodeKind], cb: ConverterCb[E, R]]]


proc exportUsing*[E, R](
    exp: E, disp: ExportDispatcher[E, R],
    tree: SemOrg, config: RunConfig
  ): Option[R] =

  if not isNil(disp.exports[tree.kind]):
    return disp.exports[tree.kind](exp, tree, config)

  else:
    for (capture, cb) in disp.fallback:
      if tree.kind in capture:
        return cb(exp, tree, config)

proc `[]=`*[E, R](
    disp: var ExportDispatcher[E, R],
    kind: OrgNodeKind, cb: ConverterCb[E, R]
  ) =

  disp.exports[kind] = cb

proc `[]=`*[E, R](
    disp: var ExportDispatcher[E, R],
    kind: OrgNodeKind,
    cb: proc(exp: E, tree: SemOrg, conf: RunConfig): R
  ) =

  disp.exports[kind] =
    proc(exp: E, tree: SemOrg, conf: RunConfig): Option[R] =
      some cb(exp, tree, conf)

proc `[]=`*[E, R](
    disp: var ExportDispatcher[E, R],
    kinds: set[OrgNodeKind], cb: ConverterCb[E, R]
  ) =

  for kind in kinds:
    disp[kind] = cb


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
  for exp in dispatcher.exporters:
    if exp.fileExt == ext:
      exp.exportTo(tree, target, config)


proc exportTo*(
    dispatcher: OrgExportDispatcher,
    tree: SemOrg,
    name: string,
    config: RunConfig = defaultRunConfig,
  ): string =

  for exp in dispatcher.exporters:
    if name == exp.fileExt:
      exp.exportTo(tree, result, config)

proc exportUsing*(
    dispatcher: OrgExportDispatcher,
    name: string,
    tree: SemOrg,
    target: AbsFile,
    config: RunConfig = defaultRunConfig
  ) =

  for exp in dispatcher.exporters:
    if exp.name == name:
      exp.exportTo(tree, target, config)
