import
  hmisc/core/all,
  hmisc/other/oswrap

import
  std/[strformat, options, macros]

import
  ../defs/[org_types, impl_org_node, impl_sem_org]

type
  RootExporter* = ref object of Exporter
    description*: string ## Target backend description
    fileExt*: string

  ConverterCb*[Exp, Writer] = proc(
    exp: Exp, writer: var Writer, node: SemOrg, runConfig: RunConf)

  ExportDispatcher*[E, W] = object
    exports: array[OrgNodeKind, ConverterCb[E, W]]
    fallback: seq[tuple[capture: set[OrgNodeKind], cb: ConverterCb[E, W]]]


proc exportUsing*[E, W](
    exp: E,
    writer: var W,
    disp: ExportDispatcher[E, W],
    tree: SemOrg,
    config: RunConf
  ) =

  if not isNil(disp.exports[tree.kind]):
    disp.exports[tree.kind](exp, writer, tree, config)

  else:
    for (capture, cb) in disp.fallback:
      if tree.kind in capture:
        cb(exp, writer, tree, config)
        break

proc `[]=`*[E, W](
    disp: var ExportDispatcher[E, W],
    kind: OrgNodeKind, cb: ConverterCb[E, W]
  ) =

  disp.exports[kind] = cb

proc `[]=`*[E, R](
    disp: var ExportDispatcher[E, R],
    kinds: set[OrgNodeKind],
    cb: ConverterCb[E, R]
  ) =

  for kind in kinds:
    disp[kind] = cb


proc getBackendDir*(conf: RunConf, exp: RootExporter): AbsDir =
  conf.tempDir / exp.name

import std/macros

macro createOnExport*(exporter, writer: untyped{nkIdent}): untyped =
  quote do:
    template onExport*(
      exporter {.inject.}: `exporter`,
      nodeKind: OrgNodeKind | set[OrgNodeKind],
      body: untyped
    ): untyped =

      exporter.impl[nodeKind] =
        proc(
          exp {.inject.}: `exporter`,
          w {.inject.}: var `writer`,
          tree {.inject.}: SemOrg,
          conf {.inject.}: RunConf
        ) =

          body
