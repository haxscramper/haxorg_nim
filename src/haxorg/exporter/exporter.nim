import std/[tables, strutils]
import haxorg/[types, parser, semorg]
export types, parser, semorg
import hmisc/core/all

type
  OrgConv* = proc(sem: SemOrg, obj: Exporter)

  Exporter* = ref object of RootObj
    traceCalls*: bool
    traceDepth*: int
    onTraceEnter*: proc(sem: SemOrg, obj: Exporter, inst: InstInfo)
    onTraceLeave*: proc(sem: SemOrg, obj: Exporter, inst: InstInfo)

    onStartCb*: OrgConv
    onFinishCb*: OrgConv
    impls*: Table[OrgNodeKind, OrgConv]

func `[]=`*(conv: Exporter, kind: OrgNodeKind, impl: OrgConv) =
  conv.impls[kind] = impl

proc addDefaultTraceHooks*(exp: Exporter) =
  exp.onTraceEnter = proc(sem: SemOrg, obj: Exporter, inst: InstInfo) =
    echo(repeat("  ", exp.traceDepth), ">", $sem.kind, " at ", inst.line)

  exp.onTraceLeave = proc(sem: SemOrg, obj: Exporter, inst: InstInfo) =
    echo(repeat("  ", exp.traceDepth), "<")

template withTrace*(
    conv: Exporter, sem: SemOrg,
    instDepth: int = -1, body: untyped
  ): untyped =

  const loc = instantiationInfo(instDepth, false)
  if notNil(conv.onTraceEnter):
    conv.onTraceEnter(sem, conv, loc)

  body

  if notNil(conv.onTraceLeave):
    conv.onTraceLeave(sem, conv, loc)


template onFinish*[T](mainConv: T, body: untyped) =
  block:
    mainConv.onFinishCb = OrgConv(
      proc(node {.inject.}: SemOrg, conv: Exporter) =
        var conv {.inject.} = T(conv)
        body
    )

template onStart*[T](mainConv: T, body: untyped) =
  block:
    mainConv.onStartCb = OrgConv(
      proc(node {.inject.}: SemOrg, conv: Exporter) =
        var conv {.inject.} = T(conv)
        body
    )

template addImpl*[T](mainConv: T, kind: OrgNodeKind, body: untyped) =
  block:
    mainConv[kind] = OrgConv(
      proc(node {.inject.}: SemOrg, conv: Exporter) =
        withTrace(conv, node, int(-2)):
          var conv {.inject.} = T(conv)
          body
    )

template addImpl*[T](mainConv: T, kinds: set[OrgNodeKind], body: untyped) =
  block:
    for kind in kinds:
      mainConv[kind] = OrgConv(
        proc(node {.inject.}: SemOrg, conv: Exporter) =
          withTrace(conv, node, int(-2)):
            var conv {.inject.} = T(conv)
            body
      )


proc call*(conv: Exporter, node: SemOrg) =
  assert node.kind in conv.impls, $node.kind
  inc conv.traceDepth
  conv.impls[node.kind](node, conv)
  dec conv.traceDepth

proc subcall*(conv: Exporter, node: SemOrg) =
  for sub in node:
    conv.call(sub)


proc withExporter*[T](conv: T, node: SemOrg): T =
  result = conv
  if notNil(conv.onStartCb): conv.onStartCb(node, conv)
  conv.call(node)
  if notNil(conv.onFinishCb): conv.onFinishCb(node, conv)
