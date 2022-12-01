import std/tables
import haxorg/[types, parser, semorg]
export types, parser, semorg
import hmisc/core/all

type
  OrgConv* = proc(sem: SemOrg, obj: Exporter)

  Exporter* = ref object of RootObj
    impls*: Table[OrgNodeKind, OrgConv]

func `[]=`*(conv: Exporter, kind: OrgNodeKind, impl: OrgConv) =
  conv.impls[kind] = impl

template addImpl*[T](mainConv: T, kind: OrgNodeKind, body: untyped) =
  block:
    mainConv[kind] = OrgConv(
      proc(node {.inject.}: SemOrg, conv: Exporter) =
        var conv {.inject.} = T(conv)
        body
    )

template addImpl*[T](mainConv: T, kinds: set[OrgNodeKind], body: untyped) =
  block:
    for kind in kinds:
      mainConv[kind] = OrgConv(
        proc(node {.inject.}: SemOrg, conv: Exporter) =
          var conv {.inject.} = T(conv)
          body
      )


proc call*(conv: Exporter, node: SemOrg) =
  assert node.kind in conv.impls, $node.kind
  conv.impls[node.kind](node, conv)

proc subcall*(conv: Exporter, node: SemOrg) =
  for sub in node:
    conv.call(sub)

proc withExporter*[T](conv: T, node: SemOrg): T =
  result = conv
  conv.call(node)
