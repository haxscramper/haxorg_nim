import hmisc/preludes/unittest
import std/times
import hmisc/core/all
import haxorg/[types, parser, semorg]
import std/sequtils
import std/strutils

type
  OrgConv = proc(sem: SemOrg, obj: Converter)

  Converter = ref object of RootObj
    impls*: Table[OrgNodeKind, OrgConv]

func `[]=`*(conv: Converter, kind: OrgNodeKind, impl: OrgConv) =
  conv.impls[kind] = impl

template addImpl*[T](mainConv: T, kind: OrgNodeKind, body: untyped) =
  block:
    mainConv[kind] = OrgConv(
      proc(node {.inject.}: SemOrg, conv: Converter) =
        var conv {.inject.} = T(conv)
        body
    )

template addImpl*[T](mainConv: T, kinds: set[OrgNodeKind], body: untyped) =
  block:
    for kind in kinds:
      mainConv[kind] = OrgConv(
        proc(node {.inject.}: SemOrg, conv: Converter) =
          var conv {.inject.} = T(conv)
          body
      )


proc call*(conv: Converter, node: SemOrg) =
  assert node.kind in conv.impls, $node.kind
  conv.impls[node.kind](node, conv)

proc subcall*(conv: Converter, node: SemOrg) =
  for sub in node:
    conv.call(sub)

type
  CreoleConverter = ref object of Converter
    res: string

proc newCreoleConverter(): CreoleConverter =
  result = CreoleConverter()
  result.addImpl(orgParagraph):
    for sub in node: conv.call(sub)

  result.addImpl({orgLink, orgRawLink}):
    conv.res.add "LINK:TODO"

  result.addImpl({orgSpace, orgWord}): conv.subcall(node)

  result.addImpl(orgQuote):
    conv.res.add '"'
    conv.subcall(node)
    conv.res.add '"'

  result.addImpl(orgQuote):
    conv.res.add '='
    conv.subcall(node)
    conv.res.add '='

  result.addImpl(orgBold):
    conv.res.add "*"
    conv.subcall(node)
    conv.res.add "*"

type
  UltraplainTextConverter = ref object of Converter
    res: string

proc newUltraplainTextConverter(): UltraplainTextConverter =
  result = UltraplainTextConverter()
  result.addImpl(not orgTokenKinds): conv.subcall(node)
  result.addImpl(orgTokenKinds): conv.res.add node.strVal()
  result.addImpl(orgLink):
    assertRef(node)
    assertRef(node.link.description)
    conv.call(node.link.description)

let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline.org"))
# let tree = orgParse(readFile("/tmp/timeline.org"))

let sem = toSemOrg(tree, nil)

writeFile("/tmp/res", tree.treeRepr().toString(false))

var gantt: seq[tuple[start: DateTime, text: seq[string]]]

var mindate = dateTime(99999, Month(1), MonthdayRange(1))
for node in itemsDFS(sem):
  if node of orgSubtree and node.subtree.level == 1:
    let tree = node.subtree
    let elements = toSeq(tree.title)
    if elements[0] of { orgTimeRange, orgTimeStamp }:
      var sem = newSem(orgParagraph, nil)
      sem.subnodes = elements[1..^1]
      var conv = newUltraplainTextConverter()
      conv.call(sem)
      let time = elements[0]
      let text = conv.res.multiReplace({ "()": "" }).strip()

      if time of orgTimeStamp or (time[1].time <= time[0].time):
        let date = tern(time of orgTimestamp, time.time, time[1].time)
        mindate = min(mindate, date)

        gantt.add((
          date,
          @[
            "[ $# ] starts $#" % [text, date.format("yyyy-MM-dd") ],
            "[ $# ] lasts 1 day" % [ text ]
          ]
        ))

      else:
        mindate = min(mindate, time[0].time)
        mindate = min(mindate, time[1].time)
        gantt.add((
          time[0].time,
          @[
            "[ $# ] starts $#" % [ text, time[0].time.format("yyyy-MM-dd") ],
            "[ $# ] ends $#" % [ text, time[1].time.format("yyyy-MM-dd") ]
          ]
        ))


var buckets: seq[tuple[start: int, elements: seq[string]]]

let start = 2006
let step = 10
let stop = 2025

for i in 0 .. ((stop - start) div step):
  buckets.add((start + step * i), newSeq[string]())

proc toBucket(entry: DateTime, elements: seq[string]) =
  for item in mitems(buckets):
    if entry.year in item.start ..< item.start + step:
      item.elements.add elements

for (date, elements) in gantt:
  toBucket(date, elements)

for (start, elements) in buckets:
  if elements.empty(): break
  let res = """
@startgantt
Project starts $#-01-01

$#

@endgantt
""" % [
    $start,
    elements.join("\n")
  ]

  echov start
  writeFile("/tmp/gantt/gantt-$#.puml" % $start, res)
  echov res
