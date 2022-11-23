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

# let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline.org"))
let tree = orgParse(readFile("/tmp/timeline.org"))

let sem = toSemOrg(tree, nil)

writeFile("/tmp/res", tree.treeRepr().toString(false))

var gantt: seq[tuple[start: DateTime, text: seq[string]]]

proc getStartDate(tree: Subtree): DateTime =
  tern(
    tree.title[0] of orgTimeStamp,
    tree.title[0].time,
    tree.title[0][0].time
  )
    

proc withConverter[T](conv: T, node: SemOrg): T =
  result = conv
  conv.call(node)
  
proc titleText(tree: Subtree): string = 
  var sem = newSem(orgParagraph, nil)
  sem.subnodes = tree.title[1..^1]
  var conv = newUltraplainTextConverter()
  conv.call(sem)
  result = conv.res.multiReplace({ "()": "", "wiki": "" }).strip()
 
for node in itemsDFS(sem):
  if node of orgSubtree:
    let tree = node.subtree
    if tree.title.first() of { orgTimeRange, orgTimeStamp }:
      let time = tree.title.first()
      var starts: string
      if tree.level == 1:
        starts = "[ $# ] starts $#" % [
          tree.titleText(),
          tree.getStartDate().format("yyyy-MM-dd") ]

      else:
        let parent = node.parent.subtree
        let days = inDays(
          tree.getStartDate() -
          node.parent.subtree.getStartDate())
        
        starts = "[ $# ] starts $# days $# [ $# ]'s start" % [
          tree.titleText(),
          $abs(days),
          tern(0 < days, "after", "before"),
          parent.titleText()
        ]

      var date: DateTime
      var ends: string
      if time of orgTimeStamp:
        date = tern(time of orgTimestamp, time.time, time[0].time)
        ends = "[ $# ] lasts 1 day" % [tree.titleText() ]

      else:
        date = time[0].time
        if tree.level == 1:
          ends = "[ $# ] ends $#" % [
            tree.titleText(), time[1].time.format("yyyy-MM-dd") ]

        else:
          ends = "[ $# ] lasts $# days" % [
            tree.titleText(),
            $inDays(time[1].time - time[0].time)
          ]

      var note = ""
      if tree.description.canGet(description):
        let desc = newUltraplainTextConverter().withConverter(
          tree.description.get()).res.dedent().strip()

        note = "note bottom\n$#\nend note" % [ desc ]


      gantt.add((date, @[starts, note, ends]))


let start = dateTime(2006, Month(7), MonthDayRange(1))
var events: seq[string]
for (date, elements) in gantt:
  if date < start: continue
  events.add elements
  
let res = """
@startgantt
<style>
ganttDiagram {
	task {
        FontName Iosevka
    }
    timeline {
        FontName Iosevka
    }
    note {
        FontName Iosevka
    }
}
</style>

Project starts $#

$#

@endgantt
""" % [
    start.format("yyyy-MM-dd"),
    events.join("\n")
  ]

writeFile("/tmp/gantt/timeline.puml", res)
