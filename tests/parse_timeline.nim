import hmisc/preludes/unittest
import std/times
import hmisc/core/all
import haxorg/[types, parser, semorg]
import std/sequtils
import std/strutils
import haxorg/exporter/[exporter_ultraplain]

const orgTimeKinds = { orgTimeRange, orgTimeStamp }

# TODO average convex time for all subtrees
# TODO average time for the tree
# TODO total extent, format duration -- export property (for convex nodes)
# TODO self exent, format duration

proc getStartDate(tree: Subtree): Option[DateTime] =
  if tree.title.notEmpty() and tree.title[0] of orgTimeKinds:
    result = some tern(
      tree.title[0] of orgTimeStamp,
      tree.title[0].time,
      tree.title[0][0].time
    )

proc getSelfExtent(
  tree: Subtree): tuple[minTime, maxTime: Option[DateTime]] =
  let title = tree.title
  if title.notEmpty() and title.first() of orgTimeKinds:
    if title.first() of orgTimeStamp:
      result.minTime = some title[0].time

    else:
      result.minTime = some title[0][0].time
      result.maxTime = some title[0][1].time

proc titleText(tree: Subtree): string =
  var sem = newSem(orgParagraph, nil)
  sem.subnodes = if tree.title.first() of orgTimeKinds:
                   tree.title[1..^1]

                 else:
                   tree.title[0..^1]

  var conv = newUltraplainTextExporter()
  conv.call(sem)
  result = conv.res.multiReplace({ "()": "", "wiki": "" }).strip()

proc testPlantumlGanttConvexExtent(sem: SemOrg): bool =
  result = true
  let prop = sem.subtree.getProperty("export_options", "plantuml/gantt")
  if prop.canGet(prop) and "convex-time:nil" in prop.exportParameters:
    result = false

proc getConvexExtent(tree: SemOrg): Option[tuple[minTime, maxTime: DateTime]] =
  for time in tree.nestedLeavesDFS(orgTimeKinds):
    var times: seq[DateTime]
    if time of orgTimeRange:
      times.add time[0].time
      times.add time[1].time

    else:
      times.add time.time

    for time in times:
      if result.isNone():
        result = some((time, time))

      else:
        result.get().minTime = min(result.get().minTime, time)
        result.get().maxTime = max(result.get().maxTime, time)


# let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline.org"))
# let tree = orgParse(readFile("/tmp/timeline.org"))
let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline_real.org"))

let sem = toSemOrg(tree, nil)

writeFile("/tmp/parsed.nim", tree.treeRepr().toString(false))
mkDir(AbsDir("/tmp/gantt"))

var gantt: seq[tuple[start: DateTime, text: seq[string]]]

let document = sem.toDocument()

proc format(sem: SemOrg): seq[string] =
  let tree = sem.subtree
  let prop = sem.getContextualProperty("export_options", "plantuml/gantt")
  if "story##arc" in tree.tags:
    result.add "[ $# ] is colored in GreenYellow/Green" % [
      sem.subtree.titleText()
    ]

  if prop.isSome():
    # TODO move this somewhere else, implement a generalized solution that
    # creates a table of strings.
    for param in prop.get().exportParameters.split(" "):
      let split = param.find(':')
      let key = tern(split == -1, param[0 .. ^1], param[0 ..< split])
      let value = tern(split == -1, "", param[split + 1 .. ^1])
      case key:
        of "same-level":
          if value == "prev":
            if sem.getPrevNode().canGet(prev):
              result.add "[ $# ] displays on same row as [ $# ]" % [
                sem.subtree.titleText(),
                prev.subtree.titleText()
              ]

          elif value.startsWith("[["):
            let parsed = orgParse(value)[0][0]
            assert parsed of orgLink, $parsed.treeRepr()
            let semLink = toSemOrg(parsed, nil)
            if document.getLinked(semLink.link).canGet(tree):
              result.add "[ $# ] displays on same row as [ $# ]" % [
                sem.subtree.titleText(),
                tree.subtree.titleText()
              ]

        of "convex-time":
          discard

        else:
          raise newUnexpectedKindError(key)

for node in itemsDFS(sem):
  if node of orgSubtree:
    let tree = node.subtree

    var
      starts: string
      start: Option[DateTime]
      parentStart: Option[DateTime]
      parent = node.getParentSubtree()

    if tree.getStartDate().canGet(it):
      start = some it

    elif node.testPlantumlGanttConvexExtent() and
         node.getConvexExtent().canGet(extent):
      start = some extent.minTime

    if parent.canGet(parent):
      if parent.subtree.getStartDate().canGet(it):
        parentStart = some it

      elif parent.testPlantumlGanttConvexExtent() and
           parent.getConvexExtent().canGet(extent):
        parentStart = some extent.minTime

    if start.canGet(start):
      if parentStart.canGet(parentStart):
        let days = inDays(start - parentStart)
        starts = "[ $# ] starts $# days $# [ $# ]'s start" % [
          tree.titleText(),
          $abs(days),
          tern(0 < days, "after", "before"),
          parent.get().subtree.titleText()
        ]

      else:
        starts = "[ $# ] starts $#" % [
          tree.titleText(),
          start.format("yyyy-MM-dd") ]


    var (minTime, maxTime) = tree.getSelfExtent()
    if minTime.isNone() and
       maxTime.isNone() and
       node.testPlantumlGanttConvexExtent() and
       node.getConvexExtent().canGet(extent):

      minTime = some extent.minTime
      maxTime = some extent.maxTime

    var ends = ""
    if minTime.canGet(minTime) and maxTime.canGet(maxTime):
      if tree.level == 1:
        ends = "[ $# ] ends $#" % [
          tree.titleText(), maxTime.format("yyyy-MM-dd") ]

      else:
        ends = "[ $# ] lasts $# days" % [
          tree.titleText(),
          $inDays(maxTime - minTime)
        ]

    else:
      ends = "[ $# ] lasts 1 day" % [ tree.titleText() ]

    var note = ""
    if tree.description.canGet(description):
      let desc = newUltraplainTextExporter().withExporter(
        tree.description.get()).res.dedent().strip()

      note = "note bottom\n$#\nend note" % [ desc ]

    if minTime.canGet(minTime):
      gantt.add((minTime, @[starts, note, ends] & format(node)))


let start = dateTime(1900, Month(6), MonthDayRange(1))
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
projectscale yearly
Project starts $#
Sundays are colored in Lavender/LightBlue
Saturdays are colored in Lavender/LightBlue

$#

@endgantt
""" % [
    start.format("yyyy-MM-dd"),
    events.join("\n")
  ]

writeFile("/tmp/gantt/timeline.puml", res)
