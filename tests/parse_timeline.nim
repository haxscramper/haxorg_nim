import hmisc/preludes/unittest
import std/times
import hmisc/core/all
import haxorg/[types, parser, semorg]
import std/sequtils
import std/strutils
import haxorg/exporter/[exporter_ultraplain]

proc getStartDate(tree: Subtree): DateTime =
  tern(
    tree.title[0] of orgTimeStamp,
    tree.title[0].time,
    tree.title[0][0].time
  )
    

proc titleText(tree: Subtree): string = 
  var sem = newSem(orgParagraph, nil)
  sem.subnodes = tree.title[1..^1]
  var conv = newUltraplainTextExporter()
  conv.call(sem)
  result = conv.res.multiReplace({ "()": "", "wiki": "" }).strip()

let tree = orgParse(readFile("/mnt/workspace/repos/fic/wiki/timeline.org"))
# let tree = orgParse(readFile("/tmp/timeline.org"))

let sem = toSemOrg(tree, nil)

writeFile("/tmp/res", tree.treeRepr().toString(false))
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

        else:
          raise newUnexpectedKindError(key)

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
        let desc = newUltraplainTextExporter().withExporter(
          tree.description.get()).res.dedent().strip()

        note = "note bottom\n$#\nend note" % [ desc ]


      gantt.add((date, @[starts, note, ends] & format(node)))


let start = dateTime(2006, Month(6), MonthDayRange(1))
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
Sundays are colored in Lavender/LightBlue
Saturdays are colored in Lavender/LightBlue

$#

@endgantt
""" % [
    start.format("yyyy-MM-dd"),
    events.join("\n")
  ]

writeFile("/tmp/gantt/timeline.puml", res)
