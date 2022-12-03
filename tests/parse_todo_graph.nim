import haxorg/[types, parser, semorg]
import hmisc/other/[hshell, oswrap]
import std/[strutils, sequtils]
import haxorg/exporter/exporter_ultraplain
import hmisc/algo/hlex_base
import hmisc/core/all
startHax()
let
  parse = orgParse(readFile(relToSource"assets/todo_graph.org"))
  sem = toSemOrg(parse, nil)
  doc = sem.toDocument()

proc text(node: SemOrg): string =
  newUltraplainTextExporter().withExporter(node).res

proc isBlockerLink(sem: SemOrg): bool =
  if not (sem of orgSubtree): return false
  let link = sem.subtree.title[0]
  if link of orgLink and
     sem.subtree.getProperty("blocker").canGet(prop):
    return true

proc isTodoItem(sem: SemOrg): bool =
  if not (sem of orgSubtree): return false
  sem.getTodo().isSome()

proc dotText(str: string): string =
  str.replace("\n", "\\l")

let c = defaultSemOrgReprConf - sorfSkipParagraph

proc toDotId(id: string): string =
  for ch in id:
    case ch:
      of {' ', '-'}:
        result.add "_"

      of AsciiLetters + {'_'}:
        result.add ch

      of Digits:
        result.add "_"
        result.add ch

      else:
        result.add "t"

proc treeId(sem: SemOrg): string =
  result = tern(
    sem.getId().isSome(),
    sem.getId().get().toDotid(),
    sem.subtree.title.text().toDotId()
  )

  assert not result.empty(), $sem.treeRepr(c)



proc recTree(sem: SemOrg): seq[string] =
  assert sem of orgSubtree, $sem.treeRepr(c)
  assert not sem.isBlockerLink(), $sem.treeRepr(c)

  if sem.isTodoItem():
    block ordered_link:
      if sem.parentTree().canGet(parent):
        if parent.subtree.getProperty("ordered").isSome() and
           sem.getPrevNode().canGet(prev):
          result.add "$# -> $#;" % [
            prev.treeid(),
            sem.treeId()
          ]

        elif parent.isTodoItem():
          result.add "$# -> $#;" % [
            parent.treeid(),
            sem.treeId()
          ]

    
    var desc: seq[string]
    for sub in sem:
      if sub.isBlockerLink():
        let link = sub.subtree.title[0].link.linkId
        result.add "$# -> $#[label=\"$#\"];" % [
          link,
          sem.treeId(),
          newSem(
            orgParagraph, SemOrg(nil), toSeq(sub)).text().strip().dotText()
        ]

      elif sub.isTodoItem():
        result.add recTree(sub)

      else:
        desc.add sub.text()

    desc.insert ""
    desc.insert sem.subtree.title.text()
    result.add "$#[label=\"$#\\l\", color=$#];" % [
      sem.treeId(),
      desc.join("\n").strip().dotText(),
      case sem.getTodo().get():
        of obiTodo: "red"
        of obiDone: "green"
        of obiWip: "yellow"
        else: "magenta"
    ]

  else:
    result.add "subgraph cluster_$# {" % sem.treeId()
    result.add "color=white;"
    var label: seq[string]
    for sub in sem:
      if not (sub of orgSubtree):
        label.add sub.text()

    result.add "fontcolor=white;"
    result.add "label=\"$#\\l\";" % [
      label.join("\n").strip().dotText()
    ]

    for sub in sem:
      if sub of orgSubtree:
        result.add sub.recTree()

    result.add "}"

var result: seq[string]
for node in sem:
  if node of orgSubtree:
    result.add recTree(node)


let graph = """
digraph G {
    node[shape=rect,style=filled,fontname=iosevka,color=white,penwidth=2];
    edge[fontname=iosevka,penwidth=2,color=white,fontcolor=white];
    graph[fontname=iosevka,bgcolor=black];
    nodesep=0.8;
    rankdir=LR;
    splines=polyline;
$#
}
""" % [
  result.join("\n")
]

let file = getAppTempFile("res.dot")
file.writeFile(graph)

let outf = "-o$#" % $file.withExt("png")
shellCmd("dot", "-Tpng", $file, $outf).execShell()
