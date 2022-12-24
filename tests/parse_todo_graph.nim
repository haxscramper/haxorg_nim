import haxorg/[types, parser, semorg]
import hmisc/other/[hshell, oswrap]
import std/[strutils, sequtils]
import haxorg/exporter/exporter_ultraplain
import hmisc/algo/hlex_base
import hmisc/core/all

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

proc toSafeIdent*(id: string): string =
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

proc escapeDot*(str: string): string =
  str.multiReplace({
    "\n": "\\l",
    "\"": "\\\""
  })

proc getTreeDescription*(
    sem: SemOrg,
    firstParagraphAsDescription: bool = true
  ): Option[SemOrg] =
  ## Get tree `:description:` content or the first paragraph if present.
  if sem.subtree.description.canGet(desc):
    return some desc

  elif firstParagraphAsDescription:
    if 0 < len(sem) and sem[0] of {orgParagraph, orgAnnotatedParagraph}:
      return some sem[0]

proc getTreeBody*(sem: SemOrg): SemOrg =
  result = newSem(orgStmtList, sem)
  for node in sem:
    if not(node of orgSubtree):
      result.add node

proc getSafeTreeIdImage*(
    sem: SemOrg,
    idClean: proc(id: string): string = toSafeIdent
  ): string =
  ## Map an org tree to unique identifier image. This can't be used to
  ## uniquely refer back to the original tree (aside from mapping all known
  ## trees and then iterating over) and instead intended for automatic
  ## conversion options.
  let id = sem.getId()
  if id.isSome():
    # If explicit ID is present use it and return immediately
    result.add idClean(id.get())

  else:
    # Otherwise build the tree up from it's name pieces.
    if sem.parentTree().canGet(parent):
      result = getSafeTreeIdImage(parent)
      result.add "_"

    result = idClean(sem.subtree.title.text())

  assert not result.empty(), $sem.treeRepr(c)

proc getSafeIdImage*(
    note: OrgFootnote,
    idClean: proc(id: string): string = toSafeIdent
  ): string =

  if note.inline:
    result = idClean(note.text.text())

  else:
    result = idClean(note.ident)



proc recTree(sem: SemOrg): seq[string] =
  assert sem of orgSubtree, $sem.treeRepr(c)
  assert not sem.isBlockerLink(), $sem.treeRepr(c)

  if sem.isTodoItem():
    block ordered_link:
      if sem.parentTree().canGet(parent):
        if parent.subtree.getProperty("ordered").isSome() and
           sem.getPrevNode().canGet(prev):
          result.add "$# -> $#;" % [
            prev.getSafeTreeIdImage(),
            sem.getSafeTreeIdImage()
          ]

        elif parent.isTodoItem():
          result.add "$# -> $#;" % [
            parent.getSafeTreeIdImage(),
            sem.getSafeTreeIdImage()
          ]

    
    var desc: seq[string]
    for sub in sem:
      if sub.isBlockerLink():
        let link = sub.subtree.title[0].link.linkId
        result.add "$# -> $#[label=\"$#\"];" % [
          link,
          sem.getSafeTreeIdImage(),
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
      sem.getSafeTreeIdImage(),
      desc.join("\n").strip().dotText(),
      case sem.getTodo().get():
        of obiTodo: "red"
        of obiDone: "green"
        of obiWip: "yellow"
        else: "magenta"
    ]

  else:
    result.add "subgraph cluster_$# {" % sem.getSafeTreeIdImage()
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


proc compileDot*(
    dot: string,
    resfile: AbsFile = getAppTempFile("res.dot"),
    resimage: AbsFile = resfile.withExt("png")
  ) =

  resfile.writeFile(dot)

  let outf = "-o$#" % $resimage
  shellCmd("dot", "-Tpng", $resfile, $outf).execShell()


when isMainModule:
  startHax()
  let
    parse = orgParse(readFile(relToSource"assets/todo_graph.org"))
    sem = toSemOrg(parse, nil)
    doc = sem.toDocument()

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

