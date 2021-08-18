# Template file for new exportters, replace `` with exporter name

import ast, buf, exporter, semorg, common
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import fusion/matching
import std/[options, strformat, strutils]


#===========================  Type defintions  ===========================#

type
  OrgMarkupExporter* = ref object of OrgExporter
    listDepth*: int
    markupMap*: array[OskMarkupKindsRange, (string, string)]
    impl*: ExportDispatcher[OrgMarkupExporter, string]

#=============================  Boilerplate  =============================#

using
  exp: OrgMarkupExporter
  tree: SemOrg
  conf: RunConfig


proc exportUsing*(exp, tree, conf): Option[string] =
  exp.exportUsing(exp.impl, tree, conf)

proc exportAllUsing*(exp, tree, conf; sep: string = "\n"): string =
  for node in tree:
    if Some(@exported) ?= exportUsing(exp, node, conf):
      result &= exported & sep


#======================  Exporter implementations  =======================#

proc exportMarkup*(exp, tree, conf): string =
  let (op1, op2) = exp.markupMap[tree.subKind]
  result = op1 & exportAllUsing(exp, tree, conf) & op2


proc exportStmtList*(exp, tree, conf): string =
  result = exportAllUsing(exp, tree, conf)

proc exportList*(exp, tree, conf): string =
  inc exp.listDepth
  result = exportAllUsing(exp, tree, conf, "")
  dec exp.listDepth

proc exportListItem*(exp, tree, conf): string =
  let pref = repeat("  ", exp.listDepth)

  result = pref & tree.itemBullet & " "

  for part in ["tag", "header", "body"]:
    if not tree[part].isEmptyNode():
      result &= exportUsing(exp, tree[part], conf) & " "

  result &= "\n"

#============================  Constructors  =============================#

proc newOrgMarkupExporter*(): OrgMarkupExporter =
  result = OrgMarkupExporter(
    name: "markup-base",
    fileExt: "",
    description: "Base exporter for pdf files",
  )

  result.impl[orgAllKinds] = proc(exp, tree, conf): auto =
    some treeRepr(tree, false)

  result.impl[onkStmtList] = exportStmtList
  result.impl[onkList] = exportList
  result.impl[onkListItem] = exportListItem
  result.impl[onkWord] = proc(exp, tree, conf): string =
    $tree.node.text

  result.impl[onkMarkup] = exportMarkup

  result.impl[onkParagraph] = proc(exp, tree, conf): string =
    exportAllUsing(exp, tree, conf, "")


  result.impl[onkDocument] = proc(exp, tree, conf): string =
    for node in tree:
      result &= exportUsing(exp, node, conf) & "\n"


#=========================  Register exporters  ==========================#

register(newOrgMarkupExporter())


method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  target = exportUsing(exp, tree, conf).get()
