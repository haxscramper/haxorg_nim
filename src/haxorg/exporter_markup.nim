# Template file for new exportters, replace `` with exporter name

import ast, buf, exporter, semorg
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import fusion/matching
import std/[options, strformat]


#===========================  Type defintions  ===========================#

type
  OrgMarkupExporter* = ref object of OrgExporter
    indent*: int
    markupMap*: array[OskMarkupKindsRange, (string, string)]
    impl*: ExportDispatcher[OrgMarkupExporter, string]

#=============================  Boilerplate  =============================#

using
  exp: OrgMarkupExporter
  tree: SemOrg
  conf: RunConfig


proc exportUsing*(exp, tree, conf): Option[string] =
  exp.exportUsing(exp.impl, tree, conf)

proc exportAllUsing*(exp, tree, conf): string =
  for node in tree:
    if Some(@exported) ?= exportUsing(exp, node, conf):
      result &= exported

#======================  Exporter implementations  =======================#

proc exportMarkup*(exp, tree, conf): string =
  let (op1, op2) = exp.markupMap[tree.subKind]
  result = op1 & exportAllUsing(exp, tree, conf) & op2


#============================  Constructors  =============================#

proc newOrgMarkupExporter*(): OrgMarkupExporter =
  result = OrgMarkupExporter(
    name: "markup-base",
    fileExt: "",
    description: "Base exporter for pdf files",
  )

  result.impl[orgAllKinds] = proc(exp, tree, conf): auto =
    some treeRepr(tree, false)


  result.impl[onkMarkup] = exportMarkup

  result.impl[onkDocument] = proc(exp, tree, conf): string =
    "document"


#=========================  Register exporters  ==========================#

register(newOrgMarkupExporter())


method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  target = exportUsing(exp, tree, conf).get()
