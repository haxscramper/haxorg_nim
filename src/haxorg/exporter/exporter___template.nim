import
  hmisc/core/all,

  ./exporter_root,
  ../defs/defs_all,
  ../external/[cmd_pygmentize, cmd_images],

  std/[macros, strformat, strutils, options, tables, sequtils],

  hmisc/hasts/[base_writer],
  hmisc/other/[hshell, oswrap],
  hmisc/algo/hstring_algo

type
  OrgXXXExporter = ref object of RootExporter
    impl: ExportDispatcher[OrgXXXExporter, XXXWriter]
    getStyle: proc(tree: SemOrg): string
    fonttbl: seq[(string, string)]

  XXXWriter = object
    writer: XmlWriter
    resDir: AbsDir



createOnExport(OrgXXXExporter, XXXWriter)

using
  exp: OrgXXXExporter
  w: var XXXWriter
  conf: RunConf
  tree: SemOrg

proc exportUsing(exp, w, tree, conf) =
  exp.exportUsing(w, exp.impl, tree, conf)

proc exportAllUsing*(exp, w, tree, conf) =
  for node in tree:
    exportUsing(exp, w, node, conf)


proc newOrgXXXExporter*(): OrgXXXExporter =
  result = OrgXXXExporter(
    name: "xxx-base",
    fileExt: "xxx",
    description: "Base xxx exporter",
  )

  result.onExport orgAllKinds:
    w.writeRaw($tree.kind)

method exportTo*(exp, tree; target: AbsFile; conf: RunConf) =
  var w = newXXXWriter(target)
  exportUsing(exp, w, tree, conf)
  w.close()
