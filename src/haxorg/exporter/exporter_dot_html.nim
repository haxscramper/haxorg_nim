import haxorg/exporter/exporter
export exporter
import hmisc/core/all
import std/[tables, sequtils, strutils]

type
  DotHtmlExporter* = ref object of Exporter
    row: int
    rowLinks*: Table[int, seq[OrgLink]] ## Mapping between rows in the text
    ## body and resulting links.
    res*: string

func props(values: openarray[(string, string)]): string =
  mapIt(values, "$#=\"$#\"" % [$it[0], $it[1]]).join(" ")

proc newDotHtmlExporter*(): DotHtmlExporter =
  result = DotHtmlExporter()
  result.addDefaultTraceHooks()
  # result.traceCalls = true

  const tableProps = props({
    "align": "left",
    "cellborder": "1",
    "cellspacing": "0",
    "cellpadding": "0"
  })

  const cellProps = props({
    "align": "left"
  })

  result.onStart():
    conv.res.add "<table $#><tr><td $#>" % [tableProps, cellProps]

  result.onFinish():
    conv.res.add "</td></tr></table>"
    # echov conv.res

  result.addImpl(orgParagraph): conv.recTrace():
    conv.subcall(node)

  result.addImpl(orgEmpty):
    conv.res.add node.strVal()

  result.addImpl(orgNewline):
    inc conv.row
    conv.print ">> add row", conv.row
    conv.res.add "</td></tr><tr><td $# $#>" % [
      cellProps,
      props({"port": "row" & $conv.row})
    ]


  result.addImpl(orgLink): conv.recTrace():
    conv.rowLinks.mgetOrDefault(conv.row).add node.link
    conv.call(node.link.description)

  result.addImpl(orgWord):
    conv.res.add node.strVal()

  result.addImpl(orgSpace):
    conv.res.add node.strVal()
