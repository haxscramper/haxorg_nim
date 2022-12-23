import haxorg/exporter/exporter
export exporter
import hmisc/core/all
import std/tables

type
  DotHtmlExporter* = ref object of Exporter
    row: int
    rowLinks*: Table[int, seq[OrgLink]] ## Mapping between rows in the text body
    ## and resulting links.
    res*: string

proc newDotHtmlExporter*(): DotHtmlExporter =
  result = DotHtmlExporter()
  result.addDefaultTraceHooks()
  result.traceCalls = true

  result.onStart():
    conv.res.add "<table><tr><td>X"

  result.onFinish():
    conv.res.add "</td></tr></table>"
    # echov conv.res

  result.addImpl(orgParagraph):
    echov "???"
    conv.subcall(node)

  result.addImpl(orgEmpty):
    conv.res.add node.strVal()

  result.addImpl(orgNewline):
    conv.res.add "</td></tr><tr><td>"
    inc conv.row

  result.addImpl(orgLink):
    conv.rowLinks.mgetOrDefault(conv.row).add node.link
    conv.call(node.link.description)

  result.addImpl(orgWord):
    conv.res.add node.strVal()

  result.addImpl(orgSpace):
    conv.res.add node.strVal()
