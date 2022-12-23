import haxorg/exporter/exporter
export exporter
import hmisc/core/all
import std/tables

type
  DotHtmlExporter* = ref object of Exporter
    row: int
    rowLinks*: Table[int, OrgLink] ## Mapping between rows in the text body
    ## and resulting links.
    res*: string

proc newDotHtmlExporter*(): DotHtmlExporter =
  result = DotHtmlExporter()
