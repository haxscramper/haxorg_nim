import haxorg/exporter/exporter
export exporter
import hmisc/core/all

type
  UltraplainTextExporter* = ref object of Exporter
    res*: string

proc newUltraplainTextExporter*(): UltraplainTextExporter =
  result = UltraplainTextExporter()
  result.addImpl(not orgTokenKinds): conv.subcall(node)
  result.addImpl(orgTokenKinds): conv.res.add node.strVal()
  result.addImpl(orgLink):
    assertRef(node)
    assertRef(node.link.description)
    conv.call(node.link.description)
