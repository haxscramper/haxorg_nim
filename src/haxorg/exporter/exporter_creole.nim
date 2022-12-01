import haxorg/exporter/exporter

type
  CreoleExporter* = ref object of Exporter
    res*: string

proc newCreoleExporter*(): CreoleExporter =
  result = CreoleExporter()
  result.addImpl(orgParagraph):
    for sub in node: conv.call(sub)

  result.addImpl({orgLink, orgRawLink}):
    conv.res.add "LINK:TODO"

  result.addImpl({orgSpace, orgWord}): conv.subcall(node)

  result.addImpl(orgQuote):
    conv.res.add '"'
    conv.subcall(node)
    conv.res.add '"'

  result.addImpl(orgQuote):
    conv.res.add '='
    conv.subcall(node)
    conv.res.add '='

  result.addImpl(orgBold):
    conv.res.add "*"
    conv.subcall(node)
    conv.res.add "*"
