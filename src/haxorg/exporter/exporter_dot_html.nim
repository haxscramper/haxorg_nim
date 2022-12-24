import haxorg/exporter/exporter
export exporter
import hmisc/core/all
import std/[tables, sequtils, strutils]

type
  DotHtmlExporter* = ref object of Exporter
    row: int
    rowLinks*: Table[int, seq[OrgLink]] ## Mapping between rows in the text
    ## body and resulting links.
    rowFootnotes*: Table[int, seq[OrgFootnote]]
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

  result.addImpl(orgTokenKinds):
    conv.res.add node.strVal()

  result.addImpl(orgQuote):
    conv.res.add "\""
    conv.subcall(node)
    conv.res.add "\""

  result.onStart():
    conv.res.add "<table $#><tr><td $#>" % [tableProps, cellProps]

  result.onFinish():
    conv.res.add "</td></tr></table>"
    # echov conv.res

  result.addImpl(orgParagraph): conv.recTrace():
    conv.subcall(node)

  result.addImpl(orgStmtList): conv.recTrace():
    conv.subcall(node)

  result.addImpl(orgAnnotatedParagraph): conv.recTrace():
    let p = node.paragraph
    case p.kind:
      of aopFootnote:
        if p.footnote.inline:
          conv.call(p.footnote.text)

        else:
          conv.res.add "[fn:$#]" % p.footnote.ident

      else: assert false, $node.treeRepr(treePara)

    conv.call(p.body)

  result.addImpl(orgNewline):
    inc conv.row
    conv.res.add "</td></tr><tr><td $# $#>" % [
      cellProps,
      props({"port": "row" & $conv.row})
    ]

  result.addImpl(orgFootnote): conv.recTrace():
    let note = node.footnoteTarget
    if note.inline:
      conv.res.add "[$#_$#]" % [
        $conv.row,
        $conv.rowFootnotes.mgetOrDefault(conv.row).len()
      ]

    else:
      conv.res.add "[$#]" % note.ident

    conv.rowFootnotes.mgetOrDefault(conv.row).add note

  result.addImpl(orgLink): conv.recTrace():
    conv.rowLinks.mgetOrDefault(conv.row).add node.link
    conv.call(node.link.description)

  result.addImpl(orgWord):
    conv.res.add node.strVal()

  result.addImpl(orgSpace):
    conv.res.add node.strVal()
