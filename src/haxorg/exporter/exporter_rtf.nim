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
  OrgRtfExporter = ref object of RootExporter
    impl: ExportDispatcher[OrgRtfExporter, RtfWriter]
    getStyle: proc(tree: SemOrg): string
    fonttbl: seq[(string, string)]

  RtfWriter = object
    base: BaseWriter



genBaseWriterProcs(RtfWriter)
createOnExport(OrgRtfExporter, RtfWriter)

using
  exp: OrgRtfExporter
  w: var RtfWriter
  conf: RunConf
  tree: SemOrg

proc exportUsing(exp, w, tree, conf) =
  exp.exportUsing(w, exp.impl, tree, conf)

proc exportAllUsing*(exp, w, tree, conf) =
  for node in tree:
    exportUsing(exp, w, node, conf)

template pcmd(w; cmd: string, body: untyped): untyped =
  w.writeRaw("{", cmd, " ")
  body
  w.writeRaw("}")
  w.line()

proc cmdl(w; cmds: varargs[string]) =
  for cmd in cmds:
    w.writeRaw("\\", cmd)

  w.line()

template icmd(w; cmd: string, body: untyped): untyped =
  w.writeRaw("{", cmd, " ")
  body
  w.writeRaw("}")

proc subsRtfBase(
    text: string,
    map: openarray[(string, string)],
    start: string = "template_"
  ): string =
  let map = toTable(map)
  var prev = 0
  var found = 0
  while found != -1:
    result.add text[prev ..< found]

    var pos = found
    while text[pos] in {'a' .. 'z', '_'}:
      inc pos

    var name = text[found ..< pos]
    # echov name
    if name.len > 0:
      name = name[start.len .. ^1]

    if name in map:
      result.add map[name]
      prev = found + name.len + start.len

    else:
      result.add name
      prev = found + name.len

    found = text.find(start, prev)

  result.add text[prev .. text.high]
  # echov text[0..200]
  # echov result[0..200]

func startsWith*(str: string, offset: int, pat: string): bool =
  for idx in 0 .. pat.high:
    let pos = offset + idx
    if str.high < pos or str[pos] != pat[idx]:
      return false

  return true



proc readRtfBase(file: AbsFile): string =
  let text = readFile(file)
  var
    pos = 0
    lastEnd = 0
    isPre: seq[bool]

  while pos < text.len:
    case text[pos]:
      of '{':
        inc pos, 2
        let isp = (
          text.startsWith(pos, "fonttbl") or
          text.startsWith(pos, "colortbl") or
          text.startsWith(pos, "stylesheet")
        )

        isPre.add isp

      of '}':
        inc pos
        if isPre.len == 0:
          return text[lastEnd .. text.high]

        elif isPre.pop():
          lastEnd = pos

      else:
        inc pos

  echov text[lastEnd .. text.high]

proc exportInclude*(exp, w, tree, conf) =
  let incs = tree.includeSpec

  case incs.kind:
    of oikExportInclude:
      var map: seq[(string, string)]
      for name, value in incs.vars:
        map.add(name, $value)

      w.writeRaw(incs.file.getAbs().readRtfBase().subsRtfBase(map))
      if incs.newpage:
        w.line()
        w.writeRaw(r"\page")
        w.line()

    else:
      raise newUnexpectedKindError()


proc exportDocument*(exp, w, tree, conf) =
  w.pcmd r"\rtf1\ansi\deff0":
    let d = tree.document

    w.line()
    w.pcmd r"\fonttbl":
      w.line()
      for idx, font in pairs(exp.fonttbl):
        w.writeRaw "{", font[0], font[1], "}"
        if idx < exp.fonttbl.high:
          w.line()

    if d.pagenum.canGet(pagenum):
      w.pcmd r"\footer\qc":
        w.icmd r"\field":
          if pagenum == 1:
            w.writeRaw r"\*\fldinst {PAGE}"

          else:
            raise newImplementError("Page numbering staring from different location")

        w.writeRaw r"\par"


    exportAllUsing(exp, w, tree, conf)



proc exportSubtree*(exp, w, tree, conf) =
  w.pcmd r"\pard" & exp.getStyle(tree):
    exportAllUsing(exp, w, tree[semfTitle], conf)
    w.writeRaw(r"\line\par")


  exportAllUsing(exp, w, tree[semfBody], conf)

const defaultRtfFontTable*: seq[tuple[font, definition: string]] = @{
  r"\f0": r"\froman Times New Roman;",
  r"\f1": r"Consolas;"
}

const defaultRtfStylesheet*: array[OrgTextContext, string] = toMapArray {
  otcSubtree1: r"\f0\fs48",
  otcSubtree2: r"\f0\fs40",
  otcSubtree3: r"\f0\fs36",
  otcSubtree4: r"\f0\fs32",
}

proc defaultRtfStyle*(tree: SemOrg): string =
  defaultRtfStyleSheet[tree.getTextContext()]



proc hexenc(s: string, width: int): string =
  var w = 0
  for c in s:
    inc w, 2
    if w > width:
      result.add "\n"
      w = 0

    result.add $toHex(uint8(c)).toLowerAscii()

proc imageSpec(size: tuple[pixw, pixh: int], tw, th: Option[OrgDimensions]): string =
  if tw.canGet(w):
    let x = int(w.toUnits(odkPx) / size.pixw.float * 100)
    result.add fmt"\picw{size.pixw}\pich{size.pixh}"
    result.add fmt"\picwgoal{size.pixw.float.pxToTwip.int}\picscalex{x}"
    result.add fmt"\pichgoal{size.pixh.float.pxToTwip.int}\picscaley{x}"

  elif th.canGet(h):
    let y = int(h.toUnits(odkPx) / size.pixh.float * 100)
    result.add fmt"\picw{size.pixw}\pich{size.pixh}"
    result.add fmt"\picwgoal{size.pixw.float.pxToTwip.int}\picscalex{y}"
    result.add fmt"\pichgoal{size.pixh.float.pxToTwip.int}\picscaley{y}"

  elif th.canGet(h) and tw.canGet(w):
    let x = int(w.toUnits(odkPx) / size.pixw.float * 100)
    let y = int(h.toUnits(odkPx) / size.pixh.float * 100)
    result.add fmt"\picw{size.pixw}\pich{size.pixh}"
    result.add fmt"\picwgoal{size.pixw.float.pxToTwip.int}\picscalex{x}"
    result.add fmt"\pichgoal{size.pixh.float.pxToTwip.int}\picscaley{y}"

  else:
    raise newImplementError()

proc newOrgRtfExporter*(
    style: proc(tree: SemOrg): string = defaultRtfStyle,
    fonttbl: openarray[tuple[font, definition: string]] = defaultRtfFontTable,
  ): OrgRtfExporter =

  result = OrgRtfExporter(
    name: "rtf-base",
    fileExt: "rtf",
    description: "Base rtf exporter",
    getStyle: style,
    fonttbl: @fonttbl
  )

  result.onExport orgAllKinds:
    w.writeRaw($tree.kind)

  result.onExport orgSrcCode:
    w.pcmd r"\pard\ql\f1":
      for line in tree.node[orgfBody]:
        for part in line:
          case part.kind:
            of orgCodeText: w.writeRaw(part.strVal)
            of orgEmpty, orgCodeCallout: discard
            else: raise newUnexpectedKindError(part)

        w.writeRaw(r"\line")
        w.line()

      w.writeRaw(r"\par")


  result.onExport {orgWord, orgRawText}:
    w.writeRaw(tree.node.text.strVal())

  result.onExport orgNewline:
    w.writeRaw(" ")

  result.onExport orgLink:
    let l = tree.link
    case l.kind:
      of olkFile:
        let f = l.linkFile
        case f.category:
          of ofcBitmapImage:
            var spec = r"\pict\pngblip"
            for p in tree.properties:
              case p.kind:
                of opkAttrImg:
                  spec.add imageSpec(
                    getImageSize(f.getAbs()),
                    p.image.width, p.image.height)

                of opkColumnSpec:
                  spec.add imageSpec(
                    getImageSize(f.getAbs()),
                    p.cellSpec.width, p.cellSpec.height)

                  # if .canGet(w) and .canGet(h):
                  #   let pxH = h.toUnits(odkPx).int
                  #   let pxW = w.toUnits(odkPx).int
                  #   spec.add(
                  #     fmt"\picw{pxW}\pich{pxH}" &
                  #       fmt"\picwgoal1860\picscalex75" &
                  #       fmt"\pichgoal1440\picscaley75")

                of opkToplevel: discard


                else:
                  raise newImplementKindError(p)

            w.icmd r"\*\shppict":
              w.icmd spec:
                w.line()
                w.writeRaw f.getAbs().readFile().hexenc(64)

            if anyIt(tree.properties, it of opkToplevel):
              w.line()
              w.writeRaw(r"\line")
              w.line()

          else:
            raise newImplementKindError(f.category)

      else:
        w.writeRaw($l.kind)

  result.onExport orgParagraph:
    w.icmd r"\pard\qj":
      exportAllUsing(exp, w, tree, conf)
      w.writeRaw(r"\par")

  result.onExport orgBold:
    w.icmd r"\b":
      exportAllUsing(exp, w, tree, conf)

  result.onExport orgMonospace:
    w.icmd r"\f1":
      exportAllUsing(exp, w, tree, conf)

  result.onExport orgTable:
    let t = tree.table
    w.line()
    for row in t.rows:
      var edge = 1200
      w.cmdl("trowd")
      w.cmdl(fmt"trleft{edge}") # IMPLEMENT format table position depending on
                            # the target paper size and target table size
                            # (compute based on the column widths, border
                            # widths, number of columns and so on).

      for colIdx, col in row.cells:
        let b = col.borders
        var res: string
        for idx, border in [b.top, b.right, b.bottom, b.left]:
          let dir =
            case idx:
              of 0: "t"
              of 1: "r"
              of 2: "b"
              of 3: "l"

          case border:
            of ocsNone: discard
            of ocsSingleLine: res.add fmt"\clbrdr{dir}\brdrs\brdrw10"
            of ocsDoubleLine: res.add fmt"\clbrdr{dir}\brdrsb\brdrw10"

        if res.len > 0:
          w.writeRaw(res)
          w.line()

        edge += 2000 # IMPLEMENT Get target cell sizes based on the target
                     # formatting
        w.cmdl(&"cellx{edge}")

      for col in row.cells:
        exportAllUsing(exp, w, col.body, conf)
        w.cmdl("intbl", "cell")

      w.cmdl("row")

  result.impl[orgCommandInclude] = exportInclude
  result.impl[orgDocument] = exportDocument
  result.impl[orgStmtList] = exportAllUsing
  result.impl[orgSubtree] = exportSubtree
  result.onExport orgEmpty:
    discard

method exportTo*(exp, tree; target: AbsFile; conf: RunConf) =
  var w = newRtfWriter(target)
  exportUsing(exp, w, tree, conf)
  w.close()
