import
  hmisc/core/all,

  ./exporter_root,
  ../defs/defs_all,
  ../external/[cmd_pygmentize, cmd_images],

  std/[macros, strformat, strutils, options, tables, sequtils, lenientops],

  hmisc/hasts/[xml_ast],
  hmisc/other/[hshell, oswrap],
  hmisc/algo/hstring_algo

type
  OrgOdtExporter = ref object of RootExporter
    impl: ExportDispatcher[OrgOdtExporter, OdtWriter]
    getStyle: proc(tree: SemOrg): string

  OdtWriter = object
    writer: XmlWriter
    resDir: AbsDir

createOnExport(OrgOdtExporter, OdtWriter)

proc newOdtWriter*(dir: AbsDir): OdtWriter =
  OdtWriter(writer: newXmlWriter(), resDir: dir)

using
  exp: OrgOdtExporter
  w: var OdtWriter
  conf: RunConf
  tree: SemOrg
  params: openarray[(string, string)]

proc exportUsing(exp, w, tree, conf) =
  exp.exportUsing(w, exp.impl, tree, conf)

proc exportAllUsing*(exp, w, tree, conf) =
  for node in tree:
    exportUsing(exp, w, node, conf)



proc writeRaw*(w; text: string) = w.writer.writeRaw(text)


proc indent*(w) = w.writer.indent()
proc dedent*(w) = w.writer.dedent()
proc line*(w) = w.writer.line()
proc enew*(w) =
  w.writer.line()
  w.writer.writeIndent()

template eindent*(w, body: untyped): untyped =
  xml_ast.eindent(w.writer, body)

proc eopen*(w; elem: string, params; indent: bool = false) =
  xml_ast.xmlStart(w.writer, elem, params, indent)

proc esingle*(w; elem: string, params; indent: bool = false) =
  xml_ast.xmlSingle(w.writer, elem, params, indent)

proc eclose*(w; elem: string; indent: bool = true) =
  xml_ast.xmlEnd(w.writer, elem, indent)

template ewrap*(w; name: string; params; body: untyped): untyped =
  xml_ast.ewrap(w.writer, name, params, body)

template ewrapl*(w: var OdtWriter, name: string; params; body: untyped): untyped =
  xml_ast.ewrapl(w.writer, name, params, body)

template ewrapl1*(w; name: string; params; body: untyped): untyped =
  xml_ast.ewrapl1(w.writer, name, params, body)

func t(str: string): string = "text:" & str

proc exportAllUsingNl*(exp, w, tree, conf) =
  for idx, node in tree:
    exportUsing(exp, w, node, conf)
    if idx < len(tree) - 1:
      w.line()

proc cpFile(w; file: AbsFile, sub: string) =
  let dir = w.resDir / RelDir(sub)
  mkDir(dir)
  cpFile(file, dir /. file.splitFile2().file)

proc writeImage(w; file: AbsFile, size: tuple[pixw, pixh: int]) =
  assertExists file
  w.ewrapl("draw:frame", {
    "text:anchor-type": "as-char",
    "svg:width": $size.pixw & "pt",
    "svg:height": $size.pixh & "pt",
    "draw:z-index": "0"
  }): w.eindent():
    w.writer.etagl("draw:image", {
      "xlink:href": "Pictures/" & file.nameExt(),
      "xlink:type": "simple",
      "xlink:show": "embed",
      "xlink:actuate": "onLoad",
      "draw:mime-type": "image/png"
    })

    w.cpFile(file, "Pictures")


proc getSizes(pic: AbsFile, tw, th: Option[OrgDimensions]): tuple[pixw, pixh: int] =
  var (pixw, pixh) = getImageSize(pic)
  if tw.canGet(w):
    result.pixw = toUnits(w, odkPx).int
    result.pixh = int(pixh * (result.pixw / pixw.float))

  elif th.canGet(h):
    result.pixh = toUnits(h, odkPx).int
    result.pixw = int(pixw * (result.pixh / pixh.float))

  elif th.canGet(h) and tw.canGet(w):
    result.pixh = toUnits(h, odkPx).int
    result.pixw = toUnits(w, odkPx).int

  else:
    raise newImplementError()


proc exportLink*(exp, w, tree, conf) =
  let l = tree.link
  case l.kind:
    of olkFile:
      let f = l.linkFile
      case f.category:
        of ofcBitmapImage:
          var ops: seq[(string, string)]
          var standalone: bool = false
          var (pixw, pixh) = getImageSize(f.getAbs())

          for p in tree.properties:
            case p.kind:
              of opkAttrImg:
                (pixw, pixh) = getSizes(
                  f.getAbs(), p.image.width, p.image.height)

              of opkColumnSpec:
                (pixw, pixh) = getSizes(
                  f.getAbs(), p.cellSpec.width, p.cellSpec.height)

              of opkToplevel:
                standalone = true

              else:
                echov p.kind

          if standalone:
            w.ewrapl(t"p", []): w.eindent():
              w.writeImage(f.getAbs(), (pixw, pixh))

          else:
            w.writeImage(f.getAbs(), (pixw, pixh))

        else:
          discard

    else:
      w.writeRaw($l.kind)

proc exportDocument*(exp, w, tree, conf) =
  let contentXml = readFile(relToSource"assets/content.xml")
  for (kind, value) in interpolatedExprs(contentXml):
    case kind:
      of iekVar, iekExpr:
        case value:
          of "document_body":
            exportAllUsing(exp, w, tree, conf)

          else:
            raise newUnexpectedKindError(value)

      else:
        w.writeRaw value

const
  defaultStyleMap* = toMapArray {
    otcBold: "org-bold",
    otcMonospaceInline: "org-monospace",
    otcPlain: "org-normal",
    otcSubtree1: "org-subtree-1",
    otcSubtree2: "org-subtree-2",
    otcSubtree3: "org-subtree-3",
    otcSubtree4: "org-subtree-4",
  }

proc newOrgOdtExporter*(
    styleMap: array[OrgTextContext, string] = defaultStyleMap
  ): OrgOdtExporter =

  block:
    result = OrgOdtExporter(
      name: "odt-base",
      fileExt: "odt",
      description: "Base odt exporter",
      getStyle: proc(org: SemOrg): string =
                  styleMap[org.getTextContext()]
    )

    result.onExport orgAllKinds:
      w.writeRaw($tree.kind)

  result.onExport {orgBold, orgMonospace, orgItalic}:
    w.ewrap(t"span", {t"style-name": exp.getStyle(tree)}):
      exportAllUsing(exp, w, tree, conf)

  result.onExport orgStmtList:
    w.indent()
    exportAllUsing(exp, w, tree, conf)
    w.dedent()

  result.onExport {orgWord, orgRawText}:
    w.writeRaw(tree.strVal())

  result.onExport orgSubtree:
    w.eindent():
      w.line()
      w.ewrapl(t"p", {t"style-name": exp.getStyle(tree)}):
        w.eindent():
          exportAllUsing(exp, w, tree[semfTitle], conf)

      w.line()

    exportAllUsing(exp, w, tree[semfBody], conf)

  result.onExport orgParagraph:
    w.ewrapl(t"p", []):
      w.eindent():
        exportAllUsing(exp, w, tree, conf)

  result.onExport orgNewline:
    w.writeRaw("\n")

  result.impl[orgLink] = exportLink

  result.onExport orgEmpty:
    discard

  result.onExport orgCommandInclude:
    let i = tree.includeSpec
    case i.kind:
      of oikExportInclude:
        if "odt" in i.backend:
          raise newImplementError()

        else:
          discard

      else:
        raise newImplementKindError(i)

  result.onExport orgTable:
    let t = tree.table


    w.ewrapl("table:table", []): w.eindent():
      w.enew()
      for col in 0 ..< t.columnCount:
        w.esingle("table:table-column", [])
        w.enew()

      for rowIdx, row in pairs(t.rows):

        w.line()
        w.ewrapl("table:table-row", []): w.eindent():
          for cellIdx, cell in pairs(row.cells):

            w.ewrapl("table:table-cell", []): w.eindent():
              exportUsing(exp, w, cell.body, conf)

  result.impl[orgDocument] = exportDocument

method exportTo*(exp, tree; target: AbsFile; conf: RunConf) =
  let dir = conf.getBackendDir(exp)
  rmDir dir
  mkDir dir
  var w = newOdtWriter(dir)
  exportUsing(exp, w, tree, conf)
  mkWithDirStructure dir:
    dir "META-INF":
      file "manifest.xml":
        readFile(relToSource"assets/manifest.xml")

    file "content.xml": readAll(w.writer)
    file "styles.xml": readFile(relToSource"assets/styles.xml")
    file "manifest.rdf": readFile(relToSource"assets/manifest.rdf")
    file "meta.xml": readFile(relToSource"assets/meta.xml")
    file "mimetype": "application/vnd.oasis.opendocument.text"
    file "settings.xml": readFile(relToSource"assets/settings.xml")

    # dir "Thumbnails":
    #   file "thumbnail.png":
    #     readFile(relToSource"thumbnail.png")

    dir "Configurations2":
      dir "accelerator"
      dir "floater"
      dir "images":
        dir "Bitmaps"

      dir "menubar"
      dir "popupmenu"
      dir "progressbar"
      dir "statusbar"
      dir "toolbar"
      dir "toolpanel"

  let res = dir /. "res.odt"

  withDir dir:
    discard runShell shellCmd(zip).withIt do:
      it - "r"
      it.arg res
      it.arg "."

  mvFile res, target
