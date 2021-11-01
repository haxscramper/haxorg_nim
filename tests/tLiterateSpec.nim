import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base, clformat],
  hmisc/types/colorstring

import
  haxorg,
  haxorg/parse/[parse_org_structure, convert_semorg],
  haxorg/exporter/[
    exporter_html,
    exporter_tex,
    exporter_rtf,
    exporter_odt,
  ],
  haxorg/defs/impl_org_node

suite "Execute literate spec for nim generics":
  test "text":
    var conf = haxrunConf()
    conf.templateDir = AbsDir(relToSource"assets")

    let (parse, sem, ctx) = orgParseSemExec(
      AbsFile(relToSource"assets/literate-spec.org"), conf)

    getTestTempFile("parsecolored", "txt").writeFile(
      parse.
      treeRepr(hdisplay(flags += dfWithRanges)).
      toString(color = false))

    getTestTempFile("semcolored", "txt").writeFile(
      sem.treeRepr().toString(color = false))

    conf.exportWith(
      newOrgHtmlExporter(),
      sem,
      getTestTempFile("page", "html"))

    conf.exportWith(
      newOrgRtfExporter(),
      sem,
      getTestTempFile("page", "rtf"))

    conf.exportWith(
      newOrgOdtExporter(),
      sem,
      getTestTempFile("page", "odt"))

    conf.exportWith(
      newOrgTexPdfExporter(),
      sem,
      getTestTempFile("page", "pdf"))
