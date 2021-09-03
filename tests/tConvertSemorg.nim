import hmisc/preludes/unittest

importx:
  hmisc/[
    algo/[hparse_base, hlex_base, clformat],
    types/colorstring,
    other/oswrap
  ]

  haxorg/[
    parse/[
      parse_org_structure,
      convert_semorg],

    defs/[
      impl_org_node,
      impl_sem_org,
      org_types],

    runcode/[
      runcode_nim,
      runcode_root],

    exporter/[
      exporter_tex]
  ]

suite "Convert to semorg":
  test "Full document":
    let tree = parseOrg(varPosStr asConst slurp"assets/input-1.txt")
    # echov tree.treeRepr(hdisplay(flags += dfWithRanges))
    var conf = initRunConf()
    conf["nim"] = newNimCodeBlock

    var (sem, ctx) = tree.toSemDocument(conf)
    conf.ctx = ctx

    sem.evalCode(conf)

    echo sem.treeRepr()
    pprint(ctx.symTable, ignore = matchType("SemOrg"))
    newOrgTexPdfExporter().exportTo(sem, AbsFile"/tmp/target_pdf.pdf", conf)
