import
  ./haxorg/[
    defs/org_types,
    defs/impl_org_node,
    defs/impl_sem_org,
    runcode/runcode_root,
    parse/parse_org_structure,
    parse/parse_org_text,
    parse/convert_semorg
  ],

  ./haxorg/runcode/[
    runcode_nim
  ],

  hmisc/algo/hlex_base,
  hmisc/other/oswrap

export impl_sem_org
export OrgConf, SemOrg

proc haxrunConf*(): OrgConf =
  var conf = initOrgConf()
  conf["nim"] = CodeBuilder(proc(): CodeBlock {.closure.} = newNimCodeBlock())
  return conf

proc exportWith*(
     conf: OrgConf, exp: Exporter, sem: SemOrg, file: AbsFile) =
  exportTo(exp, sem, file, conf)

proc orgParse*(text: string, conf: OrgConf = haxrunConf()): OrgNode =
  result = parseOrg(varPosStr(text))

proc orgSem*(tree: OrgNode, file: AbsFile, conf: OrgConf = haxrunConf()):
  tuple[sem: SemOrg, context: SemOrgCtx] =
  result = toSemDocument(tree, file, conf)


proc orgSem*(text: string, file: AbsFile, conf: OrgConf = haxrunConf()):
  tuple[sem: SemOrg, context: SemOrgCtx] =
  let tree = orgParse(text)
  result = toSemDocument(tree, file, conf)


proc orgSemExec*(text: string, file: AbsFile, conf: OrgConf = haxrunConf()):
  tuple[sem: SemOrg, context: SemOrgCtx] =
  result = orgSem(text, file, conf)
  evalCode(result.sem, conf)

proc orgParseSemExec*(text: string, file: AbsFile, conf: OrgConf = haxrunConf()):
  tuple[parse: OrgNode, sem: SemOrg, context: SemOrgCtx] =
  result.parse = orgParse(text)
  var (sem, context) = toSemDocument(result.parse, file, conf)
  evalCode(sem, conf)
  result.sem = sem
  result.context = context

proc orgParseSemExec*(file: AbsFile, conf: OrgConf = haxrunConf()):
  tuple[parse: OrgNode, sem: SemOrg, context: SemOrgCtx] =

  orgParseSemExec(file.readFile(), file, conf)
