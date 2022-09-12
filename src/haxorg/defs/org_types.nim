import std/[options, tables, hashes, uri, rationals]

export rationals

import
  hmisc/algo/hlex_base,
  hmisc/other/[hshell, oswrap, hpprint, hargparse]

import hmisc/core/all except Attr

import
  ./org_types_enums,
  ./org_types_misc,
  ./org_types_sem

import
  ../parse/lex_all

export
  org_types_enums,
  org_types_sem,
  org_types_misc


type
  CodeBuilder* = proc(): CodeBlock
  OrgConf* = object
    tempDir*: AbsDir
    codeCreateCallbacks*: Table[string, CodeBuilder]
    ctx*: SemOrgCtx

    templateDir*: AbsDir

  Exporter* = ref object of RootObj
    name*: string


method exportTo*(
    exporter: Exporter,
    tree: SemOrg,
    file: AbsFile,
    conf: OrgConf
  ) {.base.} =

  raise newImplementBaseError(Exporter(), "exportTo")

method runCode*(
    codeBlock: CodeBlock,
    context: var CodeRunContext,
    conf: OrgConf
  ) {.base.} =

  raise newImplementBaseError(CodeBlock(), "runCode")

method parseFrom*(
    codeBlock: CodeBlock, semorg: OrgNode,
    scope: var SemOrgCtx) {.base.} =
  ## Parse code block body from semorg node. This method is called from
  ## top-level convert dispatcher loop using
  ## `parseFrom(semorg.codeBloc,semorg)` to trigger runtime dispatch.
  ## Overrides for this method can set only `codeBlock` argument, or modify
  ## `semorg` too, it doesn't really matter.
  raise newImplementBaseError(CodeBlock(), "parseFrom")

method blockPPtree*(
    codeBlock: CodeBlock, conf: var PPrintConf): PPrintTree {.base.} =
  pptree(codeBlock, conf)


proc `[]=`*(conf: var OrgConf, lang: string, codeBuilder: CodeBuilder) =
  conf.codeCreateCallbacks[lang] = codeBuilder

proc initOrgConf*(): OrgConf =
  OrgConf(tempDir: getAppTempDir())

proc initSemOrgCtx*(file: AbsFile): SemOrgCtx =
  SemOrgCtx(fileStack: @[file])

proc initAnchor*(sem: SemOrg): OrgAnchor =
  OrgAnchor(kind: oakSemOrg, targetNode: sem)
