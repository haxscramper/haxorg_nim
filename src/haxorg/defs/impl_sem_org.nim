
import hmisc/core/all

importx:
  ./org_types
  ./impl_org_node

  std/sequtils

  hmisc/[
    macros/ast_spec,
    other/hpprint,
    types/colorstring,
    algo/[clformat, hstring_algo]
  ]

export clformat

const
  semOrgSpec* = astSpec(SemOrg, OrgNodeKind):
    orgSubtree:
      0 as "title": orgParagraph
      1 as "body": orgStmtList or orgEmpty

    orgSrcCode:
      0 as "compile-result": orgEmpty
      1 as "eval-result": orgEmpty



proc add*(tree: var SemOrg, subtree: SemOrg) =
  assert tree.kind in orgSubnodeKinds
  tree.subnodes.add subtree

iterator items*(tree: SemOrg): SemOrg =
  for subnode in tree.subnodes:
    yield subnode


iterator mitems*(tree: var SemOrg): var SemOrg =
  for subnode in mitems(tree.subnodes):
    yield subnode

proc len*(tree: SemOrg): int = tree.subnodes.len

func isEmptyNode*(tree: SemOrg): bool =
  isNil(tree) or tree.kind == orgEmptyNode

func `?`*(tree: SemOrg): bool = not isEmptyNode(tree)

func hasAssoc*(tree: SemOrg, kinds: set[OrgNodeKind]): bool =
  tree.assocList.isSome() and anyIt(tree.assocList.get(), it of kinds)

func getAssoc*(tree: SemOrg, kinds: set[OrgNodeKind]): seq[SemOrg] =
  if tree.assocList.isSome():
    for item in tree.assocList.get():
      if item of kinds:
        result.add item

func `[]`*(tree: SemOrg, idx: int): SemOrg =
  tree.subnodes[idx]

func `[]`*(tree: SemOrg, name: string): SemOrg =
  getSingleSubnode(semOrgSpec, tree, name)


func `[]=`*(tree: SemOrg, name: string, other: SemOrg) =
  tree.subnodes[getSingleSubnodeIdx(semOrgSpec, tree, name)] = other


proc newSem*(kind: OrgNodeKind, str: string): SemOrg =
  SemOrg(kind: kind, isGenerated: true, str: str)

proc newSem*(node: OrgNode, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(
    kind: node.kind,
    isGenerated: false,
    node: node,
    subkind: node.subkind,
    subnodes: @subnodes)

proc newSem*(kind: OrgNodeKind, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(kind: kind, isGenerated: true, subnodes: @subnodes)

func strVal*(node: SemOrg): string =
  assertKind node, orgTokenKinds
  if node.isGenerated:
    result = node.str

  else:
    result = node.node.strVal()

proc treeRepr*(
    node: SemOrg,
    conf: HDisplayOpts = defaultHDisplay,
    rawBlockParams: bool = false
  ): ColoredText =

  coloredResult()

  var pprintConf = pconf(
    ignorePaths = matchField("blockArgs") & matchType("CliDesc"),
    confId = 228
  )

  proc aux(n: SemOrg, level: int) =
    addIndent level
    let kind = hshow(n.kind)
    add kind

    case n.kind:
      of orgTokenKinds:
        add " "
        add hshow(n.strVal().indentBody(kind.len + level * 2))

      of orgSrcCode:
        add "\n"
        // "Source code block tree repr"
        if isNil(n.codeBlock):
          addIndent level + 1
          add "empty code block" + fgRed

        else:
          add blockPPtree(n.codeBlock, pprintConf).objectTreeRepr(
            pprintConf, level * 2 + 2)

          if rawBlockParams:
            add "\n"
            add pptree(n.codeBlock.blockArgs.value, pprintConf).objectTreeRepr(
              pprintConf, level * 2 + 2)


        for sub in n:
          add "\n"
          aux(sub, level + 1)

      of orgSubtree:
        add "\n"
        add pptree(n.subtree, pprintConf).objectTreeRepr(
          pprintConf, level * 2 + 2)

        for sub in n:
          add "\n"
          aux(sub, level + 1)

      else:
        for sub in n:
          add "\n"
          aux(sub, level + 1)



  aux(node, 0)
  endResult()
