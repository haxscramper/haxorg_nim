import
  ./org_types,
  ./impl_org_node

import
  hmisc/macros/ast_spec,
  hmisc/other/hpprint,
  hmisc/types/colorstring,
  hmisc/algo/clformat,
  hmisc/core/all

const
  semOrgSpec* = astSpec(SemOrg, OrgNodeKind):
    orgSubtree:
      0 as "title": orgParagraph
      1 as "body": orgStmtList or orgEmpty


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
  tree.node.kind == orgEmptyNode

func `[]`*(tree: SemOrg, idx: int): SemOrg =
  tree.subnodes[idx]

func `[]`*(tree: SemOrg, name: string): SemOrg =
  getSingleSubnode(semOrgSpec, tree, name)


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
    add hshow(n.kind)

    case n.kind:
      of orgTokenKinds:
        add " "
        add hshow(n.strVal())

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
