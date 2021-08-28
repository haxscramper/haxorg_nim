import
  ./org_types,
  ./impl_org_node

import hmisc/macros/ast_spec

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

proc len*(tree: SemOrg): int = tree.subnodes.len

func isEmptyNode*(tree: SemOrg): bool =
  tree.node.kind == orgEmptyNode

func `[]`*(tree: SemOrg, name: string): SemOrg =
  tree.subnodes[getNamedSubnode(tree.kind, name)]

func `[]`*(tree: SemOrg, idx: int): SemOrg =
  tree.subnodes[idx]

proc newSem*(node: OrgNode, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(
    kind: node.kind,
    isGenerated: false,
    node: node,
    subkind: node.subkind,
    subnodes: @subnodes)

proc newSem*(kind: OrgNodeKind, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(kind: kind, isGenerated: true, subnodes: @subnodes)
