import
  ./org_types,
  ./impl_org_node

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
