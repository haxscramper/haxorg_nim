import
  ./org_types

import
  hmisc/core/all,
  hmisc/algo/[hlex_base, hparse_base]

import std/[tables, strformat]

func strVal*(node: OrgNode): string =
  case node.kind:
    of orgTokenKinds:
      return $node.text

    of orgNowebMultilineBlock:
      raise newImplementKindError(node)

    of orgSnippetMultilineBlock:
      raise newImplementKindError(node)

    else:
      return node.str

func getPosStr*(node: OrgNode): PosStr =
  node.text

func getSubnodeName*(kind: OrgNodeKind, idx: int): string =
  template fail(): untyped = "<<fail>>"

  case kind:
    of orgSubtree:
      case idx:
        of 0: "prefix"
        of 1: "todo"
        of 2: "urgency"
        of 3: "title"
        of 4: "completion"
        of 5: "tags"
        of 6: "times"
        of 7: "drawers"
        of 8: "body"
        else: fail()

    of orgDrawer:
      case idx:
        of 0: "name"
        of 1: "body"
        else: fail()

    of orgProperty:
      case idx:
        of 0: "name"
        of 1: "subname"
        of 2: "values"
        else: fail()

    of orgMultilineCommand:
      case idx:
        of 0: "name"
        of 1: "args"
        of 2: "body"
        else: fail()

    of orgMetaTag:
      case idx:
        of 0: "name"
        of 1: "args"
        of 2: "body"
        else: fail()

    of orgTable:
      case idx:
        of 0: "args"
        of 1: "rows"
        else: fail()

    of orgTableRow:
      case idx:
        of 0: "args"
        of 1: "text"
        of 2: "body"
        else: fail()

    of orgTableCell:
      case idx:
        of 0: "args"
        of 1: "text"
        else: fail()

    of orgCommand:
      case idx:
        of 0: "name"
        of 1: "args"
        else: fail()

    of orgSrcCode:
      case idx:
        of 0: "lang"
        of 1: "header-args"
        of 2: "body"
        of 3: "result"
        else: fail()

    of orgCallCode:
      case idx:
        of 0: "name"
        of 1: "header-args"
        of 2: "args"
        of 3: "end-args"
        of 4: "result"
        else: fail()

    of orgCmdArguments:
      case idx:
        of 0: "flags"
        of 1: "args"
        else: fail()

    of orgCmdValue:
      case idx:
        of 0: "name"
        of 1: "value"
        else: fail()

    of orgAssocStmtList:
      case idx:
        of 0: "assoc"
        of 1: "main"
        else: fail()

    of orgResult:
      case idx:
        of 0: "hash"
        of 1: "body"
        else: fail()

    of orgListItem:
      case idx:
        of 0: "bullet"
        of 1: "counter"
        of 2: "checkbox"
        of 3: "tag"
        of 4: "header"
        of 5: "completion"
        of 6: "body"
        else: fail()

    of orgFootnote:
      case idx:
        of 0: "name"
        of 1: "definition"
        else: fail()

    of orgLink:
      case idx:
        of 0: "link"
        of 1: "desc"
        else: fail()

    else:
      fail()

const nodeNames =
  block:
    var res: Table[(OrgNodeKind, string), int]

    for kind in OrgNodeKind:
      for idx in 0 .. 20:
        let str = getSubnodeName(kind, idx)
        if str == "<<fail>>":
          break

        else:
          res[(kind, str)] = idx

    res


func getNamedSubnode*(kind: OrgNodeKind, name: string): int =
  if (kind, name) in nodeNames:
    return nodeNames[(kind, name)]

  else:
    raiseAssert(&"Node of kind '{kind}' does not have named subtre '{name}'")

func contains*(node: OrgNode, name: string): bool =
  (node.kind, name) in nodeNames



func add*(node: var OrgNode, other: OrgNode | seq[OrgNode]) =
  node.subnodes.add other

func len*(node: OrgNode): int =
  if node.kind in orgSubnodeKinds: node.subnodes.len else: 0

func getStr*(node: OrgNode): string =
  if node.kind in orgTokenKinds:
    assert false

  else:
    assert false

func `[]`*(node: var OrgNode, idx: BackwardsIndex): var OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, idx: BackwardsIndex): OrgNode =
  node.subnodes[idx]

func `[]=`*(node: var OrgNode, idx: BackwardsIndex, val: OrgNode) =
  node.subnodes[idx] = val

func `[]`*(node: var OrgNode, idx: int): var OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, idx: int): OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, name: string): OrgNode =
  node[getNamedSubnode(node.kind, name)]


func `[]`*(node: var OrgNode, name: string): var OrgNode =
  let idx = getNamedSubnode(node.kind, name)
  while node.len - 1 < idx:
    node.add OrgNode(kind: orgEmptyNode)

  node[getNamedSubnode(node.kind, name)]


func `[]=`*(node: var OrgNode, idx: int, val: OrgNode) =
  node.subnodes[idx] = val

func `[]=`*(node: var OrgNode, name: string, val: OrgNode) =
  let idx = getNamedSubnode(node.kind, name)
  while node.len - 1 < idx:
    node.add OrgNode(kind: orgEmptyNode)

  node[idx] = val

func `[]`*(node: var OrgNode, name: var string): var OrgNode =
  node[getNamedSubnode(node.kind, name)]



template subKindErr*(subKindVal: OrgNodeSubKind): untyped {.dirty.} =
  raise OrgSubKindError(
    subkind: subKindVal,
    msg: "Unexpected node subkind - " & $subKindVal)

iterator items*(node: OrgNode): OrgNode =
  for n in node.subnodes:
    yield n

iterator pairs*(node: OrgNode): (int, OrgNode) =
  for idx, n in node.subnodes:
    yield (idx, n)

proc `$`*(org: OrgNodeKind): string {.inline.} = toString(org)[3 ..^ 1]
proc `$`*(org: OrgNodeSubKind): string {.inline.} = toString(org)[3 ..^ 1]


proc newOrgIdent*(text: PosStr): OrgNode =
  OrgNode(kind: orgIdent, text: text)

proc newOrgIdent*(text: string): OrgNode =
  OrgNode(kind: orgIdent, text: initPosStr(text))

proc newBareIdent*(text: PosStr): OrgNode =
  OrgNode(kind: orgBareIdent, text: text)

proc newTree*(kind: OrgNodeKind, text: PosStr): OrgNode =
  assert kind in orgTokenKinds, $kind
  result = OrgNode(kind: kind)
  result.text = text


proc newTree*(
    kind: OrgNodeKind, subkind: OrgNodeSubKind, text: PosStr): OrgNode =

  result = newTree(kind, text)
  result.subKind = subKind

proc newTree*(kind: OrgNodeKind, tok: OrgStructureToken): OrgNode =
  newTree(kind, initPosStr(tok))

proc newTree*(kind: OrgNodeKind, tok: OrgCommandToken): OrgNode =
  newTree(kind, initPosStr(tok))

proc newTree*(kind: OrgNodeKind, tok: OrgTextToken): OrgNode =
  newTree(kind, initPosStr(tok))

proc newTree*(kind: OrgNodeKind, subnodes: varargs[OrgNode]): OrgNode =
  result = OrgNode(kind: kind)
  for node in subnodes:
    result.subnodes.add node

proc newTree*(
   kind: OrgNodeKind,
   subkind: OrgNodeSubKind, subnodes: varargs[OrgNode]
  ): OrgNode =

  result = newTree(kind, subnodes)
  result.subkind = subkind

proc newTree*(
    kind: OrgNodeKind, str: string, subnodes: varargs[OrgNode]
  ): OrgNode {.inline.} =

  if kind in orgTokenKinds:
    result = newTree(kind, subnodes)
    result.text = initPosStr(str)

  else:
    result = newTree(kind, subnodes)
    result.str = str


proc newTree*(
    kind: OrgNodeKind, subKind: OrgNodeSubKind,
    str: string, subnodes: varargs[OrgNode]
  ): OrgNode {.inline.} =

  result = newTree(kind, str, subnodes)
  result.subKind = subKind


proc newEmptyNode*(subkind: OrgNodeSubKind = oskNone): OrgNode =
  result = OrgNode(kind: orgEmptyNode)
  result.subKind = subKind

proc isEmptyNode*(tree: OrgNode): bool =
  tree.kind == orgEmptyNode

proc newOStmtList*(subnodes: varargs[OrgNode]): OrgNode =
  orgStmtList.newTree(subnodes)

proc newWord*(ptext: PosStr): OrgNode = orgWord.newTree(ptext)
