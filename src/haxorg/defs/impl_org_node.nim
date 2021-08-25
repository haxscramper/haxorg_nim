import
  ./org_types

import
  hmisc/core/all,
  hmisc/algo/[hlex_base, hparse_base, clformat],
  hmisc/macros/ast_spec

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
  assertKind(kind, orgTokenKinds)
  result = OrgNode(kind: kind)
  result.text = text


proc newTree*(
    kind: OrgNodeKind, subkind: OrgNodeSubKind, text: PosStr): OrgNode =

  result = newTree(kind, text)
  result.subKind = subKind


proc newTree*(kind: OrgNodeKind, tok: OrgCommandToken): OrgNode =
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

proc newOrgEmptyNode*(): OrgNode = OrgNode(kind: orgEmptyNode)
proc newOrgEmpty*(): OrgNode = OrgNode(kind: orgEmptyNode)

proc isEmptyNode*(tree: OrgNode): bool =
  tree.kind == orgEmptyNode

proc newOStmtList*(subnodes: varargs[OrgNode]): OrgNode =
  orgStmtList.newTree(subnodes)

proc newWord*(ptext: PosStr): OrgNode = orgWord.newTree(ptext)


const
  orgNodeSpec* = astSpec(OrgNode, OrgNodeKind):
    orgList:
      0 .. ^1 as "items":
        orgListItem

    orgSubtree:
      0 as "prefix"
      1 as "todo": orgBigIdent or orgEmpty
      2 as "urgency": orgUrgencyStatus or orgEmpty
      3 as "title": orgParagraph
      4 as "completion": orgCompletion or orgEmpty
      5 as "tags": orgOrgTag or orgEmpty
      6 as "times"
      7 as "drawers": orgDrawer or orgEmpty
      8 as "body": orgStmtList or orgEmpty

    orgDrawer:
      0 as "name"
      1 as "body"

    orgProperty:
      0 as "name": orgIdent
      1 as "subname"
      2 as "values"

    orgMultilineCommand:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty
      2 as "body"

    orgMetaTag:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty
      2 as "body": orgRawText

    orgTable:
      0 as "args": orgCmdArguments or orgEmpty
      1 as "rows":
        0 .. ^1:
          orgTableRow

    orgTableRow:
      0 as "args": orgCmdArguments or orgEmpty
      1 as "text": orgParagraph
      2 as "body":
        0 .. ^1:
          orgTableCell

    orgTableCell:
      0 as "args": orgCmdArguments or orgEmpty
      1 as "text": orgParagraph

    orgCommand:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty

    orgCodeLine:
      0 .. ^1:
        orgCodeText or orgCodeTangle or orgCodeCallout

    orgSrcCode:
      0 as "lang": orgIdent
      1 as "header-args":
        orgCmdArguments or orgEmpty

      2 as "body":
        orgStmtList:
          0 .. ^1:
            orgCodeLine

      3 as "result": orgRawText or orgEmpty

    orgCallCode:
      0 as "name": orgIdent
      1 as "header-args": orgCmdArguments or orgEmpty
      2 as "args"
      3 as "end-args"
      4 as "result": orgRawText or orgEmpty

    orgCmdArguments:
      0 as "flags":
        orgInlineStmtList:
          0 .. ^1:
            orgCmdFlag

      1 as "args":
        orgInlineStmtList:
          0 ..^ 1:
            orgCmdValue

    orgCmdValue:
      0 as "name"
      1 as "value"

    orgAssocStmtList:
      0 as "assoc"
      1 as "main"

    orgResult:
      0 as "hash"
      1 as "body"

    orgListItem:
      0 as "bullet":
        ## List prefix - either dash/plus/star (for unordered lists), or
        ## `<idx>.`/`<name>.`

      1 as "counter"

      2 as "checkbox":
        ## Optional checkbox
        orgCheckbox or orgEmptyNode

      3 as "tag":
        ## Described text (for description list)
        orgParagraph or orgEmptyNode

      4 as "header":
        ## Main part of the list
        orgParagraph

      5 as "completion":
        ## Cumulative completion progress for all subnodes
        orgCompletion or orgEmptyNode

      6 as "body":
        ## Additional list items - more sublists, or extended body (with
        ## code blocks, extra parargaphs etc.)
        orgStmtList or orgEmptyNode


    orgFootnote:
      0 as "name"
      1 as "definition"

    orgLink:
      0 as "link"
      1 as "desc"

# echo orgNodeSpec.treeRepr()

proc treeRepr*(
    org: OrgNode,
    opts: HDisplayOpts = defaultHDisplay): ColoredText =

  coloredResult()

  proc aux(
      n: OrgNode, level: int,
      name: Option[string],
      err: Option[ColoredText]
    ) =

    addIndent(level)
    add hshow(n.kind)

    if err.isSome():
      add " \n"
      add err.get().indent(level * 2 + 2)

    elif name.isSome():
      add " "
      add toCyan(name.get())

    case n.kind:
      of orgTokenKinds - orgEmpty:
        add " "
        add hshow(
          n.text.strVal(),
          hdisplay(flags -= dfSpellEmptyStrings))

      of orgEmpty:
        discard

      of orgNowebMultilineBlock:
        raise newImplementKindError(n)

      of orgSnippetMultilineBlock:
        raise newImplementKindError(n)

      else:
        for idx, sub in n:
          add "\n"
          aux(
            sub,
            level + 1,
            orgNodeSpec.fieldName(n, idx),
            validateSub(orgNodeSpec, n, idx)
          )

        let err = validateSelf(orgNodeSpec, n)
        if err.isSome():
          add "\n"
          add err.get().indent(level * 2)


  aux(org, 0, none(string), none(ColoredText))
  endResult()
