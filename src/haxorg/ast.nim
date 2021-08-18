import std/[lexbase, tables, strformat, sugar, sequtils]
import ./common, ./buf
import hpprint/hpprint_repr

import hmisc/core/all
import hmisc/types/colorstring


template subKindErr*(subKindVal: OrgNodeSubKind): untyped {.dirty.} =
  raise OrgSubKindError(
    subkind: subKindVal,
    msg: "Unexpected node subkind - " & $subKindVal
  )

func strVal*(node: OrgNode): string =
  case node.kind:
    of orgTokenKinds:
      return $node.text

    of onkNowebMultilineBlock:
      raiseAssert("#[ IMPLEMENT ]#")

    of onkSnippetMultilineBlock:
      raiseAssert("#[ IMPLEMENT ]#")

    else:
      return node.str

func getSubnodeName*(kind: OrgNodeKind, idx: int): string =
  template fail(): untyped = "<<fail>>"

  case kind:
    of onkSubtree:
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

    of onkDrawer:
      case idx:
        of 0: "name"
        of 1: "body"
        else: fail()

    of onkProperty:
      case idx:
        of 0: "name"
        of 1: "subname"
        of 2: "values"
        else: fail()

    of onkMultilineCommand:
      case idx:
        of 0: "name"
        of 1: "args"
        of 2: "body"
        else: fail()

    of onkMetaTag:
      case idx:
        of 0: "name"
        of 1: "args"
        of 2: "body"
        else: fail()

    of onkTable:
      case idx:
        of 0: "args"
        of 1: "rows"
        else: fail()

    of onkTableRow:
      case idx:
        of 0: "args"
        of 1: "text"
        of 2: "body"
        else: fail()

    of onkTableCell:
      case idx:
        of 0: "args"
        of 1: "text"
        else: fail()

    of onkCommand:
      case idx:
        of 0: "name"
        of 1: "args"
        else: fail()

    of onkSrcCode:
      case idx:
        of 0: "lang"
        of 1: "header-args"
        of 2: "body"
        of 3: "result"
        else: fail()

    of onkCallCode:
      case idx:
        of 0: "name"
        of 1: "header-args"
        of 2: "args"
        of 3: "end-args"
        of 4: "result"
        else: fail()

    of onkCmdArguments:
      case idx:
        of 0: "flags"
        of 1: "args"
        else: fail()

    of onkCmdValue:
      case idx:
        of 0: "name"
        of 1: "value"
        else: fail()

    of onkAssocStmtList:
      case idx:
        of 0: "assoc"
        of 1: "main"
        else: fail()

    of onkResult:
      case idx:
        of 0: "hash"
        of 1: "body"
        else: fail()

    of onkListItem:
      case idx:
        of 0: "bullet"
        of 1: "counter"
        of 2: "checkbox"
        of 3: "tag"
        of 4: "header"
        of 5: "completion"
        of 6: "body"
        else: fail()

    of onkFootnote:
      case idx:
        of 0: "name"
        of 1: "definition"
        else: fail()

    of onkLink:
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


{.push inline.}

func add*(node: var OrgNode, other: OrgNode | seq[OrgNode]) =
  node.subnodes.add other

func len*(node: OrgNode): int =
  if node.kind in orgSubnodeKinds: node.subnodes.len else: 0

func getStr*(node: OrgNode): string =
  if node.kind in orgTokenKinds:
    $EndOfFile
  else:
    node.str

func charLen*(node: OrgNode): int = node.text.len

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
    node.add OrgNode(kind: onkEmptyNode)

  node[getNamedSubnode(node.kind, name)]


func `[]=`*(node: var OrgNode, idx: int, val: OrgNode) =
  node.subnodes[idx] = val

func `[]=`*(node: var OrgNode, name: string, val: OrgNode) =
  let idx = getNamedSubnode(node.kind, name)
  while node.len - 1 < idx:
    node.add OrgNode(kind: onkEmptyNode)

  node[idx] = val

func `[]`*(node: var OrgNode, name: var string): var OrgNode =
  node[getNamedSubnode(node.kind, name)]

{.pop.}

iterator items*(node: OrgNode): OrgNode =
  for n in node.subnodes:
    yield n

iterator pairs*(node: OrgNode): (int, OrgNode) =
  for idx, n in node.subnodes:
    yield (idx, n)

proc `$`*(onk: OrgNodeKind): string {.inline.} = toString(onk)[3 ..^ 1]
proc `$`*(onk: OrgNodeSubKind): string {.inline.} = toString(onk)[3 ..^ 1]



func objTreeRepr*(node: OrgNode, name: string = "<<fail>>"): ObjTree =
  var name = tern(name != "<<fail>>", &"({toGreen(name)}) ", "")
  if node.isNil:
    return pptConst($(name & toBlue("<nil>")))


  if node.subKind != oskNone:
    name &= &"[{toMagenta($node.subKind)}] "

  case node.kind:
    of onkIdent:
      return pptConst(
        &"{name}{toItalic($node.kind)} {toCyan($node.text)}")


    of orgTokenKinds - {onkIdent, onkMarkup}:
      let txt = $node.text
      if '\n' in txt:
        return pptConst(
          &"{name}{toItalic($node.kind)}\n\"\"\"\n{toYellow(txt)}\n\"\"\"")

      else:
        return pptConst(
          &"{name}{toItalic($node.kind)} \"{toYellow(txt)}\"")

    of onkNowebMultilineBlock:
      var body: string
      for slice in node.nowebBlock.slices:
        if slice.isPlaceholder:
          body.add $toRed("<<")

        for ch in slice.slice:
          body.add ch

        if slice.isPlaceholder:
          body.add $toRed(">>")

      return pptConst(
        &"{name}{toItalic($node.kind)}\n\"\"\"\n{body}\n\"\"\"")

    of onkSnippetMultilineBlock:
      var body: string
      for slice in node.snippetBlock.slices:
        if slice.isPlaceholder:
          body.add $toRed("$")

        if slice.hasBody:
          body.add $toGreen("{")

        for ch in slice.slice:
          body.add ch

        if slice.hasBody:
          body.add $toGreen("}")

      return pptConst(
        &"{name}{toItalic($node.kind)}\n\"\"\"\n{body}\n\"\"\"")

    else:
      # If anyone wonders why nim is the best productivit language - this
      # is why. Quite simple task to be honest, but involves a lot of
      # moving parts that make life /so/ much easier.

      # Easily defining custom templates - code below is analogous to `?:`
      # ternary expression. It is not available, but like this would stop
      # me from rolling my own solution.
      let mark = tern(node.str.len > 0, &" <{toBlue(node.str)}>", "")

      # This is an example of block argument syntax - `pptObj` accepts
      # three arguments - name, style and list of fields. I pass first two
      # explicitly, and use `collect` macro for generating list of
      # subtrees.
      return pptObj(&"{name}{toItalic($node.kind)}{mark}", initStyle()):
        collect(newSeq): # this is a macro from stdlib, not some kind of
                         # special, built-in syntax.
          for idx, subnode in pairs(node):
            # `pairs` is defined as `iteratro` with barely four lines
            # implementation.

            # Last expression is accumulated as macro result.
            objTreeRepr(subnode, getSubnodeName(node.kind, idx))

func objTreeRepr*(sn: seq[OrgNode]): ObjTree =
  pptObj("seq[]", mapIt(sn, objTreeRepr(it)))

func objTreeRepr*(sn: seq[seq[OrgNode]]): ObjTree =
  pptObj("seq[[]]", mapIt(sn, objTreeRepr(it)))

func treeRepr*(node: OrgNode | seq[OrgNode] | seq[seq[OrgNode]]): string =
  node.objTreeRepr().treeRepr(backticks = false)

func lispRepr*(node: OrgNode | seq[OrgNode] | seq[seq[OrgNode]]): string =
  node.objTreeRepr().lispRepr()

{.push inline.}

proc newOrgIdent*(text: StrSlice): OrgNode =
  OrgNode(kind: onkIdent, text: text)

proc newOrgIdent*(text: string): OrgNode =
  OrgNode(kind: onkIdent, text: initStrSlice(text))

proc newBareIdent*(text: StrSlice): OrgNode =
  OrgNode(kind: onkBareIdent, text: text)

proc newTree*(kind: OrgNodeKind, text: StrSlice): OrgNode =
  assert kind in orgTokenKinds, $kind
  result = OrgNode(kind: kind)
  result.text = text


proc newTree*(
    kind: OrgNodeKind, subkind: OrgNodeSubKind, text: StrSlice): OrgNode =

  result = newTree(kind, text)
  result.subKind = subKind

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
    result.text = newStrBufSlice(str)

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
  result = OrgNode(kind: onkEmptyNode)
  result.subKind = subKind

proc isEmptyNode*(tree: OrgNode): bool =
  tree.kind == onkEmptyNode

proc newOStmtList*(subnodes: varargs[OrgNode]): OrgNode =
  onkStmtList.newTree(subnodes)

  # OrgNode(kind: onkStmtList)
proc newWord*(ptext: StrSlice): OrgNode = onkWord.newTree(ptext)

{.pop.}
