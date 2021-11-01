import
  ./org_types

import
  hmisc/core/all,
  hmisc/algo/[hlex_base, hparse_base, clformat],
  hmisc/macros/ast_spec

export strVal

import std/[tables, strformat, sequtils]

export clformat

func strVal*(node: OrgNode): string =
  case node.kind:
    of orgTokenKinds:
      return node.text.strVal()

    else:
      return node.str

func getPosStr*(node: OrgNode): PosStr =
  node.text

#=========================  Node specification  ==========================#
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
      0 as "properties":
        orgPropertyList or orgEmpty

      1 as "logbook":
        orgLogbook or orgEmpty

    orgLogbook:
      0 .. ^1 as "logs":
        orgLogbookStateChange or orgLogbookNote

    orgLogbookStateChange:
      0 as "newstate": orgBigIdent
      1 as "oldstate": orgBigIdent
      2 as "time": orgTimeStamp

    orgLogbookNote:
      0 as "time": orgTimeStamp
      1 as "text": orgStmtList



    orgPropertyList:
      0 .. ^1:
        orgProperty

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
      0 as "args": orgCmdArguments
      1 .. ^1 as "rows": orgTableRow

    orgTableRow:
      0 as "args":
        ## Optional arguments for row - can be specified using `#+row`. For
        ## pipe formatting this is not supported, so arguments would be an
        ## empty node.
        orgCmdArguments or orgEmpty


      1 as "text":
        ## It is possible to put text on the *row* level.
        orgParagraph or orgEmpty

      2 as "body":
        0 .. ^1:
          orgTableCell

    orgTableCell:
      0 as "args": orgCmdArguments
      1 as "text": orgParagraph or orgEmpty or orgStmtList

    orgCommand:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty

    orgCommandCaption:
      0 as "text": orgParagraph

    orgCommandOptions:
      0 as "args": orgCmdArguments


    orgCommandInclude:
      0 as "file": orgFilePath
      1 as "kind": orgEmpty or orgIdent
      2 as "lang": orgEmpty or orgIdent
      3 as "args": orgEmpty or orgCmdArguments

    orgCommandHeader:
      0 as "args": orgEmpty or orgCmdArguments

    orgCodeLine:
      0 .. ^1:
        orgCodeText or orgCodeTangle or orgCodeCallout or orgEmpty

    orgSrcCode:
      0 as "lang": orgIdent
      1 as "header-args": orgCmdArguments
      2 as "body":
        orgStmtList:
          0 .. ^1:
            orgCodeLine

      3 as "result": orgRawText or orgEmpty

    orgCallCode:
      0 as "name": orgIdent
      1 as "header-args": orgCmdArguments
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

generateFieldEnum(orgNodeSpec, "orgf")


func add*(node: var OrgNode, other: OrgNode | seq[OrgNode]) =
  if node.subnodes.len == 0:
    when other is seq:
      if other.len > 0:
        node.line = other[0].line
        node.column = other[0].column

    else:
      node.line = other.line
      node.column = other.column

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

func `[]`*(node: OrgNode, slice: HSlice[int, BackwardsIndex]): seq[OrgNode] =
  node.subnodes[slice]

func `[]`*(node: OrgNode, idx: BackwardsIndex): OrgNode =
  node.subnodes[idx]

func `[]=`*(node: var OrgNode, idx: BackwardsIndex, val: OrgNode) =
  node.subnodes[idx] = val

func `[]`*(node: var OrgNode, idx: int): var OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, idx: int): OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, name: string): OrgNode =
  node[getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))]

func `[]`*(node: var OrgNode, name: string): var OrgNode =
  node[getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))]

func `[]`*(node: OrgNode, field: OrgNodeField): OrgNode =
  node[field.toName()]

func `[]`*(node: var OrgNode, field: OrgNodeField): var OrgNode =
  node[field.toName()]

func `[]`*(node: OrgNode, name: string, kind: OrgNodeKind): OrgNode =
  let idx = getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))
  result = node[idx]
  if result.kind != kind:
    raise newUnexpectedKindError(
      result,
      "Node of kind ", mkind(node.kind),
      " was expected to have subnode of kind ",
      mkind(kind), " at position ", idx,
      "(positional name - '{name}')"
    )

func `[]`*(node: OrgNode, field: OrgNodeField, kind: OrgNodeKind): OrgNode =
  node[field.toName(), kind]

func `[]=`*(node: var OrgNode, idx: int, val: OrgNode) =
  node.subnodes[idx] = val

func `[]=`*(node: var OrgNode, name: string, val: OrgNode) =
  let idx = getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))
  while node.len - 1 < idx:
    node.add OrgNode(kind: orgEmptyNode)

  node[idx] = val

func `[]`*(node: var OrgNode, name: var string): var OrgNode =
  node[getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))]

template subKindErr*(subKindVal: OrgNodeSubKind): untyped {.dirty.} =
  raise OrgSubKindError(
    subkind: subKindVal,
    msg: "Unexpected node subkind - " & $subKindVal)

iterator items*(node: OrgNode): OrgNode =
  if not(node of orgTokenKinds):
    for n in node.subnodes:
      yield n

iterator mitems*(node: var OrgNode): var OrgNode =
  if not(node of orgTokenKinds):
    for n in mitems(node.subnodes):
      yield n

iterator pairs*(node: OrgNode): (int, OrgNode) =
  if not(node of orgTokenKinds):
    for idx, n in node.subnodes:
      yield (idx, n)

iterator mpairs*(node: OrgNode): (int, var OrgNode) =
  if not(node of orgTokenKinds):
    for idx, n in mpairs(node.subnodes):
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
  assertKind(
    kind,
    orgTokenKinds,
    "cannot initialize token token tree with given kind")

  result = OrgNode(kind: kind, line: text.line, column: text.column)
  result.text = text


proc newTree*(
    kind: OrgNodeKind, subkind: OrgNodeSubKind, text: PosStr): OrgNode =

  result = newTree(kind, text)
  result.subKind = subKind


# proc newTree*(kind: OrgNodeKind, tok: OrgCommandToken): OrgNode =
#   newTree(kind, initPosStr(tok))

proc newTree*(
    str: PosStr, kind: OrgNodeKind, subnodes: varargs[OrgNode]): OrgNode =
  result = OrgNode(kind: kind)

  result.line = str.line
  result.column = str.column
  for node in subnodes:
    result.subnodes.add node

proc newTree*(kind: OrgNodeKind, subnodes: varargs[OrgNode]): OrgNode =
  result = OrgNode(kind: kind)
  if 0 < subnodes.len:
    result.line = subnodes[0].line
    result.column = subnodes[0].column

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

proc newCmdArguments*(): OrgNode =
  newTree(
    orgCmdArguments,
    newTree(orgInlineStmtList),
    newTree(orgInlineStmtList))

proc newOrgEmptyNode*(): OrgNode = OrgNode(kind: orgEmptyNode)
proc newOrgEmpty*(): OrgNode = OrgNode(kind: orgEmptyNode)

proc isEmptyNode*(tree: OrgNode): bool =
  tree.kind == orgEmptyNode


proc newOStmtList*(subnodes: varargs[OrgNode]): OrgNode =
  orgStmtList.newTree(subnodes)

proc newWord*(ptext: PosStr): OrgNode = orgWord.newTree(ptext)



# echo orgNodeSpec.treeRepr()


const treeReprDisplay = hdisplay(flags += dfWithRanges)

proc treeRepr*(
    org: OrgNode,
    opts: HDisplayOpts = treeReprDisplay
  ): ColoredText =

  coloredResult()

  proc aux(
      n: OrgNode, level: int,
      name: Option[string]
    ) =

    addIndent(level)
    if isNil(n):
      add hshow(nil, opts)
      return

    add hshow(n.kind)

    if n.subKind != oskNone:
      add " ("
      add hshow(n.subKind)
      add ")"

    if opts.withRanges:
      add " "
      add hshow(n.line, opts)
      add ":"
      add hshow(n.column, opts)


    if name.isSome():
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

      else:
        var printed = false
        if n of orgParagraph:
          proc isMarkupWords(org: OrgNode): bool =
            org.kind in {orgWord} or
            (
              org.kind in {orgBold, orgItalic, orgWord, orgParagraph} and
               allIt(org, isMarkupWords(it))
            )

          proc formatMarkup(n: OrgNode): ColoredText =
            case n.kind:
              of orgWord:
                result.add "W["
                result.add hshow(n.strVal(), hdisplay(flags -= dfUseQuotes))
                result.add "]"

              of orgBold, orgParagraph:
                let pref =
                  case n.kind:
                    of orgWord: "B"
                    of orgParagraph: "P"
                    else: ""

                result.add pref & "["
                for sub in items(n):
                  result.add formatMarkup(sub)
                result.add "]"

              else: raise newUnexpectedKindError(n)


          var reslen = 0
          add "\n"
          var prevWord = false
          for idx, word in pairs(n):
            if isMarkupWords(word):
              if idx == 0:
                addIndent(level + 1)

              let formatted = formatMarkup(word)
              add formatted
              reslen += len(formatted)
              if 60 < reslen:
                reslen = 0
                add "\n"
                addIndent(level + 1)

              prevWord = true

            else:
              if not prevWord and idx > 0:
                add "\n"

              aux(word, level + 1, none(string))

              prevWord = false

        else:
          if not printed:
            for idx, sub in n:
              add "\n"
              let subErr = validateSub(orgNodeSpec, n, idx)
              if subErr.isSome():
                 add subErr.get().indent(level * 2 + 2)
                 add "\n"

              aux(sub, level + 1, orgNodeSpec.fieldName(n, idx))

        let err = validateSelf(orgNodeSpec, n)
        if err.isSome():
          add "\n"
          add err.get().indent(level * 2 + 2)


  aux(org, 0, none(string))
  endResult()
