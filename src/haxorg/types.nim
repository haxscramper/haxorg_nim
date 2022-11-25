import
  haxorg/[
    enum_types,
    misc_types
  ],
  hmisc/algo/[
    hlex_base,
    hparse_base,
    clformat
  ],
  hmisc/core/[
    all
  ],
  hmisc/other/[
    hshell,
    hargparse,
    oswrap
  ],
  hmisc/macros/[
    ast_spec
  ],
  std/[
    options,
    tables,
    uri,
    sequtils,
    strformat
  ]

export enum_types, misc_types

import std/macros


#=============================  Error types  =============================#

type
  OrgUserNode* = ref object of RootObj
    ## User-defined org-mode node.
    ##
    ## - HINT :: This node is intended as an escape hatch for parser users
    ##   to add their own information into the tree. Parser and semcheck
    ##   won't generate nodes of this kind - this is handled only by final
    ##   user. Corresponding node kind is
    ##   [[code:OrgNodeKind.orgUserNode]]
    testField: char

type
  OrgToken* = HsTok[OrgTokenKind]
  OrgLexer* = HsLexer[OrgToken]


type
  OrgNodeObj* = object
    # subkind*: OrgNodeSubKind
    line*: int
    column*: int
    subnodes*: seq[OrgNode]
    case kind*: OrgNodeKind
      of orgTokenKinds:
        token*: OrgToken

      of orgUserNode:
        userNode*: OrgUserNode

      of orgError:
        # TODO implement support for error reporting directly in the AST
        # using error nodes.
        report*: int

      else:
        ranges*: PosStr
        str*: string


  OrgNode* = ref OrgNodeObj



type
  Lexer* = object
    tokens*: seq[OrgToken]
    pos*: int

  ParseInstInfo* = object
    line*: int
    col*: int
    file*: string
    procname*: string

  ParseConf* = object
    dropEmptyWords*: bool
    parseEnter*: proc(loc: ParseInstInfo, lex: Lexer)
    parseLeave*: proc(loc: ParseInstInfo, lex: Lexer, node: OrgNode)

macro parse*(def: untyped): untyped =
  result = def
  let procname = def[0][1].toStrLit()
  let start = quote do:
    let (file, line, col) = instantiationInfo(fullPaths = true)
    let iinfo = ParseInstInfo(
      line: line,
      col: col,
      file: file,
      procname: `procname`
    )

    if not parseConf.parseEnter.isNil():
      parseConf.parseEnter(iinfo, lex)

    defer:
      if not parseConf.parseLeave.isNil():
        parseConf.parseLeave(iinfo, lex, result)

  result[^1] = newStmtList(start & toSeq(def[^1]))

const defaultParseConf*: ParseConf = ParseConf(
  dropEmptyWords: true
)

func strVal*(node: OrgNode): string =
  case node.kind:
    of orgTokenKinds:
      return node.token.strVal()

    else:
      return node.str

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
      6 as "times": orgStmtList or orgEmpty
      7 as "drawer": orgDrawer
      8 as "body": orgStmtList or orgEmpty

    orgDrawer:
      0 as "properties":
        orgPropertyList or orgEmpty

      1 as "logbook":
        orgLogbook or orgEmpty

      2 as "description":
        orgSubtreeDescription or orgEmpty

    orgSubtreeDescription:
      0 as "text": orgParagraph

    orgLogbook:
      0 .. ^1 as "logs":
        orgLogbookStateChange or
        orgLogbookNote or
        orgLogbookRefile or
        orgLogbookClock

    orgLogbookStateChange:
      0 as "newstate": orgBigIdent or orgEmpty
      1 as "oldstate": orgBigIdent or orgEmpty
      2 as "time": orgTimeStamp or orgEmpty
      3 as "text": orgStmtList or orgEmpty

    orgLogbookRefile:
      0 as "time": orgTimeStamp
      1 as "from": orgLink
      2 as "text": orgStmtList or orgEmpty

    orgLogbookNote:
      0 as "time": orgTimeStamp
      1 as "text": orgStmtList or orgEmpty

    orgTimeAssoc:
      0 as "name": orgBigIdent or orgEmpty
      1 as "time": orgTimeStamp or orgTimeRange

    orgLogbookClock:
      0 as "time": orgTimeRange or orgTimeStamp

    orgTimeRange:
      0 as "from": orgTimeStamp
      1 as "to": orgTimeStamp
      2 as "diff": orgSimpleTime or orgEmpty


    orgPropertyList:
      0 .. ^1:
        orgProperty or orgPropertyAdd

    orgPropertyAdd:
      0 as "name": orgRawText
      1 as "subname"
      2 as "values"

    orgProperty:
      0 as "name": orgRawText
      1 as "subname"
      2 as "values"

    orgMultilineCommand:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty
      2 as "body"

    orgMetaSymbol:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty
      2 as "body": orgRawText

    orgTable:
      0 as "args": orgCmdArguments or orgEmpty
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
      0 as "args": orgCmdArguments or orgEmpty
      1 as "text": orgEmpty or orgStmtList

    orgCommand:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty

    orgCommandCaption:
      0 as "text": orgParagraph

    orgCommandOptions:
      0 .. ^1 as "args":
        orgRawText

    orgCommandInclude:
      0 as "file": orgFile
      1 as "kind": orgEmpty or orgIdent
      2 as "lang": orgEmpty or orgIdent
      3 as "args": orgEmpty or orgCmdArguments

    orgCommandHeader:
      0 as "args": orgEmpty or orgCmdArguments

    orgCodeLine:
      0 .. ^1:
        orgCodeText or orgCodeTangle or orgCodeCallout or orgEmpty

    orgSrcCode:
      0 as "lang": orgIdent or orgEmpty
      1 as "header-args": orgCmdArguments or orgEmpty
      2 as "body":
        orgStmtList:
          0 .. ^1:
            orgCodeLine

      3 as "result": orgRawText or orgEmpty

    orgSrcInlineCode:
      0 as "lang": orgIdent or orgEmpty
      1 as "header-args": orgCmdArguments or orgEmpty
      2 as "body": orgCodeLine

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
        orgParagraph or orgEmptyNode

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
      0 as "protocol"
      1 as "link"
      2 as "desc"

generateFieldEnum(orgNodeSpec, "orgf")


func `==`*(lhs, rhs: OrgToken): bool =
  lhs.kind == rhs.kind and lhs.strVal() == rhs.strVal()

func add*(node: var OrgNode, other: OrgNode | seq[OrgNode]) =
  assert node.kind notin orgTokenKinds, $node.kind
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

func last*(node: var OrgNode): var OrgNode = node[^1]
func last*(node: OrgNode): OrgNode = node[^1]

# template subKindErr*(subKindVal: OrgNodeSubKind): untyped {.dirty.} =
#   raise OrgSubKindError(
#     subkind: subKindVal,
#     msg: "Unexpected node subkind - " & $subKindVal)

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
# proc `$`*(org: OrgNodeSubKind): string {.inline.} = toString(org)[3 ..^ 1]


proc newTree*(kind: OrgNodeKind, token: OrgToken): OrgNode =
  result = OrgNode(kind: kind)
  result.token = token

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
  kind: OrgNodeKind, subnodes: openarray[(string, OrgNode)]): OrgNode =
  result = OrgNode(kind: kind)
  for (key, node) in subnodes:
    result[key] = node

proc newEmptyNode*(): OrgNode =
  result = OrgNode(kind: orgEmptyNode)

const table = getNodeRanges(orgNodeSpec)

proc newEmptiedTree*(kind: OrgNodeKind): OrgNode =
  ## Create new org node with all named subnodes set to `empty`.
  result = OrgNode(kind: kind)
  for name, _ in table[kind]:
    result[name] = newEmptyNode()

proc newError*(subnode: varargs[OrgNode]): OrgNode =
  ## Create new error error node with given subnodes
  newTree(orgError, @subnode)

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

const treeReprDisplay = hdisplay(flags += dfWithRanges)

func `$`*(node: OrgNode): string =
  result = &"({node.kind} "
  case node.kind:
    of orgTokenKinds:
      result &= &"\"{node.strVal()}\")"

    else:
      var first = true
      result &= "["
      for sub in node:
        if not first: result &= ", "
        result &= $sub
      result &= "])"


proc orgSubnodeFieldName*(node: OrgNode, idx: int): Option[string] =
  orgNodeSpec.fieldName(node, idx)

proc orgIsSingularField*(node: OrgNode, idx: int): bool =
  orgNodeSpec.isSingleField(node, idx)

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
    add " "
    add hshow(int(n.kind))

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
          n.strVal(),
          hdisplay(flags -= dfSpellEmptyStrings))

      of orgEmpty:
        discard

      else:
        var printed = false
        if n of orgParagraph and false:
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

              aux(sub, level + 1, orgSubnodeFieldName(n, idx))

        let err = validateSelf(orgNodeSpec, n)
        if err.isSome():
          add "\n"
          add err.get().indent(level * 2 + 2)


  aux(org, 0, none(string))
  endResult()
