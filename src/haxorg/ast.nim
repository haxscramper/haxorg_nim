import std/[lexbase, tables, strformat, sugar, sequtils]
import common, buf
import hpprint/hpprint_repr

import hmisc/helpers
import hmisc/types/colorstring

type
  OrgSubKindError* = ref object of CatchableError
    subkind: OrgNodeSubKind

  OrgUserNode* = ref object of RootObj
    ## User-defined org-mode node.
    ##
    ## - HINT :: This node is intended as an escape hatch for parser users
    ##   to add their own information into the tree. Parser and semcheck
    ##   won't generate nodes of this kind - this is handled only by final
    ##   user. Corresponding node kind is
    ##   [[code:OrgNodeKind.onkUserNode]]

  OrgNodeSubKind* = enum
    ## Additional node classification that does not warrant own AST
    ## structure, but could be very useful for further processing.
    ##
    ## This listtries to cover *all* possible combinations of uses for each
    ## identifier.
    oskNone


    oskBold ## Node is bold text
    oskItalic
    oskVerbatim
    oskMonospaced
    oskBacktick
    oskUnderline
    oskStrike
    oskQuote ## Line quote text `> sometext`
    oskAngle


    oskDescriptionTagText ## Description list tag text
    oskLinkContent ## Link description text
    oskTitleText ## Paragraph in title of the subtree
    oskCaptionText ## Paragraph in `#+caption:`
    oskListHeaderText
    oskListBodyText
    oskListTagText
    oskStandaloneText
    oskSrcInlineText
    oskCallInlineText

    oskMetatagText ## Raw content of the metatag
    oskMetatagArgs
    oskLinkAddress
    oskComment
    oskMetaTag

    oskHashTagIdent
    oskSymbolIdent
    oskBracTagIdent
    oskOrgTagIdent
    oskMetaTagIdent
    oskTodoIdent


    oskDashBullet
    oskPlusBullet
    oskStarBullet

    oskRomanBullet
    oskNumBullet
    oskLetterBullet

    oskOrderedList
    oskUnorderedList
    oskMixedList
    oskFullDescList
    oskPartialDescList



    oskText
    oskSpace
    oskParen
    oskBracket
    oskCurly
    oskPunct
    oskBigWord






  OrgNodeKind* = enum
    ## Different kinds of org-mode nodes produces by parser.
    ##
    ## Note that it does not directly map to document in a way that one
    ## might expect, mainly due to extensibility of the org-mode. For
    ## example there is no `onkExampleBlock` (for `#+begin-example`), but
    ## instead it is represented as `MultilineCommand[Ident["example"]]`.
    ## This is a little more verbose, but allows to use single
    ## `MultilineCommand` node for anything, including source code,
    ## examples and more. Though /some/ command blocks that are
    ## /especially/ important do have their own node kinds and syntax (such
    ## as source code blocks)
    ##
    ## Most mulitline commands have corresponding single-line versions, and
    ## sometimes an inline too. Notable example are passthrough blocks -
    ## you can write `#+html: <some-html-code>`, `#+begin-export html` and
    ## finally `@@html: <html-code>@@`. One and multi-line blocks usually
    ## have similar syntax, but inline ones are pretty different. #[ DOC why? ]#
    ##
    ## There is no difference between multi-line and inline commands blocks
    ## in AST. #[ REVIEW is this a good idea, maybe separating those two
    ## would make things more intuitive? ]#
    ##
    ## #[ All ? ]# Elements that have inline, single-line and multiline
    ## versions are
    ##
    ## - `onkPassCode` :: Passthrough block of code to particular backend
    ## - `onkCallCode` :: Evaluate named code block
    ## - `onkSrcCode` :: Named code block
    onkNone  ## Default valye for node - invalid state

    onkDocument ## Toplevel part of the ast, not created by parser, and
                ## only used in `semorg` stage

    onkUserNode ## User-defined node [[code:OrgUserNode]]

    onkEmptyNode ## Empty node - valid state that does not contain any
                 ## value

    onkStmtList ## List of statements, possibly recursive. Used as toplevel
    ## part of the document, in recursive parsing of subtrees, or as
    ## regular list, in cases where multiple subnodes have to be grouped
    ## together.

    onkAssocStmtList ## Associated list of statements - AST elements like
    ## commands and links are grouped together if placed on adjacent lines

    onkSubtree ## Section subtree
    onkSubtreeTimes

    onkCompletion ## Task compleation cookie, indicated either in percents
    ## of completion, or as `<done>/<todo>` ratio.

    onkCheckbox ## Single checkbox item like `[X]` or `[-]`

    onkList
    onkListItem
    onkCounter

    onkComment ## Inline or trailling comment. Can be used addition to
    ## `#+comment:` line or `#+begin-comment` section. Nested comment
    ## syntax is allowed (`#[ level1 #[ level2 ]# ]#`), but only outermost
    ## one is represented as separate AST node, everything else is a
    ## `.text`

    onkRawText ## Raw string of text from input buffer. Things like
    ## particular syntax details of every single command, link formats are
    ## not handled in parser, deferring formatting to future processing
    ## layers

    onkCommand ## Single-line command

    onkMultilineCommand ## Multiline command such as code block, latex
    ## equation, large block of passthrough code. Some built-in org-mode
    ## commands do not requires `#+begin` prefix, (such as `#+quote` or
    ## `#+example`) are represented by this type of block as well.

    onkResult ## Command evaluation result

    onkIdent ## regular identifier - `alnum + [-_]` characters for
    ## punctuation. Identifiers are compared and parsed in
    ## style-insensetive manner, meaning `CODE_BLOCK`, `code-block` and
    ## `codeblock` are identical.

    onkBareIdent ## Bare identifier - any characters are allowed

    onkBigIdent ## full-uppsercase identifier such as `MUST` or `TODO`

    onkVerbatimMultilineBlock ## Verbatim mulitiline block that *might* be
    ## a part of `onkMultilineCommand` (in case of `#+begin-src`), but not
    ## necessarily. Can also be a part of =quote= and =example= multiline
    ## blocks.

    # TODO implement as separate node kind, different from regular non-leaf
    # subnodes.
    onkNowebMultilineBlock ## Source code block that was parsed for noweb
    ## interpolation.

    onkSnippetMultilineBlock ## Source code block that was parsed to be
    ## used as snippet. It is quite close to `noweb`, but is added to
    ## support literate snippets.

    onkSrcCode ## Block of source code - can be multiline, single-line and
    ## inline (such as `src_nim`). Lattern is different from regular
    ## monospaced text inside of `~~` pair as it contains additional
    ## internal structure, optional parameter for code evaluation etc.

    onkCallCode ## Call to named source code block. Inline, multiline, or
    ## single-line.

    onkPassCode ## Passthrough block. Inline, multiline, or single-line.
    ## Syntax is `@@<backend-name>:<any-body>@@`. Has line and block syntax
    ## respectively

    onkCmdArguments ## Command arguments

    onkCmdFlag ## Flag for source code block. For example `-n`, which is
    ## used to to make source code block export with lines

    onkCmdValue ## Key-value pairs for source code block evaluatio. Things
    ## like `:noweb false`

    onkCmdFuncArg ## Key-value pair for source code block call. As example
    ## - `:var x=random` will be parsed as `CmdValue[Ident("var"),
    ## CmdFuncArg[Ident("x"), RawStr("random")]]`

    onkUrgencyStatus ## Subtree importance level, such as `[#A]` or `[#B]`.
    ## Default org-mode only allows single character for contents inside of
    ## `[]`, but this parser makes it possible to use any regular
    ## identifier, such as `[#urgent]`.

    onkParagraph ## Single 'paragraph' of text. Used as generic container
    ## for any place in AST where unordered sentence might be encountered -
    ## not limited to actual paragraph

    onkMarkup ## Region of text with formatting, which contains standalone
    ## words - can itself contain subnodes, which allows to represent
    ## nested formatting regions, such as `*bold /italic/*` text.
    ## Particular type of identifier is stored in string form in `str`
    ## field for `OrgNode` - bold is represented as `"*"`, italic as `/`
    ## and so on. In case of explicit open/close pairs only opening one is
    ## stored.
    ##
    ## NOTE: when structured sentences are enabled, regular punctuation
    ## elements like `some text (notes)` are also represented as `Word,
    ## Word, Markup(str: "(", [Word])` - e.g. structure is not fully flat.

    onkMath ## Inline latex math. Moved in separate node kinds due to
    ## *very* large differences in syntax. Contains latex math body
    ## verbatim.

    onkWord ## Regular word - technically not different from `onkIdent`,
    ## but defined separately to disiguish between places where special
    ## syntax is required and free-form text.

    onkLink ## External or internal link. Consists of one or two elements -
    ## target (url, file location etc.) and description (`onkParagraph` of
    ## text). Description might be empty, and represented as empty node in
    ## this case. For external links particular formatting of the address
    ## is not handled by parser and instead contains raw string from input
    ## text.

    onkMacro ## Org-mode macro replacement - during export each macro is
    ## expanded and evaluated according to it's environment. Body of the
    ## macro is not parsed fully during org-mode evaluation, but is checked
    ## for correct parenthesis balance (as macro might contain elisp code)

    onkBackendRaw ## Raw content to be passed to a particular backend. This
    ## is the most compact way of quoting export strings, after
    ## `#+<backend>: <single-backend-line>` and `#+begin-export <backend>`
    ## `<multiple-lines>`.

    onkSymbol ## Special symbol that should be exported differently to
    ## various backends - greek letters (`\alpha`), mathematical notations
    ## and so on.

    onkTimeStamp ## Single date and time entry (active or inactive),
    ## possibly with repeater interval. Is not parsed directly, and instead
    ## contains `onkRawText` that can be parsed later

    onkTimeRange ## Date and time range format - two `onkDateTime` entries

    onkTable ## Org-mode table. Tables can be writtein in different
    ## formats, but in the end they are all represented using single ast
    ## type. NOTE: it is not guaranteed that all subnodes for table are
    ## exactly `onkTableRow` - sometimes additional property metadata might
    ## be used, making AST like `Table[AssocStmtList[Command[_],
    ## TableRow[_]]]` possible

    onkTableRow ## Horizontal table row
    onkTableCell ## Single cell in row. Might contain anyting, including
    ## other tables, simple text paragraph etc.

    onkFootnote ## Footnote entry. Just as regular links - internal content
    ## is not parsed, and instead just cut out verbatim into target AST
    ## node.

    onkHorizontal ## Horizotal rule. Rule body might contain other
    ## subnodes, to represnt `---- some text ----` kind of formatting.

    onkOrgTag ## Original format of org-mode tags in form of `:tagname:`.
    ## Might contain one or mode identifgiers, but does not provide support
    ## for nesting - `:tag1:tag2:`. Can only be placed within restricted
    ## set of places such as subtree headings and has separate place in AST
    ## when allowed (`onkSubtree` always has subnode `№4` with either
    ## `onkEmpty` or `onkOrgTag`)

    onkHashTag ## More commonly used `#hashtag` format, with some
    ## additional extension. Can be placed anywere in the document
    ## (including section headers), but does not have separate place in AST
    ## (e.g. considered regular part of the text)

    onkMetaTag ## Javadoc/doxygen-like metatag. Extension to org mode
    ## syntax, making it more sutiable for writing documentation. Several
    ## differen ways of writing are supported, starting from regular -
    ## `@tag arg;`, to `@tag[arg1, arg2]{tag body}` Semicolon is mandatory
    ## for metatag without curly braces enclosing body, but otherwise.
    ## Correct metatag should have three subnodes - `Ident`, `RawStr` and
    ## any other subnode kind for body.

    onkBracTag ## Custom extension to org-mode. Similarly to `BigIdent`
    ## used to have something like informal keywords `MUST`, `OPTIONAL`,
    ## but instead aimed /specifically/ at commit message headers -
    ## `[FEATURE]`, `[FIX]` and so on.

    onkDrawer ## Single enclosed drawer like `:properties: ... :end:` or
    ## `:logbook: ... :end:`

    onkProperty ## Property entry, either in `#+property:` command, or in
                ## `:property:` drawer

    onkPlaceholder ## Placeholder entry in text, usually writte like `<text
                   ## to replace>`

    onkRadioTarget

  # TODO allow for macro replacement to be used as identifiers in cases
  # like `@@{{{backend}}}:<b>@@`

const orgTokenKinds* = {
  onkIdent,
  onkBareIdent,
  onkRawText,
  onkBigIdent,
  onkUrgencyStatus,
  onkVerbatimMultilineBlock,
  onkWord,
  onkMath,
  onkComment,
  onkCheckbox,
  onkCounter,
  onkCompletion,
  onkSymbol,
  onkTimeStamp,
  onkEmptyNode
}

const orgSubnodeKinds* = {
  low(OrgNodeKind) .. high(OrgNodeKind)
} - orgTokenKinds - {
  onkNowebMultilineBlock, onkSnippetMultilineBlock, onkUserNode
}

const orgAllKinds* = { low(OrgNodeKind) .. high(OrgNodeKind) }

type
  OskMarkupKindsRange* = range[oskBold .. oskAngle]

type
  NowebSlice* = object
    isPlaceholder*: bool
    slice*: StrSlice

  NowebBlock* = object
    slices*: seq[NowebSlice]

  SnippetSlice* = object
    hasBody*: bool
    isPlaceholder*: bool
    slice*: StrSlice

  SnippetBlock* = object
    slices*: seq[SnippetSlice]

  OrgNodeObj* = object
    subkind*: OrgNodeSubKind
    case kind*: OrgNodeKind
      of orgTokenKinds:
        text*: StrSlice

      of onkNowebMultilineBlock:
        nowebBlock*: NowebBlock

      of onkSnippetMultilineBlock:
        snippetBlock*: SnippetBlock

      of onkUserNode:
        userNode*: OrgUserNode

      else:
        ranges*: StrRanges
        str*: string
        subnodes*: seq[OrgNode]


  OrgNode* = ref OrgNodeObj

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
  node[getNamedSubnode(node.kind, name)]


func `[]=`*(node: var OrgNode, idx: int, val: OrgNode) =
  node.subnodes[idx] = val

func `[]=`*(node: var OrgNode, name: string, val: OrgNode) =
  node[getNamedSubnode(node.kind, name)] = val

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
    return pptConst(name & toBlue("<nil>"))


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
          body.add toRed("<<")

        for ch in slice.slice:
          body.add ch

        if slice.isPlaceholder:
          body.add toRed(">>")

      return pptConst(
        &"{name}{toItalic($node.kind)}\n\"\"\"\n{body}\n\"\"\""
      )

    of onkSnippetMultilineBlock:
      var body: string
      for slice in node.snippetBlock.slices:
        if slice.isPlaceholder:
          body.add toRed("$")

        if slice.hasBody:
          body.add toGreen("{")

        for ch in slice.slice:
          body.add ch

        if slice.hasBody:
          body.add toGreen("}")

      return pptConst(
        &"{name}{toItalic($node.kind)}\n\"\"\"\n{body}\n\"\"\""
      )

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
