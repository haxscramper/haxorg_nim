import std/[lexbase, tables, strformat, sugar, sequtils]
import common
import hpprint, hpprint/hpprint_repr

import hmisc/helpers
import hmisc/types/colorstring

type
  OrgNodeKind* = enum
    ## Different kinds of org-mode nodes produces by parser.
    ##
    ## Note that it does not directly map to document in a way that one
    ## migght expect, mainly due to extensibility of the org-mode. For
    ## example there is no `onkExampleBlock` (for `#+begin-example`), but
    ## instead it is represented as `MultilineCommand[Ident["example"]]`.
    ## This is a little more verbose, but allows to use single
    ## `MultilineCommand` node for anything, including source code,
    ## examples and more.
    ##
    ## Most mulitline commands have corresponding single-line versions, and
    ## sometimes an inline too. Notable example are passthrough blocks -
    ## you can write `#+html: <some-html-code>`, `#+begin-export html` and
    ## finally `@@html: <html-code>@@`. One and multi-line blocks usually
    ## have similar syntax, but inline ones are pretty different.

    onkNone  ## Default valye for node - invalid state

    onkEmptyNode ## Empty node - valid state that does not contain any
                 ## value

    onkStmtList ## List of statements, possibly recursive. Used as toplevel
    ## part of the document, or in recursive parsing of subtrees.

    onkAssocStmtList ## Associated list of statements - AST elements like
    ## commands and links are grouped together if placed on adjacent lines

    onkSubtree ## Section subtree

    onkCompletion ## Task compleation cookie, indicated either in percents
    ## of completion, or as `<done>/<todo>` ratio.

    onkCheckbox ## Single checkbox item like `[X]` or `[-]`

    onkBulletList ## Regular list. Particular prefix used is stored in
    ## `.str`.

    onkNumberList ## Numbered list. Can use different numeric literals,
    ## concrete one can be determined from `.str` field (`1`, `I`, or
    ## `a.`).
    ##
    ## Each subnode of a list represents single `StmtList` entry. Line
    ## checkboxes placed as first element of the list are considered to
    ## have separate meaning, all other ones are ignored.
    ##
    ## List does not store it's indentation/nesting levels, as well as used
    ## indicies for particular elements as written in text. (e.g. if you
    ## start list with `999.` this would be discarded)


    onkPropertyList ## Org-mode property list. Subnodes are represented as
    ## `PropertyListLine`, otherwise all structural rules apply as regular
    ## list

    onkPropertyListLine ## Single line in property list. Containts two
    ## nodes - property-name, and property value.
    ##
    ## Dialogue quotes are parsed as property lists too `user :> "Hello"`

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

    onkCodeMultilineBlock ## Verbatim mulitiline block that *might* be a
    ## part of `onkMultilineCommand` (in case of `#+begin-src`), but not
    ## necessarily.

    onkInlineSrc ## Inline block of code, such as `src_nim`. It is
    ## different from regular monospaced text inside of `~~` pair as it
    ## contains additional internal structure, optional parameter for code
    ## evaluation etc.

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

    onkPass ## Inline passthrough block. Syntax is
    ## `@@<backend-name>:<any-body>@@`. Has line and block syntax
    ## respectively

    onkWord ## Regular word - technically not different from `onkIdent`,
    ## but defined separately to disiguish between places where special
    ## syntax is required and free-form text.

    onkLink ## External or internal link. Consists of one or two elements -
    ## target (url, file location etc.) and description (`onkParagraph` of
    ## text). Description might be empty, and represented as empty node in
    ## this case. For external links particular formatting of the address
    ## is not handled by parser and instead contains raw string from input
    ## text.

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

    onkMetaTag ## Javadoc/doxygen-like metatag. Extension to org mode
    ## syntax, making it more sutiable for writing documentation. Several
    ## differen ways of writing are supported, starting from regular -
    ## `@tag arg;`, to `@tag[arg1, arg2]{tag body}` Semicolon is mandatory
    ## for metatag without curly braces enclosing body, but otherwise.
    ## Correct metatag should have three subnodes - `Ident`, `RawStr` and
    ## any other subnode kind for body.

    onkDrawer ## Single enclosed drawer like `:properties: ... :end:` or
    ## `:logbook: ... :end:`

    onkProperty

  # TODO allow for macro replacement to be used as identifiers in cases
  # like `@@{{{backend}}}:<b>@@`

const orgTokenKinds = {
  onkIdent,
  onkBareIdent,
  onkRawText,
  onkBigIdent,
  onkUrgencyStatus,
  onkCodeMultilineBlock,
  onkWord,
  onkMath,
  onkComment
}

type
  OrgNodeObj* = object
    case kind*: OrgNodeKind
      of orgTokenKinds:
        text*: PosText

      else:
        str*: string
        subnodes*: seq[OrgNode]


  OrgNode* = ref OrgNodeObj

func getSubnodeName(kind: OrgNodeKind, idx: int): string =
  template fail(): untyped = "<<fail>>"

  case kind:
    of onkSubtree:
      case idx:
        of 0: "prefix"
        of 1: "todo"
        of 2: "urgency"
        of 3: "title"
        of 5: "completion"
        of 4: "tags"
        of 6: "drawers"
        of 7: "body"
        else: fail()

    of onkProperty, onkDrawer:
      case idx:
        of 0: "name"
        of 1: "body"
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


func getNamedSubnode(kind: OrgNodeKind, name: string): int =
  if (kind, name) in nodeNames:
    return nodeNames[(kind, name)]

  else:
    raiseAssert(&"Node of kind '{kind}' does not have named subtre '{name}'")




{.push inline.}

func add*(node: var OrgNode, other: OrgNode | seq[OrgNode]) =
  node.subnodes.add other

func len*(node: OrgNode): int = node.subnodes.len

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

func `[]`*(node: var OrgNode, idx: int): var OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, idx: int): OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, name: string): OrgNode =
  node[getNamedSubnode(node.kind, name)]

func `[]`*(node: var OrgNode, name: var string): var OrgNode =
  node[getNamedSubnode(node.kind, name)]

{.pop.}

iterator items*(node: OrgNode): OrgNode =
  for n in node.subnodes:
    yield n

iterator pairs*(node: OrgNode): (int, OrgNode) =
  for idx, n in node.subnodes:
    yield (idx, n)

proc toString(x: OrgNodeKind): string {.magic: "EnumToStr", noSideEffect.}

proc `$`*(onk: OrgNodeKind): string {.inline.} = toString(onk)[3 ..^ 1]
proc `$`*(text: PosText): string =
  &"\"{text.text}\""

func objTreeRepr*(node: OrgNode, name: string = "<<fail>>"): ObjTree =
  let name = tern(name != "<<fail>>", &"({toGreen(name)}) ", "")
  if node.isNil:
    return pptConst(name & toBlue("<nil>"))

  case node.kind:
    of onkIdent:
      return pptConst(
        &"{name}{toItalic($node.kind)} {toGreen(node.text.text)}")


    of orgTokenKinds - {onkIdent, onkMarkup}:
      let txt = toYellow(node.text.text)
      if '\n' in node.text.text:
        return pptConst(
          &"{name}{toItalic($node.kind)}\n\"\"\"\n{txt}\n\"\"\"")

      else:
        return pptConst(
          &"{name}{toItalic($node.kind)} \"{txt}\"")

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

proc newOrgIdent*(text: PosText): OrgNode = OrgNode(kind: onkIdent, text: text)
proc newBareIdent*(text: PosText): OrgNode = OrgNode(kind: onkBareIdent, text: text)
proc newTree*(kind: OrgNodeKind, text: PosText): OrgNode =
  result = OrgNode(kind: kind)
  result.text = text

proc newTree*(
    kind: OrgNodeKind,
    position: Position,
    text: string
  ): OrgNode {.inline.} =

  newTree(kind, initPosText(text, position))


proc newTree*(kind: OrgNodeKind, subnodes: varargs[OrgNode]): OrgNode =
  result = OrgNode(kind: kind)
  for node in subnodes:
    result.subnodes.add node

proc newTree*(
    kind: OrgNodeKind, str: string, subnodes: varargs[OrgNode]
  ): OrgNode {.inline.} =

  result = newTree(kind, subnodes)
  result.str = str


proc newEmptyNode*(): OrgNode = OrgNode(kind: onkEmptyNode)
proc newOStmtList*(): OrgNode = OrgNode(kind: onkStmtList)
proc newWord*(ptext: PosText): OrgNode = onkWord.newTree(ptext)

{.pop.}
