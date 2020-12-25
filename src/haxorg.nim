import std/[streams, strutils, parseutils, strscans,
            macros, strformat, sequtils, tables, sugar]

import std/lexbase except Newlines

import fusion/matching

import hmisc/hexceptions
import hmisc/hdebug_misc
import hmisc/helpers
import hmisc/types/colorstring
import hpprint, hpprint/hpprint_repr

type
  OrgTokenKind* = enum
    otkNone

    otkCommand
    otkBeginCommand
    otkIdent
    otkSubtreeStart
    otkListStart

  OrgNodeKind* = enum
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

    onkInlineBlock ## Inline block of code, such as `src_nim`. It is
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

  Position = object
    ## Some position in document
    line*: int ## Position line
    column*: int ## Position column
    offset*: int ## Offset from absolute document start

  PosText = object
    line*: int
    column*: int
    text*: string

  OrgToken* = object
    kind*: OrgTokenKind
    indent*: int
    text*: PosText


const orgTokenKinds = {
  onkIdent,
  onkBareIdent,
  onkRawText,
  onkBigIdent,
  onkUrgencyStatus,
  onkCodeMultilineBlock,
  onkWord
}

const
  identChars = {'a' .. 'z', 'A' .. 'Z', '_', '-'}
  bigIdentChars = {'A' .. 'Z'}
  bareIdentChars = AllChars - Whitespace

type
  OrgNodeObj* = object
    case kind*: OrgNodeKind
      of orgTokenKinds:
        text*: PosText

      else:
        str*: string
        subnodes*: seq[OrgNode]


  OrgNode* = ref OrgNodeObj

  MarkupToggle = enum
    mtLatex
    mtBold
    mtItalic
    mtVerbatim
    mtCode
    mtUnderscore
    mtStrikethrough

  PosIncrements = Table[int, Position]

  Lexer* = object of BaseLexer
    currIndent: int ## Current indentation level. When parsing over newline
    ## followed by whitespaces, `currIdent` is used to automatically skip
    ## them. To check for actulual indentation of the next line use
    ## `nextLineIndent()`.

    column: int ## Current column in lexer - used for correct annotation
    ## positioning, renewal of `PosText` buffers

    curr: OrgToken # REFACTOR remove
    positionIncrements: PosIncrements ## Additional position increments for
    ## sublexers. Contains mapping between buffer character positions and
    ## line/column increments. This allows to cut parts of original input
    ## stream with some characters discarded into sublexers, while still
    ## maintaining correct positional information. `Position` contains
    ## `line` and `column` increment, as well as number of skipped
    ## characters in `offset` field.

iterator items*(node: OrgNode): OrgNode =
  for n in node.subnodes:
    yield n

iterator pairs*(node: OrgNode): (int, OrgNode) =
  for idx, n in node.subnodes:
    yield (idx, n)

proc toString(x: OrgNodeKind): string {.magic: "EnumToStr", noSideEffect.}

proc `$`(onk: OrgNodeKind): string {.inline.} = toString(onk)[3 ..^ 1]
proc `$`(text: PosText): string =
  &"\"{text.text}\""

func `[]`(pos: Position, idx: static[FieldIndex]): int =
  when idx == 0:
    pos.line

  elif idx == 1:
    pos.column

  elif idx == 2:
    pos.offset

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


func objTreeRepr*(node: OrgNode, name: string = "<<fail>>"): ObjTree =
  let name = tern(name != "<<fail>>", &"({toGreen(name)}) ", "")
  if node.isNil:
    return pptConst(name & toBlue("<nil>"))

  case node.kind:
    of onkIdent:
      return pptConst(&"{name}{node.kind} {toGreen(node.text.text)}")


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

using lexer: var Lexer






{.push inline.}

proc `line=`(lexer; line: int) = lexer.lineNumber = line

proc line(lexer): int = lexer.lineNumber

proc `[]`(lexer): char =
  lexer.buf[lexer.bufpos]

proc `[]`(lexer; idx: int): char =
  if idx + lexer.bufpos < 0:
    EndOfFile

  else:
     lexer.buf[lexer.bufpos + idx]

proc `[]`(lexer; slice: Slice[int]): string =
  result = lexer.buf[lexer.bufpos + slice.a .. min(
    lexer.bufpos + slice.b,
    lexer.buf.high
  )]

proc `[]`(lexer; slice: HSlice[int, BackwardsIndex]): string =
  lexer.buf[lexer.bufpos + slice.a .. lexer.buf.high]


proc `@?`(lexer; slice: Slice[int]): seq[char] = @(lexer[slice])

proc `[]`(lexer; str: string): bool =
  result = lexer.buf[lexer.bufpos ..< lexer.bufpos + str.len] == str

func initPosText(text: string, line, column: int): PosText =
  PosText(text: text, line: line, column: column)

func len(text: PosText): int = text.text.len
func add(text: var PosText, ch: char) = text.text.add ch
func high(text: PosText): int = text.text.len - 1

func add(node: var OrgNode, other: OrgNode | seq[OrgNode]) =
  node.subnodes.add other

func len(node: OrgNode): int = node.subnodes.len

func getStr(node: OrgNode): string =
  if node.kind in orgTokenKinds:
    $EndOfFile
  else:
    node.str

func charLen(node: OrgNode): int = node.text.len
func `[]`(node: var OrgNode, idx: BackwardsIndex): var OrgNode =
  node.subnodes[idx]

func `[]`(node: OrgNode, idx: BackwardsIndex): OrgNode =
  node.subnodes[idx]

func `[]`(node: var OrgNode, idx: int): var OrgNode =
  node.subnodes[idx]

func `[]`(node: OrgNode, idx: int): OrgNode =
  node.subnodes[idx]

func `[]`(node: OrgNode, name: string): OrgNode =
  node[getNamedSubnode(node.kind, name)]

func `[]`(node: var OrgNode, name: var string): var OrgNode =
  node[getNamedSubnode(node.kind, name)]


{.pop.}

template atom(lexer; idx: int; c: char): bool =
  ## Test if curent lexer character is equal to char `c`
  lexer[] == c

template atom(lexer; idx: int; s: set[char]): bool =
  ## Test of current lexer character is in set `s`
  lexer[] in s

template nxt(lexer; idx, step: int = 1) =
  ## Advance positions in lexer by `step` steps
  for i in 0 ..< step:
    lexer.advance()

proc hasNxt(lexer: Lexer; idx: int): bool =
  lexer.buf[lexer.bufpos] != EndOfFile

proc advance(lexer; chars: int = 1) =
  ## Advance lexer `chars` points forward. To explicitly move to next line
  ## use `nextLine()` as newlines are not skipped when encountered - this
  ## is made to allow handling of various optional 'until-EOL' constructs
  ## such as coommand arguments etc.
  ##
  ## Column and line numbers are updated when scanning over newline
  ## character - e.g. when moving **from** `'\n'` to some other character
  ## column and line number will be changed.

  # Add advance optional, to make moving to new line an error (e.g. some
  # constructs are only allowed to be places on single line).


  for _ in 0 ..< chars:
    case lexer[]:
      of '\n':
        lexer.bufpos = lexer.handleLF(lexer.bufpos)
        lexer.column = 0

      of '\r':
        lexer.bufpos = lexer.handleCR(lexer.bufpos)

      else:
        inc lexer.bufpos
        inc lexer.column


proc pop(lexer): char {.inline.} =
  result = lexer[]
  lexer.advance()


proc getSkipWhile(lexer; chars: set[char]): PosText =
  var slice = lexer.bufpos .. lexer.bufpos

  while lexer.buf[slice.b] in chars:
    inc slice.b

  dec slice.b

  result = PosText(
    line: lexer.lineNumber,
    column: lexer.getColNumber(lexer.bufpos),
    text: lexer.buf[slice])

  lexer.advance(result.len)

proc getBlockUntil(lexer; str: string, leftMargin: int = 0): PosText =
  let
    line = lexer.lineNumber
    column = lexer.column

  var buf: string


  block mainSearch:
    while true:
      while lexer[] != str[0]:
        buf.add lexer[]

        lexer.advance()

      if lexer[str]:
        break mainSearch



  return initPosText(buf, line, column)

proc error(lexer; message: string, annotation: string = ""): CodeError =
  toCodeError(
    lexer.buf,
    message = message,
    exprLen = 5,
    offset = lexer.offsetBase + lexer.bufpos,
    annotation = annotation
  )

proc skip(lexer; chars: set[char] = Whitespace) =
  while lexer[] in chars:
    lexer.advance()

proc getSkipUntil(lexer; chars: set[char]): PosText =
  result = lexer.getSkipWhile(AllChars - chars)

proc getSkipWhileTo(lexer; chars: set[char], to: char): PosText =
  result = getSkipWhile(lexer, chars)
  if lexer[] != to:
    discard lexer.error(&"Expected '{to}', but found '{lexer[]}'")


proc next(lexer) =
  lexer.curr.kind = otkNone

  case lexer[]:
    of '#':
      if lexer["#+begin"]:
        lexer.advance 2
        lexer.curr.kind = otkBeginCommand
        lexer.curr.text = lexer.getSkipWhileTo(identChars, ':')
        lexer.skip({':'})

      elif lexer["#+"]:
        lexer.advance 2
        lexer.curr.kind = otkCommand
        lexer.curr.text = lexer.getSkipWhileTo(identChars, ':')
        lexer.skip({':'})

    of '*':
      if lexer.column == 0:
        lexer.curr.kind = otkSubtreeStart

      else:
        lexer.curr.kind = otkListStart
        lexer.curr.text = lexer.getSkipWhileTo({'*'}, ' ')
        lexer.skip({' '})

    else:
      raiseAssert(&"#[ IMPLEMENT on char {[lexer[]]} ]#")

func toLower(c: char): char {.inline.} = toLowerAscii(c)

func charEq(c1, c2: char, caseInsensetive: bool = true): bool {.inline.} =
  if c1 == c2:
    true

  elif caseInsensetive and toLower(c1) == toLower(c2):
    true

  else:
    false

func `[]`(text: PosText, pos: int): char {.inline.} = text.text[pos]
func `[]`(text: PosText, slice: HSlice[int, BackwardsIndex]): PosText {.inline.} =
  PosText(
    text: text.text[slice],
    column: text.column + slice.a,
    line: text.line
  )

proc stripToken(
    tok: OrgToken,
    pref: string,
    skipChars: set[char] = {},
    caseInsensentive: bool = true
  ): OrgToken =

  result = tok

  var idx = 0
  var prefPos = 0

  while true:
    if idx < pref.len and
       charEq(result.text[idx], pref[prefPos], caseInsensentive):
      inc prefPos
      inc idx

    elif result.text[idx] in skipChars:
      inc idx

    else:
      break


  result.text = result.text[idx .. ^1]




{.push inline.}

proc newOrgIdent(text: PosText): OrgNode = OrgNode(kind: onkIdent, text: text)
proc newOrgIdent(token: OrgToken): OrgNode = OrgNode(kind: onkIdent, text: token.text)
proc newBareIdent(text: PosText): OrgNode = OrgNode(kind: onkBareIdent, text: text)
proc newTree(kind: OrgNodeKind, text: PosText): OrgNode =
  result = OrgNode(kind: kind)
  result.text = text

{.pop.}

proc newTree(kind: OrgNodeKind, subnodes: varargs[OrgNode]): OrgNode =
  result = OrgNode(kind: kind)
  for node in subnodes:
    result.subnodes.add node

proc newTree(
    kind: OrgNodeKind, str: string, subnodes: varargs[OrgNode]
  ): OrgNode {.inline.} =

  result = newTree(kind, subnodes)
  result.str = str






proc parseBareIdent(lexer): OrgNode =
  lexer.skip()
  result = newBareIdent(getSkipWhile(lexer, bareIdentChars))

proc newPosText(lexer): PosText =
  PosText(line: lexer.lineNumber, column: lexer.column)

proc getSkipToEOL(lexer): PosText =
  if lexer[] in Newlines:
    lexer.newPosText()

  else:
    lexer.getSkipUntil(Newlines)



proc parseCommandArgs(lexer): OrgNode =
  result = onkRawText.newTree(lexer.getSkipToEOL())

proc nextLine(lexer) =
  ## Move lexer position to the start of new line. Update column, line
  ## number and other fields accordingly.

  if lexer[] in Newlines:
    lexer.bufpos = lexer.handleLF(lexer.bufpos)
    lexer.column = 0

  else:
    while lexer[] notin Newlines + {EndOfFile}:
      lexer.advance()

    lexer.bufpos = lexer.handleLF(lexer.bufpos)
    # lexer.advance()
    lexer.column = 0

proc parseMultilineCommand(lexer): OrgNode =
  result = OrgNode(kind: onkMultilineCommand)
  result.add newOrgIdent(lexer.curr.stripToken("begin", {'_', '-'}))
  result.add parseBareIdent(lexer)
  result.add parseCommandArgs(lexer)
  lexer.nextLine()

  result.add onkCodeMultilineBlock.newTree(
    lexer.getBlockUntil("#+end"))

  lexer.nextLine()


proc newEmptyNode(): OrgNode =
  OrgNode(kind: onkEmptyNode)

proc parseCommand(lexer): OrgNode =
  result = OrgNode(kind: onkCommand)
  result.add newOrgIdent(lexer.curr)
  result.add parseCommandArgs(lexer)
  lexer.nextLine()

proc optGetWhile(lexer; chars: set[char], resKind: OrgNodeKind): OrgNode =
  if lexer[] in chars:
    result = newTree(resKind, lexer.getSkipWhile(chars))

  else:
    result = newEmptyNode()

proc getInsideSimple(lexer; delimiters: (char, char)): PosText =
  lexer.advance()
  return lexer.getSkipUntil({delimiters[1]})
const EmptyChars = Whitespace + {EndOfFile}

proc parseInlineMath(lexer): OrgNode =
  discard

proc parseBracket(lexer): OrgNode =
  ## Parse any square bracket entry starting at current lexer position, and
  ## return it.
  if lexer[0..1] == "[[":
    # Link start
    discard

  elif lexer[] == '[':
    # Inactive timestamp
    discard

proc parseAtEntry(lexer): OrgNode =
  ## Parse any entry starting with `@` sign - metatags, annotations, inline
  ## backend passes.
  if lexer[0..1] == "@@":
    # Inline backend pass
    discard

  elif lexer[0..1] == "@[":
    # Annotation start
    discard

  elif lexer[] == '@' and lexer[+1] in identChars:
    # Metatag start OR random `@` in the text
    discard

  else:
    raise lexer.error("Expected @-entry")


proc startNew(lexer; buffer: var PosText) =
  buffer.text = ""
  buffer.line = lexer.line
  buffer.column = lexer.column

proc getLastLevel(node: var OrgNode, level: int): var OrgNode =
  case level:
    of 0: return node
    of 1: return node[^1]
    of 2: return node[^1][^1]
    of 3: return node[^1][^1][^1]
    else: return getLastLevel(node, level - 4)

proc getLastLevel(node: OrgNode, level: int): OrgNode =
  case level:
    of 0: node
    of 1: node[^1]
    else: getLastLevel(node, level - 2)


proc parseText(lexer): seq[OrgNode] =
  # TODO implement support for additional formatting options, delimited
  # pairs, and punctuation. `<placeholder>`, `(structured-punctuation)`.
  var stack: seq[seq[tuple[pending: bool,
                           node: OrgNode]]]

  template closeWith(ch: string): untyped =
    # Close last pending node in stack is there is any, otherwise move
    # current layer not lower one.

    # IMPLEMENT handling of missing node pairs should happen here - close
    # request should be performed unconditionally (e.g. at the end of text
    # parsing all elements are closed), but some blocks might be missing
    # nodes. In this case markup delimiter should be pasted as regular word
    # (and warning should be emitted).
    let layer = stack.pop
    if (stack.last.len > 0 and stack.last2.pending):
      stack.last2.pending = false
      stack.last2.node.add layer.mapIt(it.node)

    else:
      stack.last.add layer

  template pushWith(newPending: bool, node: OrgNode): untyped =
    if (stack.last.len > 0 and stack.last2.pending):
      stack.add @[@[(newPending, node)]]

    else:
      stack.last.add (newPending, node)

  template pushBuf(): untyped =
    if buf.len > 0:
      pushWith(false, onkWord.newTree(buf))
      lexer.startNew(buf)


  stack.add @[]

  var buf: PosText
  lexer.startNew(buf)


  while lexer[] != EndOfFile:
    case lexer[]:
      of {'*', '_', '/', '~', '`', '+', '='} + {'\'', '"'}:
        let ch = lexer[]

        if lexer[-1] in EmptyChars and lexer[+1] != ch:
          # Start of the regular, constrained markup section.
          # Unconditinally push new layer.
          pushBuf()
          pushWith(true, onkMarkup.newTree($ch))

        elif lexer[+1] in EmptyChars:
          # End of regular constrained section, unconditionally close
          # current layer, possibly with warnings for things like
          # `*/not-fully-italic*`
          pushBuf()
          closeWith($ch)


        elif lexer[+1] == ch:
          # Detected unconstrained formatting block, will handle it
          # regardless.
          let ch = $ch & $ch
          pushBuf()

          # If it matches underlying element in text, close it, otherwise
          # push new layers. Pushing new layers indefinitely is safe,
          # because everything will be closed on main function return
          # (possibly with warnings though)
          if stack.len > 1 and
             stack[^2][^1].pending and
             stack[^2][^1].node.getStr() == ch:
            # Close unconstrainted block
            closeWith(ch)

          else:
            # Open unconstrained block
            pushWith(true, onkMarkup.newTree(ch))

          lexer.advance()

        else:
          buf.add lexer.pop



        lexer.advance()

      of '$':
        if lexer[-1] in EmptyChars:
          discard parseInlineMath(lexer)

        else:
          raiseAssert("#[ IMPLEMENT ]#")

      of '@':
        if lexer[-1] in EmptyChars:
          discard parseAtEntry(lexer)

        else:
          raiseAssert("#[ IMPLEMENT ]#")

      else:
        buf.add lexer.pop

  while stack.len > 1:
    closeWith("")

  return stack[0].mapIt(it.node)



proc parseParagraph(lexer): OrgNode =
  result = onkParagraph.newTree(lexer.parseText())


proc parseOrgCookie(lexer): OrgNode =
  if lexer[] == '[':
    result = onkUrgencyStatus.newTree(getInsideSimple(lexer, ('[', ']'))[1..^1])
    lexer.advance()

  else:
    result = newEmptyNode()


proc skipPositional(lexer; skip: set[char]): Position =
  while lexer[] in skip:
    inc result.offset
    if lexer[] in Newlines:
      inc result.line
      result.column = 0

    else:
      inc result.column

    lexer.advance()

proc skipIndentGeq(lexer; indent: int): Position =
  if lexer[] in Newlines:
    inc result.line
    inc result.offset

    lexer.advance()

  var idx = 0
  while true:
    if lexer[] in Whitespace:
      inc result.offset
      inc result.column
      lexer.advance()

    else:
      if idx < indent:
        lexer.error(
          &"Incorrect indentation - expected at least {indent}, but found {idx}",
          "Add leading whitespaces"
        )

      else:
        break

    inc idx

proc getIndent(lexer): int =
  assert (lexer[-1] in Newlines or lexer.bufpos == 0):
    fmtJoin:
      "Indent test must be performed at the start of the line or buffer,"
      "but lexer char is '{lexer[-1]}' (bufpos: {lexer.bufpos}, +/-2 around:"
      "{toSeq(lexer[-2 .. 2])})"

  var ind = 0
  for ch in lexer[0 .. ^1]:
    if ch in Whitespace:
      inc ind

    else:
      break

  return ind



proc newSublexer(pos: Position, str: string, increments: PosIncrements): Lexer =
  ## Create new lexer using string stream `str`, with global positioning
  ## from `pos`. Text block positionsing generated from lexer would be
  ## correct, assuming `pos` was initally set right.
  open(result, newStringStream(str))
  result.line = pos.line
  result.column = pos.column
  result.offsetBase = pos.offset

proc newSublexer(pos: Position, pair: (string, PosIncrements)): Lexer {.inline.} =
  newSublexer(pos, pair[0], pair[1])

proc getPosition(lexer): Position =
  Position(
    line: lexer.line,
    column: lexer.column,
    offset: lexer.offsetBase
  )

template lexScanp(lexer; pattern: varargs[untyped]): bool =
  var idx: int = 0
  scanp(lexer, idx, pattern)

proc pop(str: var string): char {.discardable, inline.} =
  result = str[str.high]
  str.setLen(str.high)

proc cutIndentedBlock(
    lexer; indent: int,
    keepNewlines: bool = true,
    requireContinuations: bool = false,
    fromInline: bool = true
  ): (string, PosIncrements) =
  ## - @arg{requireContinuation} - only continue extracting line that are
  ##   appended with `\` character
  ## - @arg{fromInline} - block cutout begins from somewhere inside
  ##   current line and all characters until EOF should be added to buffer.


  if fromInline:
    discard lexer.lexScanp(*(~{'\n'} -> result[0].add $_))


  if requireContinuations:
    while lexer[-1] == '\\':
      result[0].pop
      lexer.advance()

      if lexer.getIndent() >= indent:
        result[1][result[0].high] = lexer.skipIndentGeq(indent)
        discard lexer.lexScanp(*(~{'\n'} -> result[0].add $_))

      else:
        echov "Not enough indent"
        break

  else:
    while true:
      let ind = lexer.getIndent()
      if ind >= indent:
        lexer.advance(indent)
        result[1][result[0].high] = lexer.skipIndentGeq(indent)
        result[0].add lexer.getSkipUntil({'\n'}).text
        if keepNewlines:
          result[0].add '\n'

        lexer.advance()

      else:
        break

  lexer.advance()

proc findOnLine(lexer; target: string): int =
  var lexIdx = 0
  while true:
    if lexer[lexIdx] in Newlines:
      return -1

    elif lexer[lexIdx] == target[0]:
      block tryMatchTarget:
        for idx, targetChar in target:
          if lexer[lexIdx + idx] != target[idx]:
            break tryMatchTarget

        return lexIdx

    else:
      # TODO come up with proper handling of edge cases. This should
      # suffice for testing, but this is nowhere near actual solution.
      if lexIdx > 100:
        break

    inc lexIdx


proc lineStartsWith(lexer; str: string): bool = lexer.findOnLine(str) != -1
proc indentTo(lexer; str: string): int =
  assert lexer[-1] in Newlines,
     "Test for indentation to must be performed only on line starts"

  lexer.findOnLine(str)

proc pop(text: var PosText): char {.discardable, inline.} =
  result = text.text[text.text.high]
  text.text.setLen(text.text.high)


proc parseDrawer(lexer): OrgNode =
  result = onkDrawer.newTree()

  result.add onkIdent.newTree(lexer.getInsideSimple((':', ':')))
  lexer.advance(2)

  while true:
    if lexer[0 .. 4] == ":end:":
      discard lexer.getSkipToEOL()
      lexer.advance()
      echov lexer @? 0 .. 5
      return

    elif lexer[] == ':':
      result.add onkProperty.newTree(
        onkIdent.newTree(lexer.getInsideSimple((':', ':'))),
        (lexer.advance();
          onkRawText.newTree(lexer.getSkipToEOL())))

      lexer.advance()

    else:
      var buf: PosText
      lexer.startNew(buf)
      while true:
        if lexer[] in {':', EndOfFile} and lexer[-1] in {'\n'}:
          discard lexer.getSkipToEOL()
          lexer.advance()
          buf.pop()
          result.add onkRawText.newTree(buf)
          return

        else:
          buf.add lexer[]
          lexer.advance()




proc parseDrawers(lexer): OrgNode =
  ## Parse one or mode drawers starting on current line.
  if lexer.lineStartsWith(":"):
    var drawerLexer = newSublexer(
      lexer.getPosition(),
      lexer.cutIndentedBlock(lexer.indentTo(":"), fromInline = false)
    )

    result = onkStmtList.newTree()

    while drawerLexer[] == ':':
      result.add parseDrawer(drawerLexer)


  else:
    return newEmptyNode()

proc parseSubtree(lexer): OrgNode =
  result = OrgNode(kind: onkSubtree)

  result.add onkBareIdent.newTree(lexer.getSkipWhile({'*'}))
  lexer.skip()

  result.add lexer.optGetWhile(bigIdentChars, onkBigIdent)
  lexer.skip()

  result.add parseOrgCookie(lexer)

  lexer.skip()

  var headerLexer = newSublexer(
    lexer.getPosition(),
    lexer.cutIndentedBlock(
      result["prefix"].charLen(),
      keepNewlines = false,
      requireContinuations = true
    )
  )

  result.add parseParagraph(headerLexer)

  # IMPLEMENT instead of cutting whole header string into sublexer, first
  # check for subtree completion status and tags, and then parse things.
  result.add newEmptyNode()
  result.add newEmptyNode()

  result.add parseDrawers(lexer)


proc parseStmtList(lexer): OrgNode =
  result = OrgNode(kind: onkStmtList)
  lexer.next()

  while lexer[] != EndOfFile:
    try:
      case lexer.curr.kind:
        of otkBeginCommand:
          result.add parseMultilineCommand(lexer)

        of otkCommand:
          result.add parseCommand(lexer)

        of otkSubtreeStart:
          result.add parseSubtree(lexer)

          echo result.treeRepr()

          quit 0

        else:
          raiseAssert(&"#[ IMPLEMENT for kind {lexer.curr.kind} {instantiationInfo()} ]#")

      lexer.skip(Newlines + Whitespace)
      lexer.next()

    except:
      echo result.treeRepr()
      raise


  # echo result.treeRepr()


proc parseOrg*(str: string): OrgNode =
  startHax()
  let sstream = newStringStream(str)
  var lexer: Lexer
  open(lexer, sstream)

  parseStmtList(lexer)
