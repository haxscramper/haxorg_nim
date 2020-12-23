import std/[streams, strutils, parseutils,
            macros, strformat, sequtils]

import std/lexbase except Newlines

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

    onkDocument ## Toplevel document
    onkSubtree ## Section subtree
    onkList ## List

    onkCommandArgs ## Arguments to single or multiline commands
    onkCommand ## Single-line command
    onkMultilineCommand ## Multiline command
    onkIdent ## regular identifier - `alnum + [-_]` characters for punctuation
    onkBareIdent ## Bare identifier - any characters are allowed
    onkBigIdent ## full-uppsercase identifier such as `MUST` or `TODO`
    onkCodeMultilineBlock ## Verbatim mulitiline block
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

    onkWord ## Regular word - technically not different from `onkIdent`,
    ## but defined separately to disiguish between places where special
    ## syntax is required and free-form text.

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
  onkCommandArgs,
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

  Lexer* = object of BaseLexer
    toggles: seq[MarkupToggle]
    currIndent: int
    column: int
    curr: OrgToken

iterator items*(node: OrgNode): OrgNode =
  for n in node.subnodes:
    yield n

proc toString(x: OrgNodeKind): string {.magic: "EnumToStr", noSideEffect.}

proc `$`(onk: OrgNodeKind): string {.inline.} = toString(onk)[3 ..^ 1]


func objTreeRepr*(node: OrgNode): ObjTree =
  if node.isNil:
    return pptConst("<nil>", initStyle(fgBlue))

  case node.kind:
    of onkIdent:
      return pptConst(&"{node.kind} {toGreen(node.text.text)}")


    of orgTokenKinds - {onkIdent, onkMarkup}:
      let txt = toYellow(node.text.text)
      if '\n' in node.text.text:
        return pptConst(&"{node.kind}\n\"\"\"\n{txt}\n\"\"\"")

      else:
        return pptConst(&"{node.kind} \"{txt}\"")

    else:
      let mark = tern(node.str.len > 0, &" <{toBlue(node.str)}>", "")
      return pptObj(
        &"{node.kind}{mark}",
        initStyle(),
        node.mapIt(it.objTreeRepr())
      )


func treeRepr*(node: OrgNode): string =
  node.objTreeRepr().treeRepr()

using lexer: var Lexer

{.push inline.}


proc `[]`(lexer): char =
  lexer.buf[lexer.bufpos]

proc `[]`(lexer; idx: int): char =
  lexer.buf[lexer.bufpos + idx]

proc `[]`(lexer; slice: Slice[int]): string =
  lexer.buf[lexer.bufpos + slice.a .. lexer.bufpos + slice.b]

proc `@?`(lexer; slice: Slice[int]): seq[char] = @(lexer[slice])

proc `[]`(lexer; str: string): bool =
  result = lexer.buf[lexer.bufpos ..< lexer.bufpos + str.len] == str

func initPosText(text: string, line, column: int): PosText =
  PosText(text: text, line: line, column: column)

func len(text: PosText): int = text.text.len

func add(node: var OrgNode, other: OrgNode) =
  node.subnodes.add other

func len(node: OrgNode): int = node.subnodes.len

func `[]`(node: var OrgNode, idx: BackwardsIndex): var OrgNode =
  return node.subnodes[idx]

{.pop.}


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
  result = onkCommandArgs.newTree(lexer.getSkipToEOL())

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


proc parseText(lexer): OrgNode =
  result = onkParagraph.newTree()
  case lexer[-1 .. 0]:
    of " *":
      result.add onkMarkup.newTree("*")
      lexer.advance()

      result.subnodes[^1].add onkWord.newTree(lexer.getSkipUntil({'*'}))
      lexer.advance()


proc parseOrgCookie(lexer): OrgNode =
  if lexer[] == '[':
    result = onkUrgencyStatus.newTree(getInsideSimple(lexer, ('[', ']')))
    lexer.advance()

  else:
    result = newEmptyNode()





proc parseSubtree(lexer): OrgNode =
  result = OrgNode(kind: onkSubtree)

  result.add onkBareIdent.newTree(lexer.getSkipWhile({'*'}))
  lexer.skip()

  result.add lexer.optGetWhile(bigIdentChars, onkBigIdent)
  lexer.skip()

  result.add parseOrgCookie(lexer)

  lexer.skip()
  result.add parseText(lexer)


proc parseDocument(lexer): OrgNode =
  result = OrgNode(kind: onkDocument)
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


  echo result.treeRepr()


proc parseOrg*(str: string): OrgNode =
  startHax()
  let sstream = newStringStream(str)
  var lexer: Lexer
  open(lexer, sstream)

  parseDocument(lexer)
