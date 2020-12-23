import std/[streams, strutils, parseutils,
            macros, strformat, sequtils]

import std/lexbase except Newlines

import hmisc/hexceptions
import hmisc/hdebug_misc
import hmisc/other/colorlogger
import hmisc/types/colorstring
import hpprint, hpprint/hpprint_repr

type
  OrgTokenKind* = enum
    otkNone

    otkCommand
    otkBeginCommand
    otkIdent

  OrgNodeKind* = enum
    onkNone

    onkDocument
    onkCommandArgs
    onkCommand
    onkMultilineCommand
    onkIdent
    onkBareIdent
    onkCodeMultilineBlock

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
  onkCodeMultilineBlock
}

const
  identChars = {'a' .. 'z', 'A' .. 'Z', '_', '-'}
  bareIdentChars = AllChars - Whitespace

type
  OrgNodeObj* = object
    case kind*: OrgNodeKind
      of orgTokenKinds:
        text*: PosText

      else:
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

    of orgTokenKinds - {onkIdent}:
      let txt = toYellow(node.text.text)
      if '\n' in node.text.text:
        return pptConst(&"{node.kind}\n\"\"\"\n{txt}\n\"\"\"")

      else:
        return pptConst(&"{node.kind} \"{txt}\"")

    else:
      return pptObj(
        $node.kind,
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

proc `[]`(lexer; str: string): bool =
  result = lexer.buf[lexer.bufpos ..< lexer.bufpos + str.len] == str

proc `@?`(lexer; slice: Slice[int]): seq[char] = @(lexer[slice])

proc advance(lexer; chars: int = 1): bool =
  for _ in 0 ..< chars:
    inc lexer.bufpos

    case lexer[]:
      of '\n':
        lexer.bufpos = lexer.handleLF(lexer.bufpos)
        dec lexer.bufpos

      of '\r':
        lexer.bufpos = lexer.handleCR(lexer.bufpos)

      else:
        inc lexer.column


func initPosText(text: string, line, column: int): PosText =
  PosText(text: text, line: line, column: column)

proc len(text: PosText): int = text.text.len

proc add(node: var OrgNode, other: OrgNode) = node.subnodes.add other

{.pop.}

proc getSkipWhile(lexer; chars: set[char]): PosText =
  var slice = lexer.bufpos .. lexer.bufpos

  while lexer.buf[slice.b] in chars:
    inc slice.b

  dec slice.b

  result = PosText(
    line: lexer.lineNumber,
    column: lexer.getColNumber(lexer.bufpos),
    text: lexer.buf[slice])

  discard lexer.advance(result.len)

proc getBlockUntil(lexer; str: string, leftMargin: int = 0): PosText =
  let
    line = lexer.lineNumber
    column = lexer.column

  var buf: string


  block mainSearch:
    while true:
      while lexer[] != str[0]:
        buf.add lexer[]

        discard lexer.advance()

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
    discard lexer.advance()

proc getSkipUntil(lexer; chars: set[char]): PosText =
  result = lexer.getSkipWhile(AllChars - chars)

proc getSkipWhileTo(lexer; chars: set[char], to: char): PosText =
  result = getSkipWhile(lexer, chars)
  if lexer[] != to:
    discard lexer.error(&"Expected '{to}', but found '{lexer[]}'")


proc next(lexer) =
  if lexer.column == 0:
    case lexer[]:
      of '#':
        if lexer["#+begin"]:
          discard lexer.advance 2
          lexer.curr.kind = otkBeginCommand
          lexer.curr.text = lexer.getSkipWhileTo(identChars, ':')
          lexer.skip({':'})

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
      discard lexer.advance()

    lexer.bufpos = lexer.handleLF(lexer.bufpos)
    # discard lexer.advance()
    lexer.column = 0

proc parseMultilineCommand(lexer): OrgNode =
  debug "parseMultilineCommand"

  result = OrgNode(kind: onkMultilineCommand)
  result.add newOrgIdent(lexer.curr.stripToken("begin", {'_', '-'}))
  result.add parseBareIdent(lexer)
  result.add parseCommandArgs(lexer)
  echov lexer @? 0 .. 4
  lexer.nextLine()
  echov lexer @? 0 .. 4


  result.add onkCodeMultilineBlock.newTree(
    lexer.getBlockUntil("#+end"))

  debug result.treeRepr()

  lexer.nextLine()




proc parseCommand(lexer): OrgNode =
  result = OrgNode(kind: onkCommand)
  result.add newOrgIdent(lexer.curr)
  result.add parseCommandArgs(lexer)
  lexer.nextLine()

proc parseDocument(lexer): OrgNode =
  result = OrgNode(kind: onkDocument)
  lexer.next()

  debug lexer[0 .. 3]

  while lexer[] != EndOfFile:
    try:
      info "Current lexer:", lexer.curr.kind
      debug lexer @? 0 .. 10
      logIndented:
        case lexer.curr.kind:
          of otkBeginCommand:
            result.add parseMultilineCommand(lexer)

          of otkCommand:
            result.add parseCommand(lexer)

          else:
            raiseAssert(&"#[ IMPLEMENT for kind {lexer.curr.kind} {instantiationInfo()} ]#")

        lexer.next()

    except:
      echo result.treeRepr()
      raise


  echo result.treeRepr()


proc parseOrg*(str: string): OrgNode =
  startHax()
  startColorLogger()
  let sstream = newStringStream(str)
  var lexer: Lexer
  open(lexer, sstream)

  parseDocument(lexer)
