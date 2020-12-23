import std/[lexbase, streams, strutils, parseutils,
            macros, strformat, sequtils]

import hmisc/hexceptions
import hmisc/types/colorstring
import hpprint, hpprint/hpprint_repr

type
  OrgTokenKind* = enum
    otkCommand
    otkBeginCommand
    otkIdent

  OrgNodeKind* = enum
    onkDocument
    onkCommandArgs
    onkMultilineCommand
    onkIdent

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
  onkCommandArgs
}

const
  identChars = {'a' .. 'z', 'A' .. 'Z', '_', '-'}

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
      return pptConst(&"{node.kind} \"{node.text.text}\"")

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

proc `[]`(lexer; slice: Slice[int]): string =
  lexer.buf[lexer.bufpos + slice.a .. lexer.bufpos + slice.b]

proc `[]`(lexer; str: string): bool =
  result = lexer.buf[lexer.bufpos ..< lexer.bufpos + str.len] == str

proc advance(lexer; chars: int = 1): bool =
  inc lexer.bufpos, chars
  inc lexer.column, chars

proc len(text: PosText): int = text.text.len

proc add(node: var OrgNode, other: OrgNode) = node.subnodes.add other

{.pop.}

proc getSkipWhile(lexer; chars: set[char]): PosText =
  result = PosText(
    line: lexer.lineNumber,
    column: lexer.getColNumber(lexer.bufpos),
    text: lexer.buf[lexer.bufpos .. lexer.buf.skipWhile(chars, lexer.bufpos) + 1]
  )

  discard lexer.advance(result.len)

proc error(lexer; message: string, annotation: string = "") =
  let err = toCodeError(
    lexer.buf,
    message = message,
    exprLen = 5,
    offset = lexer.offsetBase + lexer.bufpos,
    annotation = annotation
  )

proc getSkipWhileTo(lexer; chars: set[char], to: char): PosText =
  result = getSkipWhile(lexer, chars)
  if lexer[] != to:
    lexer.error(&"Expected '{to}', but found '{lexer[]}'")


proc next(lexer) =
  if lexer.column == 0:
    case lexer[]:
      of '#':
        if lexer["#+begin"]:
          discard lexer.advance 2
          lexer.curr.kind = otkBeginCommand
          lexer.curr.text = lexer.getSkipWhileTo(identChars, ':')

      else:
        raiseAssert("#[ IMPLEMENT ]#")

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


  echo idx
  result.text = result.text[idx .. ^1]

  echo result





proc newOrgIdent(text: PosText): OrgNode =
  OrgNode(kind: onkIdent, text: text)

proc newOrgIdent(token: OrgToken): OrgNode =
  OrgNode(kind: onkIdent, text: token.text)

proc parseMultilineCommand(lexer): OrgNode =
  result = OrgNode(kind: onkMultilineCommand)
  result.add newOrgIdent(lexer.curr.stripToken("begin", {'_', '-'}))

proc parseDocument(lexer): OrgNode =
  result = OrgNode(kind: onkDocument)
  lexer.next()

  case lexer.curr.kind:
    of otkBeginCommand:
      result.add parseMultilineCommand(lexer)

    else:
      raiseAssert(&"#[ IMPLEMENT for kind {lexer.curr.kind} {instantiationInfo()} ]#")

  echo result.treeRepr()


proc parseOrg*(str: string): OrgNode =
  let sstream = newStringStream(str)
  var lexer: Lexer
  open(lexer, sstream)

  parseDocument(lexer)
