import std/[parseutils, strutils]

export Whitespace

type
  Position* = object
    ## Some position in document
    line*: int ## Position line
    column*: int ## Position column
    offset*: int ## Offset from absolute document start

  PosText* = object
    line*: int
    column*: int
    text*: string


const
  OIdentChars* = {'a' .. 'z', 'A' .. 'Z', '_', '-', '0' .. '9'}
  OIdentStartChars* = IdentChars - {'_', '-', '0' .. '9'}

  OWordChars* = {'a' .. 'z', 'A' .. 'Z', '0' .. '9', '\x7F' .. '\xFF'}
  # IDEA in figure some additional unicode handing might be performed, but
  # for now I just asume text is in UTF-8 and everything above 127 is a
  # unicode rune too.


  OBigIdentChars* = {'A' .. 'Z'}
  OEndOfFile* = '\x00'
  OBareIdentChars* = AllChars - Whitespace
  OEmptyChars* = Whitespace + {OEndOfFile}
  OLinebreaks* = Newlines + {OEndOfFile}

{.push inline.}

func initPosText*(text: string, line, column: int): PosText =
  PosText(text: text, line: line, column: column)

func initPosText*(text: string, pos: Position): PosText =
  PosText(text: text, line: pos.line, column: pos.column)

func len*(text: PosText): int = text.text.len
func add*(text: var PosText, ch: char) = text.text.add ch
func high*(text: PosText): int = text.text.len - 1


func `[]`*(text: PosText, pos: int): char =
  text.text[pos]

func `[]`*(text: PosText, slice: HSlice[int, BackwardsIndex]): PosText =
  PosText(
    text: text.text[slice],
    column: text.column + slice.a,
    line: text.line
  )

func `[]`*(text: PosText, slice: Slice[int]): PosText  =
  PosText(
    text: text.text[slice],
    column: text.column + slice.a,
    line: text.line
  )

func toLower(c: char): char {.inline.} = toLowerAscii(c)

func charEq(c1, c2: char, caseInsensetive: bool = true): bool {.inline.} =
  if c1 == c2:
    true

  elif caseInsensetive and toLower(c1) == toLower(c2):
    true

  else:
    false


proc pop*(text: var PosText): char {.discardable, inline.} =
  result = text.text[text.text.high]
  text.text.setLen(text.text.high)

func `==`*(text: PosText, str: string): bool = text.text == str

{.pop.}

proc stripPosText*(
    text: PosText,
    pref: string,
    skipChars: set[char] = {},
    caseInsensentive: bool = true
  ): PosText =

  result = text

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
