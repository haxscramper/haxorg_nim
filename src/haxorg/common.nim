import std/[parseutils, strutils, sugar]

export Whitespace

type
  Position* = object
    ## Some position in document
    line*: int ## Position line
    column*: int ## Position column
    offset*: int ## Offset from absolute document start

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
  OWhitespace* = Whitespace - {'\n'}
  OEmptyChars* = OWhitespace + {OEndOfFile}
  OLinebreaks* = Newlines + {OEndOfFile}
  OMarkupChars* = {'*', '_', '/', '+', '~', '`'}
  OVerbatimChars* = {'`', '~', '='}
  OPunctChars* = {'(', ')', '[', ']', '.', '?', '!', ','}
  OPunctOpenChars* = {'(', '[', '{', '<'}
  OPunctCloseChars* = {')', ']', '}', '>'}
  ONumberedListChars* = {'0' .. '9'} + {'a' .. 'z'} + {'A' .. 'Z'}
  OBulletListChars* = {'-', '+', '*'}
  OListChars* = ONumberedListChars + OBulletListChars




{.push inline.}

func matchingPair*(ch: char): char =
  case ch:
    of '[': ']'
    of '(': ')'
    of '{': '}'
    of '<': '>'
    of ']': '['
    of ')': '('
    of '}': '{'
    of '>': '<'
    of OMarkupChars + {'"', '\''}: ch
    else: raiseAssert("No closing pair for '" & $ch & "' character")

# func initPosText*(text: string, line, column: int): PosText =
#   PosText(text: text, line: line, column: column)

# func initPosText*(text: string, pos: Position): PosText =
#   PosText(text: text, line: pos.line, column: pos.column)

# func len*(text: PosText): int = text.text.len
# func add*(text: var PosText, ch: char) = text.text.add ch
# func high*(text: PosText): int = text.text.len - 1


# func `[]`*(text: PosText, pos: int): char =
#   text.text[pos]

# func `[]`*(text: PosText, slice: HSlice[int, BackwardsIndex]): PosText =
#   PosText(
#     text: text.text[slice],
#     column: text.column + slice.a,
#     line: text.line
#   )

# func `[]`*(text: PosText, slice: Slice[int]): PosText  =
#   PosText(
#     text: text.text[slice],
#     column: text.column + slice.a,
#     line: text.line
#   )

func toLower(c: char): char {.inline.} = toLowerAscii(c)

func charEq*(c1, c2: char, caseInsensetive: bool = true): bool {.inline.} =
  if c1 == c2:
    true

  elif caseInsensetive and toLower(c1) == toLower(c2):
    true

  else:
    false

template rfindItNeg*(s: typed, op: untyped): int =
  var result = 0
  var found = false
  for idx in countdown(s.len - 1, 0):
    inc result
    let it {.inject.} = s[idx]
    if op:
      found = true
      break


  if found:
    result

  else:
    -1


proc rfindNeg*[T](s: openarray[T], item: T): int =
  return rfindItNeg(s, it == item)

template rfindIt*(s: typed, op: untyped): int =
  var result = -1
  for idx in 0 .. s.high:
    let it {.inject.} = s[idx]
    if op:
      result = idx
      break

  result


proc rfind*[T](s: openarray[T], item: T): int =
  return rfindIt(s, it == item)

template popUntilIt*(s: typed, op: untyped, inclusive: bool = true): untyped =
  var pos =  s.rfindItNeg(op)
  var result = false
  if pos > 0:
    if not inclusive:
      dec pos

    for _ in 0 ..< pos:
      discard s.pop()
      result = true

  result

proc popUntil*[T](
  s: var seq[T], item: T, inclusive: bool = true): bool {.discardable.} =
  return popUntilIt(s, it == item, inclusive = inclusive)


block:
  var shit1 = @[1,2,3,4]
  discard shit1.popUntilIt(it == 2) # discard this degenerate vomit
  assert shit1 == @[1]

assert [1,2,3,4].rfindIt(it == 3) == 2

assert @[1,2,3].dup(popUntil(2)) == @[1]
assert @[1,2,3].dup(popUntil(2, false)) == @[1, 2]
assert [1,2,3].rfindNeg(3) == 1
assert [1,2,3].rfindNeg(4) == -1
assert [1,2,3].rfind(3) == 2
