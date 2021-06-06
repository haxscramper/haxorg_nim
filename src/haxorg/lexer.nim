{.experimental: "dotOperators".}

import std/[tables, strutils, strformat, sequtils, streams, macros, sugar]
import common, buf
import hmisc/types/colorstring


import std/lexbase except Newlines
export open

import hmisc/hexceptions
import hmisc/algo/hstring_algo
import hmisc/hdebug_misc
import hmisc/helpers


type
  LexerImpl = ref object
    buf*: StrSlice
    bufpos*: int


  Lexer* = object
    # The only reason for splitting implementation into two parts is
    # overload for `[]` operator without parameters (for ref types it
    # cannot be overloaded)
    d*: LexerImpl

  PosIncrements* = Table[int, Position]

using lexer: var Lexer

proc `=copy`*(target: var Lexer, old: Lexer) =
  target.d = LexerImpl(
    buf: old.d.buf,
    bufpos: old.d.bufpos
  )


template bufpos(lexer): untyped = lexer.d.bufpos
template buf(lexer): untyped = lexer.d.buf
template `bufpos=`(lexer; val: int): untyped = lexer.d.bufpos = val

{.push inline.}

proc line*(lexer): int = lexer.buf.lineNumber(lexer.bufpos)
proc column*(lexer): int = lexer.buf.buf.colNumber(lexer.bufpos)

proc `[]`*(lexer): char = lexer.d.buf.absAt(lexer.bufpos)

proc `[]`*(lexer; idx: int): char =
  if idx + lexer.bufpos < 0:
    result = EndOfFile

  else:
    result = lexer.d.buf.absAt(lexer.buf.shift(lexer.bufpos, idx))

proc `[]`*(lexer; slice: Slice[int]): string =
  for idx in slice:
    result &= lexer[idx]
  # result = lexer.buf[
  #       lexer.bufpos + slice.a ..
  #   min(lexer.bufpos + slice.b, lexer.buf.high)
  # ]

proc `[]`*(lexer; slice: HSlice[int, BackwardsIndex]): string =
  lexer.buf[lexer.bufpos + slice.a .. lexer.buf.high]

proc `[]`*(lexer; pos: BackwardsIndex): char =
  lexer.buf[pos]

proc `@?`*(lexer; slice: Slice[int]): seq[char] = @(lexer[slice])

proc `[]`*(lexer; str: string): bool =
  if lexer[] == OEndOfFile:
    return false

  var bufpos = lexer.bufpos
  var strpos = 0
  for idx in indices(lexer.buf):
    if strpos > str.high:
      return true

    elif idx >= lexer.bufpos:
      if charEq(lexer.buf.absAt(idx), str[strpos]):
        inc strpos

      elif lexer.buf.absAt(idx) in {'-', '_'}:
        discard

      else:
        return false

proc toSlice*(ranges: StrRanges, lexer): StrSlice =
  initStrSlice(lexer.buf.buf, ranges)

{.pop.}

proc succ*(lexer): int = lexer.buf.succ(lexer.bufpos)

proc advance*(lexer; chars: int = 1) =
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

  # for _ in 0 ..< chars:
  lexer.bufpos = lexer.d.buf.shift(lexer.d.bufpos, chars)



iterator items*(lexer): char =
  while lexer[] != OEndOfFile:
    yield lexer[]
    lexer.advance()


proc pop*(lexer): int {.inline.} =
  result = lexer.bufpos
  lexer.advance()

proc expect*(lexer; chars: set[char]) =
  discard

func initStrRanges*(lexer): StrRanges =
  @[(lexer.bufpos, lexer.bufpos)]


func initEmptyStrRanges*(lexer): StrRanges = @[]

proc getSkipWhile*(lexer; chars: set[char]): StrRanges =
  result = lexer.initStrRanges()

  while lexer[] in chars - {EndOfFile}:
    result.add lexer.pop

proc getIndent*(lexer): int =
  assert (lexer[-1] in Newlines + {OEndOfFile}):
    fmtJoin:
      "Indent test must be performed at the start of the line or buffer,"
      "but lexer char is '{lexer[-1]}'."
      "\nbufpos: {lexer.bufpos}"
      "\n+/-3 around: {toSeq(lexer[-3 .. 3])})"
      "\n+/-3 around *in buffer*: {toSeq(lexer.buf.buf.str[-3 + lexer.bufpos .. 3 + lexer.bufpos])})"

  var ind = 0
  for ch in lexer[0 .. ^1]:
    if ch in Whitespace:
      inc ind

    else:
      break

  return ind




proc absAt*(lexer; idx: int): char =
  lexer.buf.buf[idx]

proc getBlockUntil*(
    lexer; str: string, leftMargin: int = 0,
    dedent: bool = true
  ): StrRanges =

  var ranges: seq[(int, int)] = lexer.initStrRanges()

  var
    inLine: bool = false
    prefLens: seq[int] = @[lexer.getIndent()]

  while not lexer[str] and (lexer[] != OEndOfFile):
    if inLine:
      if lexer[] in OWhitespace:
        inc prefLens[^1]

      else:
        inLine = false


    if lexer[] in OLineBreaks:
      inLine = true
      if lexer[-1] notin OLineBreaks:
        # Protection against empty rows
        prefLens.add 0

    ranges.add lexer.pop()


  if ranges.len == 1 and ranges[0][0] == ranges[0][1]:
    return @[]

  if dedent:
    discard prefLens.pop
    reverse(prefLens)

    var
      inPrefix = true
      prefLenStart = min(prefLens)
      prefLen = prefLenStart

    for idx in indices(ranges):
      if prefLen == 0 and
         lexer.absAt(idx) in OLineBreaks and
         lexer.absAt(idx + 1) in OLineBreaks:
        # Edge casing empty rows - they have no indentation, but newline
        # still has to be captured in resulting slice
        result.add idx

      elif inPrefix and prefLen > 0:
        dec prefLen
        if prefLen == 0:
          inPrefix = false

      else:
        if lexer.absAt(idx) in OLineBreaks:
          inPrefix = true
          prefLen = prefLenStart

        result.add idx

  else:
    return ranges



proc error*(lexer; message: string, annotation: string = ""): CodeError =
  result = toCodeError(
    lexer.buf.buf.str,
    message = message,
    exprLen = 5,
    offset = lexer.d.bufpos,
    annotation = annotation
  )

proc skip*(lexer; chars: set[char] = Whitespace): int {.discardable, inline.} =
  while lexer[] in chars:
    inc result
    lexer.advance()


proc skipExpected*(lexer; str: string) =
  for idx in 0 .. str.high:
    if lexer[] == str[idx]:
      lexer.advance()

    else:
      raise lexer.error(
        &"Expected '{str}', but found '{lexer[0 .. str.high]}'")

proc getSkipUntil*(lexer; chars: set[char]): StrRanges =
  result = lexer.getSkipWhile(AllChars - chars)

proc getSkipWhileTo*(lexer; chars: set[char], to: char): StrRanges =
  result = getSkipWhile(lexer, chars)
  if lexer[] != to:
    discard lexer.error(&"Expected '{to}', but found '{lexer[]}'")

proc getSkipToEOL*(lexer; withNl: bool = true): StrRanges =
  result = lexer.getSkipUntil(Newlines)
  if not withNl:
    dec result

proc nextLine*(lexer) =
  ## Move lexer position to the start of new line. Update column, line
  ## number and other fields accordingly.

  while lexer[] notin Newlines + {EndOfFile}:
    lexer.advance()

  lexer.advance()

proc gotoSOL*(lexer) =
  while lexer[] notin Newlines + {EndOfFile}:
    lexer.advance(-1)

  lexer.advance()

proc getInsideSimple*(lexer; del0, del1: char): StrRanges =
  ## Get text enclosed with `delimiters`. No special heuristics is used to
  ## determine balanced pairs, internal string literals etc. Text is cut
  ## from current position + 1 until first ocurrence of `delimiters[1]`. To
  ## get balanced pairs use `getInsideBalanced()`
  assert lexer[] == del0
  lexer.advance()
  result = lexer.getSkipUntil({del1})
  lexer.advance()

proc getInsideBalanced*(lexer; del0, del1: char): StrSlice =
  # - TODO handle escaped characters in form of `\delimiters[1]`
  var cnt: int = 0


  assert lexer[] == del0
  inc cnt

  result.buf = lexer.d.buf.buf
  lexer.advance()

  while cnt > 0 and lexer[] != OEndOfFile:
    if lexer[] == del0:
      inc cnt

    elif lexer[] == del1:
      dec cnt

    result.add lexer.pop()

  dec result.ranges



proc skipPositional*(lexer; skip: set[char]): Position =
  while lexer[] in skip:
    inc result.offset
    if lexer[] in Newlines:
      inc result.line
      result.column = 0

    else:
      inc result.column

    lexer.advance()

proc skipIndentGeq*(lexer; indent: int): Position =
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

proc newSublexer*(strbuf: StrBuf, ranges: StrRanges): Lexer =
  result.d = LexerImpl(
    buf: initStrSlice(strbuf, ranges), bufpos: ranges[0][0])

proc newLexer*(slice: StrSlice): Lexer =
  result.d = LexerImpl(buf: slice, bufpos: 0)

proc newSublexer*(lexer; ranges: StrRanges): Lexer =
  newSublexer(lexer.d.buf.buf, ranges)

proc newSublexer*(strbuf: StrSlice): Lexer =
  result.d = LexerImpl(buf: strbuf, bufpos: strbuf.ranges[0][0])

proc getBuf*(lexer): StrBuf = lexer.d.buf.buf

proc getPosition*(lexer): Position =
  Position(
    line: lexer.buf.buf.colNumber(lexer.bufpos),
    column: lexer.buf.buf.lineNumber(lexer.bufpos),
    offset: lexer.bufpos
  )

proc pop*(str: var string): char {.discardable, inline.} =
  result = str[str.high]
  str.setLen(str.high)


proc cutIndentedBlock*(
    lexer; indent: int,
    keepNewlines: bool = true,
    requireContinuation: bool = false,
    fromInline: bool = true,
    atEnd: proc(lexer: var Lexer): bool = nil
  ): StrRanges =
  ## - @arg{requireContinuation} - only continue extracting line that are
  ##   appended with `\` character
  ## - @arg{fromInline} - block cutout begins from somewhere inside
  ##   current line and all characters until EOF should be added to buffer.


  var firstLine = true

  if fromInline:
    result = lexer.initStrRanges()
    while lexer[] notin OLineBreaks:
      result.add lexer.pop

  else:
    let ind = lexer.getIndent()
    if ind < indent:
      return

    else:
      lexer.advance(indent)
      result = lexer.initStrRanges()


  if requireContinuation:
    while lexer[-1] == '\\':
      lexer.advance()

      if lexer.getIndent() >= indent:
        discard lexer.skipIndentGeq(indent)
        while lexer[] notin OLinebreaks:
          result.add lexer.pop

      else:
        break

  else:
    while lexer[] != EndOfFile and (atEnd.isNil or not atEnd(lexer)):
      let ind = tern(firstLine, 999, lexer.getIndent())

      if indent == 0 and lexer[] in OLineBreaks:
        break

      elif ind >= indent or firstLine:
        if not firstLine:
          lexer.advance(indent)

        firstLine = false

        while lexer[] notin OLineBreaks:
          result.add lexer.pop

        if keepNewlines:
          result.add lexer.pop

        else:
          lexer.advance()

      else:
        break

  # lexer.advance()


proc findOnLine*(lexer; target: string): int =
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
        return -1

    inc lexIdx

proc atEnd*(lexer): bool = lexer[] == OEndOfFile
proc atBigIdent*(lexer): bool =
  ## Determine if lexer is positioned at the start of ident, comprized only
  ## from @enum{OBigIdentChars}
  var bigchars = 0
  var allchars = 0
  while lexer[allchars] notin OWhitespace:
    inc allchars
    if lexer[allchars] in OBigIdentchars:
      inc bigchars



  return bigchars == allchars



proc lineStartsWith*(lexer; str: string): bool = lexer.findOnLine(str) != -1
proc indentTo*(lexer; str: string): int =
  assert lexer[-1] in Newlines,
     "Test for indentation to must be performed only on line starts"

  lexer.findOnLine(str)


proc allUntil*(lexer; allChars, untilChars: set[char]): bool =
  ## Check if lexer is at the start of block consisting of `allChars`,
  ## followed by `untilChars`
  ##
  ## - @arg{allChars} :: Prefix chars for block, like OIdentChars for
  ##   `some-identifier{}`
  ## - @arg{untilChars} :: Charset that MUST immediately follow `allChars`
  ##   block
  var idx = 0
  while lexer[idx] in allChars:
    inc idx

  result = lexer[idx] in untilChars



func nextSet*(
  lexer; set1, set2: set[char], direction: int = +1): range[0 .. 1] =

  var idx = 0
  while lexer[idx] notin set1 + set2:
    inc idx, direction

  if lexer[idx] in set1:
    return 0

  else:
    return 1

func listStartChar*(lexer): char =
  let ch = lexer[]
  if ch in OListChars:
    var numset: set[char]
    case ch:
      of {'0' .. '9'}:
        result = '0'
        numset = {'0' .. '9'}

      of {'a' .. 'z'}:
        result = 'a'
        numset = {'a' .. 'z'}

      of {'A' .. 'Z'}:
        result = 'A'
        numset = {'A' .. 'Z'}

      of OBulletListChars:
        result = ch
        numset = OBulletListChars

      else:
        raiseAssert("#[ IMPLEMENT ]#")

    if numset == OBulletListChars:
      var idx = 0
      while lexer[idx] in numset:
        inc idx

      if lexer[idx] notin OWhitespace:
        result = OEndOfFile

    else:
      var idx = 0
      while lexer[idx] in numset:
        inc idx

      if lexer[idx] notin {'.', ')'}:
        result = OEndOfFile

  else:
    result = OEndOfFile


func countCurrAhead*(lexer): int =
  let ch = lexer[]
  while lexer[result] == ch and not lexer.atEnd():
    inc result

proc setAround(
    lexer; set0, set1: set[char]
  ): tuple[behind, ahead: range[0 .. 1]] =

  result.ahead = lexer.nextSet(set0, set1, +1)
  result.behind = lexer.nextSet(set0, set1, -1)

proc `==`*(
    t1: (range[0 .. 1], range[0 .. 1]),
    t2: (int, int)
  ): bool =
    t1[0] == t2[0] and t1[1] == t2[1]

proc isOpenAt*(
    lexer;
    ch: var string,
    toggleChars: set[char] = OMarkupChars,
  ): bool =
  ## Check if lexer positioned on the start of *constrained* markup
  ## section and save markup character to `ch`.
  ##
  ## NOTE: if placed on unconstrained section `false` will be returned,
  ## making it mutially exclusive with `isToggleAt`
  if lexer[] in OVerbatimChars:
    result =
      lexer.countCurrAhead() == 1 and
      lexer[-1] in OWhitespace + OLineBreaks

  else:
    result =
      lexer[] in toggleChars and
      lexer.countCurrAhead() == 1 and
      lexer.setAround(OWhitespace + OLineBreaks, OWordChars) == (0, 1)

  if result:
    ch = $lexer[]


proc isCloseAt*(
    lexer;
    ch: var string,
    toggleChars: set[char] = OMarkupChars,
  ): bool =
  ## Check if lexer positioned on the end of *constrained* markup
  ## section and save markup character to `ch`.
  ##
  ## NOTE: if placed on unconstrained section `false` will be returned,
  ## making it mutially exclusive with `isToggleAt`
  if lexer[] in OVerbatimChars:
    result =
      lexer.countCurrAhead() == 1 and
      lexer[+1] in OWhitespace + OLineBreaks

  else:
    result =
      lexer[] in toggleChars and
      lexer.countCurrAhead() == 1 and
      lexer.setAround(OWordChars, OWhitespace + OLineBreaks) == (0, 1)

  if result:
    ch = $lexer[]


proc isToggleAt*(
    lexer;
    ch: var string,
    toggleChars: set[char] = OMarkupChars
  ): bool =

  ## Check if lexer positioned on toggle point of *uconstrained* markup
  ## section and save markup character to `ch`.
  ##
  ## NOTE: if placed on constrained section `false` will be returned,
  ## making it mutially exclusive with `isCloseAt` and `isOpenAt`
  result =
    lexer.countCurrAhead() == 2 and
    lexer[] in toggleChars

  if result:
    ch = $lexer[]

proc isBalancedToEOL*(lexer): bool =
  ## Check if lexer is currently positioned on balanced pair of explicit
  ## open/close delimiters (parentheses, braces etc.). Returns false if
  ## currently positioned on non-delimiter open character (e.g. not in
  ## `{({<`)
  let openChar = lexer[]
  let closeChar = case openChar:
    of '[': ']'
    of '<': '>'
    of '(': ')'
    of '{': '}'
    else: return false

  var lexer = lexer
  var cnt = 1

  lexer.advance()
  while not lexer.atEnd():
    if lexer[] == openChar:
      inc cnt

    elif lexer[] == closeChar:
      dec cnt

    else:
      discard

    if cnt == 0:
      return true

    else:
      lexer.advance()





proc allRangesTo*(
    lexer: Lexer; str: string,
    minIndex: int = -1,
    repeatIncluding: bool = false,
    remaining: bool = false
  ): seq[StrRanges] =
  ## Return all ranges until the start of `str`
  ## - @arg{remaining} :: After finding all prefix ranges, also include
  ##   leftower indices in result
  ## - @arg{repeatIncluding} :: For each occurence of `str` add it to range
  ##   twice - *prefix* and *prefix + string itself*. Can be used to determine
  ##   ranges for particular occurence of `str` in text.
  ## - @arg{minIndex} :: Only consider indices that are greater then `minIndex`

  # hello




  var lexer = deepCopy(lexer)
  var stack: seq[string]
  var ranges: StrRanges
  while not lexer.atEnd():
    if lexer[str] and stack.len == 0 and lexer.bufpos > minIndex:
      result.add ranges

      if repeatIncluding:
        for i in 0 .. str.high:
          ranges.add lexer.pop()

        result.add ranges

    var ch: string
    var ch2: string
    if lexer.isOpenAt(ch):
      stack.add ch

    elif lexer.isCloseAt(ch):
      stack.popUntil(ch)

    elif lexer.isToggleAt(ch):
      if ch in stack:
        stack.popUntil(ch)

      else:
        stack.add ch

    if ch.len > 0:
      for _ in 0 ..< ch.len:
        if lexer.bufpos > minIndex:
          ranges.add lexer.pop()

        else:
          lexer.advance()

    else:
      if lexer.bufpos > minIndex:
        ranges.add lexer.pop()

      else:
        lexer.advance()

  if remaining:
    result.add ranges

  else:
    if stack.len > 0:
      return @[]


proc firstRangesTo*(
    lexer; str: string,
    minIndex = -1
  ): StrRanges =

  ## Search for string `str` in lexer lookahead and return all precesing
  ## string ranges. This does not modifty lexer position.

  let ranges = lexer.allRangesTo(str, minIndex = minIndex)

  if ranges.len > 0:
    return ranges[0]

proc lastRangesTo*(
    lexer; str: string,
    minIndex = -1
  ): StrRanges =

  let ranges = lexer.allRangesTo(str, minIndex = minIndex)

  if ranges.len > 0:
    return ranges[^1]




proc indentedSublexer*(
    lexer;
    indent: int,
    keepNewlines: bool = true,
    requireContinuation: bool = true,
    fromInline: bool = false,
    atEnd: proc(lexer: var Lexer): bool = nil
  ): Lexer =

  newSublexer(
    lexer.buf.buf,
    lexer.cutIndentedBlock(
      indent,
      keepNewlines = keepNewlines,
      requireContinuation = requireContinuation,
      fromInline = fromInline,
      atEnd = atEnd
    )
  )

proc blockSublexer*(lexer; str: string, dedent: bool = true): Lexer =
  newSublexer(
    lexer.buf.buf,
    lexer.getBlockUntil(str = str, dedent = dedent)
  )

proc ranges*(lexer): StrRanges =
  lexer.d.buf.ranges

proc pstringRanges*(lexer): string =
  for srange in lexer.d.buf.ranges:

    let tmp = lexer.d.buf.buf.str[
      srange.start .. min(
        srange.finish,
        lexer.d.buf.buf.str.high
    )].multiReplace({
        "\n" : "\\n"
    })

    result &= &"{srange}: [\"{toGreen(tmp)}\"]"
    if lexer.d.bufpos in srange.start .. srange.finish:
      result &= toRed(" << " & $lexer.d.bufpos)

    result &= "\n"
