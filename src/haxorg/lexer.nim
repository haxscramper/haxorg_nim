{.experimental: "dotOperators".}

import std/[tables, strutils, strformat, sequtils, streams, strscans, macros]
import common, buf


import std/lexbase except Newlines
export open

import hmisc/hexceptions
import hmisc/algo/hstring_algo
import hmisc/hdebug_misc
import hmisc/helpers


type
  LexerImpl = ref object
    buf: StrSlice
    bufpos: int


  Lexer* = object
    # The only reason for splitting implementation into two parts is
    # overload for `[]` operator without parameters (for ref types it
    # cannot be overloaded)
    d: LexerImpl

  PosIncrements* = Table[int, Position]

using lexer: var Lexer

template bufpos(lexer): untyped = lexer.d.bufpos
template buf(lexer): untyped = lexer.d.buf
template `bufpos=`(lexer; val: int): untyped = lexer.d.bufpos = val

{.push inline.}

proc line*(lexer): int = lexer.buf.lineNumber(lexer.bufpos)

proc `[]`*(lexer): char = lexer.buf[lexer.bufpos]

proc `[]`*(lexer; idx: int): char =
  if idx + lexer.bufpos < 0:
    EndOfFile

  else:
     lexer.buf[lexer.bufpos + idx]

proc `[]`*(lexer; slice: Slice[int]): string =
  result = lexer.buf[
        lexer.bufpos + slice.a ..
    min(lexer.bufpos + slice.b, lexer.buf.high)
  ]

proc `[]`*(lexer; slice: HSlice[int, BackwardsIndex]): string =
  lexer.buf[lexer.bufpos + slice.a .. lexer.buf.high]

proc `@?`*(lexer; slice: Slice[int]): seq[char] = @(lexer[slice])

proc `[]`*(lexer; str: string): bool =
  result = lexer.buf[lexer.bufpos ..< lexer.bufpos + str.len] == str

{.pop.}

template atom*(lexer; idx: int; c: char): bool =
  ## Test if curent lexer character is equal to char `c`
  lexer[] == c

template atom*(lexer; idx: int; s: set[char]): bool =
  ## Test of current lexer character is in set `s`
  lexer[] in s

template nxt*(lexer; idx, step: int = 1) =
  ## Advance positions in lexer by `step` steps
  for i in 0 ..< step:
    lexer.advance()

proc hasNxt*(lexer: Lexer; idx: int): bool =
  lexer.buf[lexer.bufpos] != EndOfFile

template lexScanp*(lexer; pattern: varargs[untyped]): bool =
  var idx: int = 0
  scanp(lexer, idx, pattern)


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

  for _ in 0 ..< chars:
    lexer.bufpos = lexer.buf.succ(lexer.bufpos)

proc succ*(lexer): int = lexer.buf.succ(lexer.bufpos)

proc pop*(lexer): int {.inline.} =
  result = lexer.bufpos
  lexer.advance()

proc expect*(lexer; chars: set[char]) =
  discard

func initStrRanges*(lexer): StrRanges =
  @[(lexer.bufpos, lexer.bufpos)]

proc getSkipWhile*(lexer; chars: set[char]): StrRanges =
  result = lexer.initStrRanges()

  while lexer[] in chars - {EndOfFile}:
    result.add lexer.pop



proc getBlockUntil*(
    lexer; str: string, leftMargin: int = 0,
    dedent: bool = true
  ): StrSlice =

  var ranges: seq[(int, int)] = @[(lexer.bufpos, lexer.bufpos)]

  template lexAdvance =
    if lexer.succ() != ranges[^1][1] + 1:
      lexer.advance()
      ranges.add (lexer.bufpos, lexer.bufpos)

  block mainSearch:
    while lexer[] != EndOfFile:
      while lexer[] notin {str[0], EndOfFile}:
        lexAdvance()

      if lexer[str]:
        break mainSearch

      else:
        if lexer[] != EndOfFile:
          lexAdvance()


  return initStrSlice(lexer.buf.buf, ranges)


proc error*(lexer; message: string, annotation: string = ""): CodeError =
  toCodeError(
    lexer.buf.buf.str,
    message = message,
    exprLen = 5,
    offset = lexer.bufpos,
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

proc getSkipToEOL*(lexer): StrRanges =
  lexer.getSkipUntil(Newlines)

proc nextLine*(lexer) =
  ## Move lexer position to the start of new line. Update column, line
  ## number and other fields accordingly.

  while lexer[] notin Newlines + {EndOfFile}:
    lexer.advance()

proc getInsideSimple*(lexer; delimiters: (char, char)): StrRanges =
  ## Get text enclosed with `delimiters`. No special heuristics is used to
  ## determine balanced pairs, internal string literals etc. Text is cut
  ## from current position + 1 until first ocurrence of `delimiters[1]`. To
  ## get balanced pairs use `getInsideBalanced()`
  assert lexer[] == delimiters[0]
  lexer.advance()
  result = lexer.getSkipUntil({delimiters[1]})
  lexer.advance()

proc getInsideBalanced*(lexer; delimiters: (char, char)): StrSlice =
  # - TODO handle escaped characters in form of `\delimiters[1]`
  var cnt: int = 0
  let start = lexer.bufpos


  assert lexer[] == delimiters[0]
  inc cnt

  lexer.advance()

  while cnt > 0:
    if lexer[] == delimiters[0]:
      inc cnt

    elif lexer[] == delimiters[1]:
      dec cnt

    lexer.advance()

  let finish = lexer.bufpos() - 1
  return initStrSlice(lexer.buf.buf, start, finish)



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

proc getIndent*(lexer): int =
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



proc newSublexer*(strbuf: StrBuf, ranges: StrRanges): Lexer =
  result.d.buf = initStrSlice(strbuf, ranges)

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
    requireContinuations: bool = false,
    fromInline: bool = true
  ): StrRanges =
  ## - @arg{requireContinuation} - only continue extracting line that are
  ##   appended with `\` character
  ## - @arg{fromInline} - block cutout begins from somewhere inside
  ##   current line and all characters until EOF should be added to buffer.

  result = lexer.initStrRanges()

  if fromInline:
    while lexer[] notin OLineBreaks:
      result.add lexer.pop


  if requireContinuations:
    while lexer[-1] == '\\':
      lexer.advance()

      if lexer.getIndent() >= indent:
        discard lexer.skipIndentGeq(indent)
        while lexer[] notin OLinebreaks:
          result.add lexer.pop

      else:
        break

  else:
    while lexer[] != EndOfFile:
      let ind = lexer.getIndent()
      if indent == 0 and lexer[] in Newlines:
        break

      elif ind >= indent:
        lexer.advance(indent)

        while lexer[] notin OLineBreaks:
          result.add lexer.pop

        if keepNewlines:
          result.add lexer.pop

        else:
          lexer.advance()

      else:
        break

  lexer.advance()

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
        break

    inc lexIdx


proc lineStartsWith*(lexer; str: string): bool = lexer.findOnLine(str) != -1
proc indentTo*(lexer; str: string): int =
  assert lexer[-1] in Newlines,
     "Test for indentation to must be performed only on line starts"

  lexer.findOnLine(str)



proc indentedSublexer*(
    lexer;
    indent: int,
    keepNewlines: bool = true,
    requireContinuation: bool = true,
    fromInline: bool = false
  ): Lexer =

  newSublexer(
    lexer.buf.buf,
    lexer.cutIndentedBlock(
      indent,
      keepNewlines = keepNewlines,
      requireContinuations = requireContinuation,
      fromInline = fromInline
    )
  )
