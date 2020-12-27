import std/[tables, strutils, strformat, sequtils, streams, strscans]
import common


import std/lexbase except Newlines
export open

import hmisc/hexceptions
import hmisc/algo/hstring_algo
import hmisc/hdebug_misc


type
  Lexer* = object of BaseLexer
    currIndent*: int ## Current indentation level. When parsing over
    ## newline followed by whitespaces, `currIdent` is used to
    ## automatically skip them. To check for actulual indentation of the
    ## next line use `nextLineIndent()`.

    column*: int ## Current column in lexer - used for correct annotation
    ## positioning, renewal of `PosText` buffers

    positionIncrements: PosIncrements ## Additional position increments for
    ## sublexers. Contains mapping between buffer character positions and
    ## line/column increments. This allows to cut parts of original input
    ## stream with some characters discarded into sublexers, while still
    ## maintaining correct positional information. `Position` contains
    ## `line` and `column` increment, as well as number of skipped
    ## characters in `offset` field.

  PosIncrements* = Table[int, Position]

using lexer: var Lexer

{.push inline.}

proc `line=`*(lexer; line: int) = lexer.lineNumber = line

proc line*(lexer): int = lexer.lineNumber

proc `[]`*(lexer): char = lexer.buf[lexer.bufpos]

proc `[]`*(lexer; idx: int): char =
  if idx + lexer.bufpos < 0:
    EndOfFile

  else:
     lexer.buf[lexer.bufpos + idx]

proc `[]`*(lexer; slice: Slice[int]): string =
  result = lexer.buf[lexer.bufpos + slice.a .. min(
    lexer.bufpos + slice.b,
    lexer.buf.high
  )]

proc `[]`*(lexer; slice: HSlice[int, BackwardsIndex]): string =
  lexer.buf[lexer.bufpos + slice.a .. lexer.buf.high]


proc `@?`*(lexer; slice: Slice[int]): seq[char] = @(lexer[slice])

proc `[]`*(lexer; str: string): bool =
  result = lexer.buf[lexer.bufpos ..< lexer.bufpos + str.len] == str

proc initPosText*(lexer): PosText =
  PosText(line: lexer.lineNumber, column: lexer.column)

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
    case lexer[]:
      of '\n':
        lexer.bufpos = lexer.handleLF(lexer.bufpos)
        lexer.column = 0

      of '\r':
        lexer.bufpos = lexer.handleCR(lexer.bufpos)

      else:
        inc lexer.bufpos
        inc lexer.column


proc pop*(lexer): char {.inline.} =
  result = lexer[]
  lexer.advance()

proc expect*(lexer; chars: set[char]) =
  discard

proc getSkipWhile*(lexer; chars: set[char]): PosText =
  var slice = lexer.bufpos .. lexer.bufpos

  while lexer.buf[slice.b] in chars:
    inc slice.b

  dec slice.b

  result = PosText(
    line: lexer.lineNumber,
    column: lexer.getColNumber(lexer.bufpos),
    text: lexer.buf[slice])

  lexer.advance(result.len)

proc getBlockUntil*(lexer; str: string, leftMargin: int = 0): PosText =
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

proc error*(lexer; message: string, annotation: string = ""): CodeError =
  toCodeError(
    lexer.buf,
    message = message,
    exprLen = 5,
    offset = lexer.offsetBase + lexer.bufpos,
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

proc getSkipUntil*(lexer; chars: set[char]): PosText =
  result = lexer.getSkipWhile(AllChars - chars)

proc getSkipWhileTo*(lexer; chars: set[char], to: char): PosText =
  result = getSkipWhile(lexer, chars)
  if lexer[] != to:
    discard lexer.error(&"Expected '{to}', but found '{lexer[]}'")

proc getSkipToEOL*(lexer): PosText =
  if lexer[] in Newlines:
    lexer.initPosText()

  else:
    lexer.getSkipUntil(Newlines)

proc nextLine*(lexer) =
  ## Move lexer position to the start of new line. Update column, line
  ## number and other fields accordingly.

  if lexer[] in Newlines:
    lexer.bufpos = lexer.handleLF(lexer.bufpos)
    lexer.column = 0

  else:
    while lexer[] notin Newlines + {EndOfFile}:
      lexer.advance()

    lexer.bufpos = lexer.handleLF(lexer.bufpos)
    lexer.column = 0

proc getInsideSimple*(lexer; delimiters: (char, char)): PosText =
  ## Get text enclosed with `delimiters`. No special heuristics is used to
  ## determine balanced pairs, internal string literals etc. Text is cut
  ## from current position + 1 until first ocurrence of `delimiters[1]`. To
  ## get balanced pairs use `getInsideBalanced()`
  assert lexer[] == delimiters[0]
  lexer.advance()
  result = lexer.getSkipUntil({delimiters[1]})
  lexer.advance()

proc getInsideBalanced*(lexer; delimiters: (char, char)): PosText =
  var cnt: int = 0
  assert lexer[] == delimiters[0]
  inc cnt

  lexer.advance()

  result = lexer.initPosText()

  while cnt > 0:
    if lexer[] == delimiters[0]:
      inc cnt

    elif lexer[] == delimiters[1]:
      dec cnt

    result.add lexer.pop

  result.pop



proc startNew*(lexer; buffer: var PosText) =
  buffer.text = ""
  buffer.line = lexer.line
  buffer.column = lexer.column

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



proc newSublexer*(
  pos: Position, str: string, increments: PosIncrements): Lexer =
  ## Create new lexer using string stream `str`, with global positioning
  ## from `pos`. Text block positionsing generated from lexer would be
  ## correct, assuming `pos` was initally set right.
  open(result, newStringStream(str))
  result.line = pos.line
  result.column = pos.column
  result.offsetBase = pos.offset

proc newSublexer*(
  pos: Position, pair: (string, PosIncrements)): Lexer {.inline.} =
  # IDEA test sublexer creation from completely arbitrary sources of text,
  # such as trailing comments in source code. This would allow to provide
  # correct line positions (spell-checking, broken grammar errors and more)
  # for text located anywhere.

  newSublexer(pos, pair[0], pair[1])

proc getPosition*(lexer): Position =
  Position(
    line: lexer.line,
    column: lexer.column,
    offset: lexer.offsetBase
  )

proc pop*(str: var string): char {.discardable, inline.} =
  result = str[str.high]
  str.setLen(str.high)


proc cutIndentedBlock*(
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
        break

  else:
    while lexer[] != EndOfFile:
      let ind = lexer.getIndent()
      if indent == 0 and lexer[] in Newlines:
        break

      elif ind >= indent:
        lexer.advance(indent)
        result[1][result[0].high] = lexer.skipIndentGeq(indent)
        result[0].add lexer.getSkipUntil({'\n', EndOfFile}).text
        if keepNewlines:
          result[0].add '\n'

        elif result[0][^1] != ' ':
          result[0].add ' '

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
    lexer.getPosition(),
    lexer.cutIndentedBlock(
      indent,
      keepNewlines = keepNewlines,
      requireContinuations = requireContinuation,
      fromInline = fromInline
    )
  )
