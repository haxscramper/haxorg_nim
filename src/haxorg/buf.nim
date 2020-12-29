import hmisc/hdebug_misc

const OEndOfFile = '\x00'

type
  SegTree* = object
    ## Mapping between abbitrary elements in string buffer and linen number
    ## (to avoid scanning whole buffer for line endings)
    ranges: seq[tuple[start, finish, linum: int]] # This should be replaced
    # with segment tree for better performance (right now getting line is
    # still linear wrt. number of lines)

  StrBuf* = ref object
    str*: string ## Buffer data
    # Can be replaced with any data structure that provides indexed access
    # via `[]`.
    lineranges: SegTree ## offset range -> line number mapping

  StrRanges* = seq[tuple[start, finish: int]]

  StrSlice* = object
    buf*: StrBuf
    ranges*: seq[tuple[start, finish: int]]

func add*(ranges: var StrRanges, pos: int) =
  if ranges.len == 0:
    ranges.add (pos, pos)

  elif ranges[^1][1] + 1 < pos:
    ranges.add (pos, pos)

  else:
    ranges[^1][1] = pos

func add*(sslice: var StrSlice, pos: int) =
  sslice.ranges.add pos

func pop*(ranges: var StrRanges): int {.discardable, inline.} =
  result = ranges[^1][1]
  dec ranges[^1][1]

func pop*(sslice: var StrSlice): int {.discardable, inline.} =
  sslice.ranges.pop()

func len*(ss: StrSlice): int =
  for (start, finish) in ss.ranges:
    result += finish - start

iterator items*(ss: StrSlice): char =
  for srange in ss.ranges:
    for idx in srange.start .. srange.finish:
      yield ss.buf.str[idx]

iterator indices*(ranges: StrRanges): int =
  for srange in ranges:
    for idx in srange.start .. srange.finish:
      yield idx


iterator indices*(sslice: StrSlice): int =
  for idx in indices(sslice.ranges):
    yield idx


func `==`*(ss: StrSlice, str: string): bool =
  var idx = 0
  for ch in ss:
    if idx > str.high:
      break

    if ch != str[idx]:
      return false

    else:
      inc idx

  return true

func split*(ss: StrSlice, sep: string): seq[StrRanges] =
  raiseAssert("#[ IMPLEMENT ]#")

func strip*(ss: StrSlice): StrRanges =
  raiseAssert("#[ IMPLEMENT ]#")

func initStrSlice*(buf: StrBuf, ranges: seq[(int, int)]): StrSlice =
  StrSlice(buf: buf, ranges: ranges)

func initStrSlice*(buf: StrBuf, start, finish: int): StrSlice =
  StrSlice(buf: buf, ranges: @[(start: start, finish: finish)])

func initStrRanges*(ranges: StrRanges): StrRanges =
  @[(ranges[0][0], ranges[0][0])]

func initStrRanges*(start, finish: int): StrRanges =
  @[(start, finish)]

func succ*(slice: StrSlice, idx: int): int =
  ## Return next main index that is in `slice`.
  for rangeIdx in 0 .. slice.ranges.len:
    if slice.ranges[rangeIdx].start <= idx and
       idx < slice.ranges[rangeIdx].finish:
      return idx + 1

    elif slice.ranges[rangeIdx].finish == idx:
      if rangeIdx == slice.ranges.high:
        return -1

      else:
        return slice.ranges[rangeIdx + 1].start


  return idx + 1

func high*(strbuf: StrBuf): int =
  strbuf.str.high

func high*(slice: StrSlice): int =
  slice.ranges[^1].finish

func `[]`*(strbuf: StrBuf, pos: int): char =
  if pos < 0 or pos > strbuf.high:
    OEndOfFile

  else:
    strbuf.str[pos]

func `[]`*(strbuf: StrBuf, slice: Slice[int]): string =
  for i in slice:
    result.add strbuf[i]

func `[]`*(sslice: StrSlice, slice: Slice[int]): string =
  sslice.buf[slice]

func `[]`*(slice: StrSlice, pos: int): char =
  # echov pos
  # echov slice.ranges[^1]
  if pos < 0 or slice.ranges[^1][1] < pos or slice.buf.high < pos:
    OEndOfFile

  else:
    slice.buf.str[pos]

func lastChar*(sslice: StrSlice): char =
  sslice.buf.str[sslice.ranges[^1][1]]


func lineNumber*(rangeTree: SegTree, pos: int): int =
  ## Return line number containing byte at `pos`
  for rng in rangeTree.ranges:
    if rng.start <= pos and pos <= rng.finish:
      return rng.linum

func lineStart*(stree: SegTree, pos: int): int =
  ## Return byte position for line start
  for rng in stree.ranges:
    if rng.start <= pos and pos <= rng.finish:
      return rng.start



func lineNumber*(strbuf: StrBuf, pos: int): int =
  ## Return line number for main index `pos`
  lineNumber(strbuf.lineranges, pos)

func lineNumber*(slice: StrSlice, pos: int): int =
  lineNumber(slice.buf.lineranges, pos)

func colNumber*(strbuf: StrBuf, pos: int): int =
  ## Return column number for byte at `pos`
  pos - strbuf.lineranges.lineStart(pos)

func newStrBufSlice*(str: string): StrSlice =
  StrSlice(
    buf: StrBuf(str: str),
    ranges: @[(0, str.len)]
  )
