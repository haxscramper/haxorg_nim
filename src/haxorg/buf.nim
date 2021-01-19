import hmisc/hdebug_misc
import hmisc/algo/halgorithm
import std/enumerate

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
    case isFake*: bool
      of false:
        buf*: StrBuf
        ranges*: seq[tuple[start, finish: int]]

      of true:
        after*: int
        str*: string

func add*(ranges: var StrRanges, pos: int) =
  if ranges.len == 0:
    ranges.add (pos, pos)

  elif ranges[^1][1] + 1 < pos:
    ranges.add (pos, pos)

  else:
    ranges[^1][1] = pos


proc absAt*(slice: StrSlice, idx: int): char =
  if idx > slice.buf.str.high or idx < 0:
    '\x00'

  else:
    slice.buf.str[idx]

func add*(sslice: var StrSlice, pos: int) =
  sslice.ranges.add pos

func pop*(ranges: var StrRanges): int {.discardable, inline.} =
  result = ranges[^1][1]
  dec ranges[^1][1]

func pop*(sslice: var StrSlice): int {.discardable, inline.} =
  sslice.ranges.pop()

func len*(ss: StrSlice): int =
  for (start, finish) in ss.ranges:
    result += finish - start + 1

iterator items*(ss: StrSlice): char =
  for srange in ss.ranges:
    for idx in srange.start .. srange.finish:
      yield ss.buf.str[idx]

iterator indices*(ranges: StrRanges): int =
  for srange in items(ranges):
    for idx in srange.start .. srange.finish:
      yield idx


iterator indices*(sslice: StrSlice): int =
  for idx in indices(sslice.ranges):
    yield idx

iterator rindices*(sslice: StrSlice): int =
  for srange in ritems(sslice.ranges):
    for idx in countdown(srange.finish, srange.start):
      yield idx

func overlapping*(prefix: seq[StrRanges], all: StrRanges): StrRanges =
  var prefIndices: set[int16]
  for srange in prefix:
    for idx in indices(srange):
      prefIndices.incl int16(idx)

  for idx in indices(all):
    if int16(idx) notin prefIndices:
      result.add idx

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

func `$`*(ss: StrSlice): string =
  # This is a hack to have some sort of zero-length slices, which are
  # otherwise not possible, since ranges are *inclusive*.
  # if ss.ranges.len == 1 and ss.ranges[0][0] == ss.ranges[0][1]:
  #   return

  # else:
    for idx in indices(ss):
      result &= ss.buf.str[idx]

func initStrSlice*(buf: StrBuf, ranges: seq[(int, int)]): StrSlice =
  StrSlice(isFake: false, buf: buf, ranges: ranges)

func initStrSlice*(buf: StrBuf, start, finish: int): StrSlice =
  StrSlice(isFake: false, buf: buf, ranges: @[(start: start, finish: finish)])

func initStrRanges*(ranges: StrRanges): StrRanges =
  @[(ranges[0][0], ranges[0][0])]

func initStrRanges*(start, finish: int): StrRanges =
  @[(start, finish)]

func pred*(slice: StrSlice, idx: int): int =
  for rangeIdx in 0 .. slice.ranges.high:
    if slice.ranges[rangeIdx].start < idx and
       idx <= slice.ranges[rangeIdx].finish:
      return idx - 1

    elif slice.ranges[rangeIdx].start == idx:
      if rangeIdx == 0:
        return -1

      else:
        return slice.ranges[rangeIdx - 1].finish


  return -1

func succ*(slice: StrSlice, idx: int): int =
  ## Return next main index that is in `slice`.
  for rangeIdx in 0 .. slice.ranges.high:
    if slice.ranges[rangeIdx].start <= idx and
       idx < slice.ranges[rangeIdx].finish:
      return idx + 1

    elif slice.ranges[rangeIdx].finish == idx:
      if rangeIdx == slice.ranges.high:
        return -1

      else:
        return slice.ranges[rangeIdx + 1].start


  return -1

func shift*(slice: StrSlice, pos, shift: int): int =
  result = pos
  if shift > 0:
    for i in 0 ..< shift:
      result = slice.succ(result)

  elif shift < 0:
    for i in 0 ..< abs(shift):
      result = slice.pred(result)

func dec*(ranges: var StrRanges) =
  if ranges[^1][0] < ranges[^1][1]:
    dec ranges[^1][1]

  else:
    discard ranges.pop

func dropStart*(srange: var StrRanges) =
  srange[0 ..^ 2] = srange[1 ..^ 1]
  srange.setLen(srange.len - 1)

func split*(ss: StrSlice, sep: char): seq[StrRanges] =
  result = @[initStrRanges(-10, -10)]
  for idx in indices(ss):
    if ss.buf.str[idx] != sep:
      result[^1].add idx

    else:
      result.add initStrRanges(-10, -10)

  for srange in mitems(result):
    srange.dropStart()

  # echov result

func strip*(ss: StrSlice, chars: set[char] = {' '}): StrRanges =
  var start = ss.ranges[0][0]

  for idx in indices(ss):
    if ss.buf.str[idx] notin chars:
      start = idx
      break

  var finish = ss.ranges[^1][1]

  for idx in rindices(ss):
    if ss.buf.str[idx] notin chars:
      finish = idx
      break

  result = ss.ranges

  result[0][0] = start
  result[^1][1] = finish


func high*(strbuf: StrBuf): int =
  strbuf.str.high

func high*(slice: StrSlice): int =
  slice.ranges[^1].finish


func `[]`*(slice: StrSlice, pos: BackwardsIndex): char =
  var posIdx = 0
  var cnt = 0
  for idx in rindices(slice):
    posIdx = idx
    inc cnt
    if cnt >= pos.int:
      break

  slice.buf.str[posIdx]

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
  if pos < 0 or slice.ranges[^1][1] < pos or slice.buf.high < pos:
    return OEndOfFile

  else:
    for posIdx, posVal in enumerate(indices(slice)):
      if posIdx == pos:
        return slice.buf.str[posVal]

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
    echov rng
    if rng.start <= pos and pos <= rng.finish:
      return rng.start



func lineNumber*(strbuf: StrBuf, pos: int): int =
  ## Return line number for main index `pos`
  lineNumber(strbuf.lineranges, pos)

func lineNumber*(slice: StrSlice, pos: int): int =
  lineNumber(slice.buf.lineranges, pos)

func colNumber*(strbuf: StrBuf, pos: int): int =
  ## Return column number for byte at `pos`
  var pos = pos
  if pos == 0:
    return 0

  while strbuf.str[pos] != '\n':
    inc result
    dec pos

  dec result

  when false:
    pos - strbuf.lineranges.lineStart(pos)

func newStrBufSlice*(str: string): StrSlice =
  StrSlice(
    isFake: false,
    buf: StrBuf(str: str),
    ranges: @[(0, str.len)]
  )
