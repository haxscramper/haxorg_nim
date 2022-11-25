import
  haxorg/[
    enum_types,
    types,
    lexer,
    parse_org_common
  ],
  hmisc/core/[
    all
  ],
  hmisc/algo/[
    clformat,
    hparse_base
  ],
  std/[
    algorithm,
    sequtils,
    strformat
  ]

export clformat, hparse_base




func showImpl(
    e: Lexer, ahead: int, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  result = hshow(e.pos, opts)
  result &= "/"
  result &= hshow(e.tokens.high, opts)

  for tok in e.pos .. min(e.tokens.high, e.pos + ahead):
    result &= " "
    result &= hshow(e.tokens[tok], opts)


func hShow*(e: Lexer, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  showImpl(e, 8, opts)

func `$`*(e: Lexer, ahead: int = 8): string =
  $showImpl(e, ahead)

func `[]`*(lex: Lexer, offset: int = 0): OrgTokenKind =
  lex.tokens[lex.pos + offset].kind

func newFail(lex: Lexer, ahead: int = 8): ref UnexpectedKindError =
  newUnexpectedKinderror(lex[], lex $ ahead)


func get*(lex: Lexer, offset: int = 0): OrgToken =
  lex.tokens[lex.pos + offset]

func next*(lex: var Lexer) =
  inc lex.pos

func skip*(lex: var Lexer, expected: OrgTokenKind) =
  assert lex[] == expected, &"wanted: {expected}, got: {lex}"
  lex.next()

func skip*(lex: var Lexer, expected: set[OrgTokenKind]) =
  assert lex[] in expected, &"wanted: {expected}, got: {lex}"
  lex.next()

func pop*(lex: var Lexer): OrgToken =
  result = lex.tokens[lex.pos]
  inc lex.pos

func pop*(
    lex: var Lexer,
    expected: OrgTokenKind | set[OrgTokenKind]): OrgToken =
  assert lex[] of expected, &"wanted: {expected}, got: {lex}"
  return lex.pop()

func hasNext*(lex: Lexer, offset: int = 0): bool =
  (lex.pos + offset) < lex.tokens.len

func `?`*(lex: Lexer): bool = hasNext(lex)

func `[]`*(lex: Lexer, kind: OrgTokenKind): bool =
  lex.hasNext(0) and lex[] == kind

func `[]`*(lex: Lexer, kind: set[OrgTokenKind]): bool =
  lex.hasNext(0) and lex[] in kind

func `[]`*(lex: Lexer, kind1, kind2: OrgTokenKind): bool =
  lex.hasNext(1) and lex[] == kind1 and lex[+1] == kind2

func `[]`*(lex: Lexer, kind1, kind2, kind3: OrgTokenKind): bool =
  lex.hasNext(2) and
  lex[+0] == kind1 and
  lex[+1] == kind2 and
  lex[+2] == kind3

func trySkip*(
  lex: var Lexer, expected: OrgTokenKind | set[OrgTokenKind]): bool =
  result = lex[expected]
  if result:
    lex.next()

func space*(lex: var Lexer) =
  while lex[OTkSpace]:
    lex.next()

func goto*(lex: var Lexer, pos: int) =
  assert 0 <= pos
  lex.pos = pos

func find*(lex: Lexer, kind: OrgTokenKind, limit: set[OrgTokenKind] = {}): int =
  ## Find next token of kind `kind` and return it's absolute position. If
  ## such token is not found until the end of the input or until one of
  ## token kinds from the `limit` is encountered, return `-1`
  result = -1
  for idx in lex.pos ..< lex.tokens.len:
    if lex.tokens[idx].kind in limit:
      return

    if lex.tokens[idx].kind == kind:
      return idx

func pop*(lex: var Lexer, slice: Slice[int]): seq[OrgToken] =
  result = lex.tokens[slice]
  lex.goto(slice.b + 1)

func pop*(lex: var Lexer, toPos: int): seq[OrgToken] =
  pop(lex, lex.pos .. toPos)

const
  orgKindMap = toMapArray {
    {OTkBoldOpen,      OTkBoldClose,      OTkBoldInline}:      orgBold,
    {OTkItalicOpen,    OTkItalicClose,    OTkItalicInline}:    orgItalic,
    {OTkVerbatimOpen,  OTkVerbatimClose,  OTkVerbatimInline}:  orgVerbatim,
    {OTkBacktickOpen,  OTkBacktickClose,  OTkBacktickInline}:  orgBacktick,
    {OTkUnderlineOpen, OTkUnderlineClose, OTkUnderlineInline}: orgUnderline,
    {OTkStrikeOpen,    OTkStrikeClose,    OTkStrikeInline}:    orgStrike,
    {OTkMonospaceOpen, OTkMonospaceClose, OTkmonospaceInline}: orgMonospace,
    {OTkQuoteOpen,     OTkQuoteClose,     otNone}:             orgQuote,
    {OTkAngleOpen,     OTkAngleClose,     otNone}:             orgPlaceholder
  }

  OTkOpenKinds = {
    OTkBoldOpen,
    OTkAngleOpen,
    OTkItalicOpen,
    OTkVerbatimOpen,
    OTkBacktickOpen,
    OTkUnderlineOpen,
    OTkStrikeOpen,
    OTkMonospaceOpen,
    OTkQuoteOpen
  }

  OTkCloseKinds = {
    OTkAngleClose,
    OTkBoldClose,
    OTkItalicClose,
    OTkVerbatimClose,
    OTkBacktickClose,
    OTkUnderlineClose,
    OTkStrikeClose,
    OTkMonospaceClose,
    OTkQuoteClose
  }

  OTkInlineKinds = {
    OTkBoldInline,
    OTkItalicInline,
    OTkVerbatimInline,
    OTkBacktickInline,
    OTkUnderlineInline,
    OTkStrikeInline,
    OTkMonospaceInline
  }




proc getInside(lex: var Lexer, start, finish: set[OrgTokenKind]): Lexer =
  var toks: seq[OrgToken]
  var count = 0
  while lex[] in start:
    lex.next()

  inc count

  while 0 < count:
    if lex[] in start:
      while lex[] in start:
        if 0 < count:
          result.tokens.add lex.pop()

        else:
          lex.next()

      inc count

    elif lex[] in finish:
      while ?lex and lex[] in finish:
        if 1 < count:
          result.tokens.add lex.pop()

        else:
          lex.next()

      dec count

    else:
      result.tokens.add lex.pop()

proc initLexer*(tokens: sink seq[OrgToken]): Lexer =
  result.tokens = tokens

proc parseText*(lex: var Lexer, parseConf: ParseConf): seq[OrgNode]
proc parseParagraph*(lex: var Lexer, parseConf: ParseConf): OrgNode
proc parseTop*(lex: var Lexer, parseConf: ParseConf): OrgNode

proc parseCSVArguments*(
    lex: var Lexer, parseConf: ParseConf): seq[OrgNode] =
  result.add newTree(orgIdent, lex.pop(OTkIdent))
  if lex[] == OTkParOpen:
    lex.skip(OTkParOpen)
    while lex[] == OTkRawText:
      result.add newTree(orgRawText, lex.pop(OTkRawText))
      if lex[] == OTkComma:
        lex.next()

    lex.skip(OTkParClose)

proc parseMacro*(lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  lex.skip(OTkMacroOpen)
  result = newTree(orgMacro, parseCSVArguments(lex, parseConf))
  lex.skip(OTkMacroClose)

proc parseRawUrl*(lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgRawLink, lex.pop(OTkRawUrl))

proc parseLink*(lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgLink)

  lex.skip(OTkLinkOpen)
  lex.skip(OTkLinkTargetOpen)
  if lex[OTkLinkInternal]:
    result.add newEmptyNode()
    result.add newTree(orgRawText, lex.pop(OTkLinkInternal))

  elif lex[OTkLinkFull]:
    result.add newEmptyNode()
    result.add newTree(orgRawText, lex.pop(OTkLinkFull))

  else:
    result.add newTree(orgIdent, lex.pop(OTkLinkProtocol))
    result.add newTree(orgRawText, lex.pop(OTkLinkTarget))

  lex.skip(OTkLinkTargetClose)
  if lex[] == OTkLinkDescriptionOpen:
    var sub = lex.getInside(
      {OTkLinkDescriptionOpen},
      {OTkLinkDescriptionClose})

    result.add newTree(orgParagraph, sub.parseText(parseConf))

  else:
    result.add newEmptyNode()

  lex.skip(OTkLinkClose)

proc parseInlineMath*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  ## Parse inline math expression, starting with any of `$`, `$$`, `\(`,
  ## and `\[`.
  let start = lex[]
  const
    regular = {OTkDollarOpen, OTkLatexParOpen}
    display = {OTkDoubleDollarOpen, OTkLatexBraceOpen}

  lex.skip(regular + display)

  let close =
    case start:
      of OTkDollarOpen: OTkDollarClose
      of OTkDoubleDollarOpen: OTkDoubleDollarClose
      of OTkLatexParOpen: OTkLatexParClose
      of OTkLatexBraceOpen: OTkLatexBraceClose
      else: raise newUnexpectedKindError(lex[])

  result = newTree(
    tern(start in regular, orgInlineMath, orgDisplayMath),
    newTree(orgRawText, lex.pop(OTkLatexInlineRaw)))

  lex.skip(close)

proc parseSymbol*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  lex.skip(OTkSymbolStart)
  result = newTree(orgSymbol, newTree(orgIdent, lex.pop(OTkIdent)))
  if lex[OTkMetaBraceOpen]:
    assert false

  else:
    result.add newEmptyNode()

  while lex[OTkMetaArgsOpen]:
    lex.skip(OTkMetaArgsOpen)
    # IMPLEMENT handle the arguments
    lex.skip(OTkMetaArgsBody)
    lex.skip(OTkMetaArgsClose)

proc parseHashtag*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(
    orgHashTag, newTree(orgRawText, lex.pop(OTkHashTag)))

  if lex[OTkHashTagSub]:
    lex.skip(OTkHashTagSub)
    if lex[OTkHashTag]:
      result.add(lex.parseHashTag(parseConf))

    else:
      lex.skip(OTkHashTagOpen)
      while ?lex and not lex[OTkHashTagClose]:
        result.add lex.parseHashTag(parseConf)
        if lex[OTkComma]:
          lex.next()

      lex.skip(OTkHashTagClose)

proc parseTime*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgTimeStamp, lex.pop({OTkBracketTime, OTkAngleTime}))
  if lex[OTkTimeDash]:
    lex.skip(OTkTimeDash)
    result = newTree(
      orgTimeRange,
      result,
      newTree(orgTimeStamp, lex.pop({OTkBracketTime, OTkAngleTime})))

    if lex[OTkTimeArrow]:
      lex.skip(OTkTimeArrow)
      result["diff"] = newTree(orgSimpleTime, lex.pop(OTkTimeDuration))


type
  TextStack = seq[seq[tuple[pending: bool, node: OrgNode]]]

proc getLayerOpen(stack: TextStack, tok: OrgToken): int =
  var layerOpen = -1
  for idx, layer in pairs(stack):
    if layer.len > 0 and
       layer.last().pending and
       layer.last().node.kind == orgKindMap[tok.kind]:
      layerOpen = idx + 1

  layerOpen

proc closeAllWith(
    stack: var TextStack,
    inLayerOpen: int,
    tok: OrgToken
  ) =
  # Force close all layers of parse stack, by moving nodes from several
  # layers into subnodes. This is used for explicitly handling closing
  # delimtiers.
  let layerOpen: int = inLayerOpen
  let foldTimes: int = stack.len - layerOpen
  var nodes: seq[OrgNode]
  for _ in 0 ..< foldTimes:
    nodes.add reversed(stack.pop.mapIt(it.node))

  for node in reversed(nodes):
    if node of orgMarkupKinds and node.len == 0:
      # TODO convert markup node not `Word` and set correc positional
      # information.
      stack.last2().node.add node

    else:
      stack.last2().node.add node

  stack.last2().pending = false


proc closeWith(stack: var TextStack, closeNode: OrgNode) =
  # Close last pending node in stack is there is any, otherwise move
  # current layer not lower one. Used for handling closing buffer nodes
  # /or/ delimiters. Cannot fold multiple layers of stack - only change
  # `pending` tag if needed.
  let layer = stack.pop
  if (stack.len > 0 and stack.last.len > 0 and stack.last2.pending):
    stack.last2.pending = false
    stack.last2.node.add layer.mapIt(it.node)

  elif stack.len == 0:
    stack.add @[layer]

  else:
    stack.last.add layer

proc pushWith(stack: var TextStack, newPending: bool, node: OrgNode) =
  # Add new node to parse stack. If last-last one (last layer, last node
  # in layer) is pending opening, add new layer, otherwise push to the
  # same layer. All pending nodes will be closed in `closeWith`.
  if (stack.last.len > 0 and stack.last2.pending):
    stack.add @[@[(newPending, node)]]

  else:
    stack.last.add (newPending, node)

proc pushClosed(stack: var TextStack, node: OrgNode) =
  pushWith(stack, false, node)

proc pushBuf(stack: var TextStack, buf: var seq[OrgToken]) =
  # If buffer is non-empty push it as new word. Most of the logic in this
  # proc is for dealing with whitespaces in buffers and separating
  # them into smaller things. For example `"buffer with space"` should be
  # handled as five different `Word`, instead of a single one.

  # Buffer is pushed before parsing each inline entry such as `$math$`,
  # `#tags` etc.
  if len(buf) > 0:
    for word in buf:
      let tree = case word.kind:
        of OTkNewline: newTree(orgNewline, word)
        of OTkRawText: newTree(orgRawText, word)
        of OTkEscaped: newTree(orgEscaped, word)
        of OTkBigIdent: newTree(orgBigIdent, word)
        of OTkSpace: newTree(orgSpace, word)
        of OTkParOpen, OTkParClose:
          newTree(orgPunctuation, word)
        else: newTree(orgWord, word)

      stack.pushClosed(tree)

    buf.clear()

proc parseInline(
    stack: var TextStack,
    buf: var seq[OrgToken],
    lex: var Lexer,
    parseConf: ParseConf
  ) =
  var hadPop = false
  case lex[]:
    of OTkOpenKinds:
      # Start of the regular, constrained markup section.
      # Unconditinally push new layer.
      stack.pushBuf(buf)
      stack.pushWith(true, newTree(orgKindMap[lex[]]))

    of OTkCloseKinds:
      # End of regular constrained section, unconditionally close
      # current layer, possibly with warnings for things like
      # `*/not-fully-italic*`
      stack.pushBuf(buf)
      let layer = stack.getLayerOpen(lex.get())
      if layer != -1:
        stack.closeAllWith(layer, lex.get())

      else:
        buf.add lex.pop()

    of OTkInlineKinds:
      # Detected unconstrained formatting block, will handle it
      # regardless.
      let layerOpen = stack.getLayerOpen(lex.get())
      let isOpening = layerOpen == -1

      if isOpening:
        # Push new markup opening, no verbatim currently active
        stack.pushWith(true, newTree(orgKindMap[lex[]]))
        lex.next()

      else:
        # Push new markup opening, no verbatim currently active
        stack.closeAllWith(layerOpen, lex.get())
        lex.next()

    else:
      hadPop = true
      buf.add lex.pop()

  if not hadPop:
    lex.next()

proc parseSrcInline*(lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  lex.skip(OTkSrcOpen)
  result = newTree(orgSrcInlineCode)
  result["lang"] = newTree(orgIdent, lex.pop(OTkSrcName))
  result["body"] = newTree(
    orgCodeLine, newTree(orgCodeText, lex.pop(OTkSrcBody)))

  lex.skip(OTkSrcClose)

proc parseText*(lex: var Lexer, parseConf: ParseConf): seq[OrgNode] =
  # Text parsing is implemented using non-recusive descent parser that
  # maintains stack explicitly (instead of constructing it via function
  # calls). This is made in order to provide support for stack
  # introspection at any given moment of parsing, and perform context-aware
  # decisions. Input lex is parsed *until the end* - i.e. you need to
  # always pass sublex.

  # TODO implement support for additional formatting options, delimited
  # pairs, and punctuation. `<placeholder>`, `(structured-punctuation)`.

  # TODO parse big idents - note that things like `MUST NOT`, `SHALL NOT`
  # need to be parsed as single node.
  var stack: TextStack
  var buf: seq[OrgToken]
  stack.add @[]

  while lex.hasNext():
    # More sophisticated heuristics should be used to detect edge cases
    # like `~/me~`, `*sentence*.` and others. Since particular details are
    # not fully fleshed out I will leave it as it is now, and concentrate
    # on other parts of the document.

    case lex[]:
      of OTkOpenKinds,
         OTkCloseKinds,
         OTkInlineKinds,
         OTkPunctuation,
         OTkWord,
         OTkRawText,
         OTkSpace,
         OTkBigIdent,
         OTkEscaped,
         OTkColon,
         OTkParOpen, OTkParClose,
         OTkNewline:
        stack.parseInline(buf, lex, parseConf)

      of OTkAngleTime:
        stack.pushClosed(parseTime(lex, parseConf))

      of OTkDollarOpen,
         OTkLatexParOpen,
         OTkLatexBraceOpen,
         OTkDoubleDollarOpen:
        stack.pushBuf(buf)
        stack.pushClosed(parseInlineMath(lex, parseConf))

      of OTkFootnoteStart:
        lex.skip(OTkFootnoteStart)
        var footnote: OrgNode
        if lex[OTkColon]:
          footnote = newTree(orgFootnote)
          lex.skip(OTkColon)
          footnote.add newTree(orgIdent, lex.pop())

        else:
          footnote = newTree(orgInlineFootnote)
          lex.skip(OTkDoubleColon)
          footnote.add parseParagraph(lex, parseConf)

        stack.pushClosed(footnote)

        lex.skip(OTkFootnoteEnd)

      of OTkSrcOpen:
        stack.pushClosed(parseSrcInline(lex, parseConf))

      of OTkBracketTime:
        stack.pushClosed lex.parseTime(parseConf)

      of OTkMacroOpen:
        stack.pushClosed lex.parseMacro(parseConf)

      of OTkLinkOpen:
        stack.pushClosed lex.parseLink(parseConf)

      of OTkRawUrl:
        stack.pushClosed lex.parseRawUrl(parseConf)

      of OTkSymbolStart:
        stack.pushClosed lex.parseSymbol(parseConf)

      of OTkAtMention:
        stack.pushClosed newTree(orgAtMention, lex.pop())

      of OTkHashTag:
        stack.pushClosed lex.parseHashtag(parseConf)

      of OTkDoubleAngleOpen:
        lex.skip(OTkDoubleAngleOpen)
        stack.pushClosed newTree(orgTarget, lex.pop(OTkRawText))
        lex.skip(OTkDoubleAngleClose)

      of OTkTripleAngleOpen:
        lex.skip(OTkTripleAngleOpen)
        stack.pushClosed newTree(orgRadioTarget, lex.pop(OTkRawText))
        lex.skip(OTkTripleAngleClose)

      else:
        raise newUnexpectedKindError(lex[], $lex)

  stack.pushBuf(buf)
  while stack.len > 1:
    stack.closeWith(newOrgEmptyNode())


  result = stack.first().mapIt(it.node)


proc parseToplevelItem*(lex: var Lexer, parseConf: ParseConf): OrgNode

proc parseTable*(lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgTable)

  lex.skip(OTkTableBegin)
  lex.skip(OTkCmdArguments) # TODO parse & handle properly
  result.add newEmptyNode()

  proc parseContent(lex: var Lexer): OrgNode =
    var sub = lex.getInside({OTkContentStart}, {OTkContentEnd})
    result = parseTop(sub, parseConf)

  # TODO parse cell and column parameters properly instead of skipping
  # everyting. Handle row annotations in the `#+row` version.
  while lex[] != OTkTableEnd:
    case lex[]:
      of OTkPipeOpen:
        var row = newTree(orgTableRow, newEmptyNode(), newEmptyNode())
        lex.skip(OTkPipeOpen)
        row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        while lex[] == OTkPipeSeparator:
          lex.skip(OTkPipeSeparator)
          row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))

        lex.skip(OTkPipeClose)
        result.add row

      of OTkPipeCellOpen:
        var row = newTree(orgTableRow, newEmptyNode(), newEmptyNode())
        lex.skip(OTkPipeCellOpen)
        row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        while lex[] == OTkPipeCellOpen:
          lex.skip(OTkPipeCellOpen)
          row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))

        result.add row

      of OTkRowSpec:
        var row = newTree(orgTableRow, newEmptyNode())
        lex.skip(OTkRowSpec)
        lex.skip(OTkCmdArguments) # TODO parse and handle properly
        row.add newEmptyNode()

        row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        while lex[] == OTkCellSpec:
          lex.skip(OTkCellSpec)
          lex.skip(OTkCmdArguments) # TODO parse content
          row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        result.add row

      of OTkTableEnd:
        discard

      else:
        assert false, $lex[]

  lex.skip(OTkTableEnd)



proc parseParagraph*(lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  var sub = lex.getInside({OTkParagraphStart}, {OTkParagraphEnd})
  result = newTree(orgParagraph, parseText(sub, parseConf))

proc parseCommandArguments*(
  lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =

  result = newTree(orgInlineStmtList)

  while lex[{OTkCommandValue, OTkCommandKey}]:
    if lex[OTkCommandKey]:
      result.add newTree(
        orgCmdValue,
        newTree(orgIdent, lex.pop(OTkCommandKey)),
        newTree(orgRawtext, lex.pop(OTkCommandValue))
      )

    else:
      result.add newTree(
        orgCmdValue,
        newEmptyNode(),
        newTree(orgRawText, lex.pop(OTkCommandValue))
      )


proc parseSrcArguments*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgCmdArguments)
  result["flags"] = newTree(orgInlineStmtList)
  while lex[OTkCommandFlag]:
    result["flags"].add newTree(orgCmdFlag, lex.pop())

  result["args"] = parseCommandArguments(lex, parseConf)


proc parseQuote*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgQuoteBlock)
  lex.skip(OTkCommandPrefix)
  lex.skip(OTkCommandBegin)
  lex.skip(OTkCommandArgumentsBegin)
  lex.skip(OTkCommandArgumentsEnd)
  lex.skip(OTkCommandContentStart)
  result.add parseParagraph(lex, parseConf)
  lex.skip(OTkCommandContentEnd)
  lex.skip(OTkCommandPrefix)
  lex.skip(OTkCommandEnd)

proc parseSrc*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgSrcCode)
  lex.skip(OTkCommandPrefix)
  lex.skip(OTkCommandBegin)

  block header_args_lang:
    lex.skip(OTkCommandArgumentsBegin)
    # TODO properly parse command block arguments
    let lang = lex.pop(OTkWord)
    result["lang"] = tern(
      lang.strVal().empty(),
      newEmptyNode(),
      newTree(orgIdent, lang)
    )

    result["header-args"] = parseSrcArguments(lex, parseConf)
    lex.skip(OTkCommandArgumentsEnd)

  block body:
    var stmt = newTree(orgStmtList)
    lex.skip(OTkCommandContentStart)
    lex.skip(OTkCodeContentBegin)
    while not lex[{OTkCommandContentEnd, OTkCodeContentEnd}]:
      var line = newTree(orgCodeLine)
      while not lex[{
        OTkCommandContentEnd,
        OTkNewline,
        OTkCodeContentEnd
      }]:
        case lex[]:
          of OTkCodeText:
            line.add newTree(orgCodeText, lex.pop(OTkCodeText))

          of OTkParOpen:
            # In-code callout annotation `(refs:name)` (represented
            # as multiple tokens - open/close pars, `refs:` ident and the name
            # itself. The OTkCodeCallout
            lex.skip(OTkParOpen)
            lex.skip(OTkIdent) # IDEA more inline elements in the code blocks?
            lex.skip(OTkColon)
            line.add newTree(
              orgCodeCallout, newTree(orgIdent, lex.pop(OTkIdent)))

            lex.skip(OTkParClose)

          of OTkCodeContentEnd:
            break

          of OTkDoubleAngleOpen:
            lex.skip(OTkDoubleAngleOpen)
            line.add newTree(
              orgCodeTangle, parseCSVArguments(lex, parseConf))
            lex.skip(OTkDoubleAngleClose)

          else:
            assert false, $lex

      if lex[OTkNewline]:
        lex.next()

      stmt.add line

    lex.skip(OTkCodeContentEnd)
    lex.skip(OTkCommandContentEnd)
    result.add stmt

  block eval_result:
    result.add newEmptyNode()
  
  lex.skip(OTkCommandPrefix)
  lex.skip(OTkCommandEnd)


proc parseNestedList*(lex: var Lexer, parseConf: ParseConf): OrgNode

proc parseListItemBody*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  ## Parse *remaining* parts of the list item into a statement list node.
  ## This procedure does not require a starting stmt list open token, and
  ## is used for both regular lists and logbook notes.
  result = newTree(orgStmtList)
  while not lex[OTkStmtListClose]:
    if lex[OTkIndent, OTkListDash]:
      lex.next()
      result.add parseNestedList(lex, parseConf)
      lex.skip(OTkDedent)

    else:
      result.add parseToplevelItem(lex, parseConf)

proc parseListItem*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  ## Recursively (handles nested list in body) parse a single list item
  ## starting from the list dash token.
  result = newTree(orgListItem)
  lex.skip(OTkListDash)

  block prefix:
    result.add newEmptyNode()

  block counter:
    result.add newEmptyNode()

  block checkbox:
    result.add newEmptyNode()

  block tag:
    if lex[OTkListDescOpen]:
      lex.skip(OTkListDescOpen)
      result["tag"] = parseParagraph(lex, parseConf)
      lex.skip(OTkListDescClose)
      lex.skip(OTkDoubleColon)

    else:
      result["tag"] = newEmptyNode()

  block header:
    result.add newEmptyNode()

  block completion:
    result.add newEmptyNode()

  block body_parse:
    # Parse list body elements until enclosing token is not found
    lex.skip(OTkStmtListOpen)
    result.add parseListItemBody(lex, parseConf)

  lex.skip(OTkStmtListClose)
  lex.skip(OTkListItemEnd)

proc parseNestedList*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newEmptiedTree(orgList)
  proc nextLevel(lex: var Lexer, parseConf: ParseConf): OrgNode =
    lex.skip(OTkIndent)
    result = parseNestedList(lex, parseConf)
    lex.skip(OTkDedent)

  while lex[OTkListDash]:
    result.add lex.parseListItem(parseConf)
    if lex[OTkSameIndent]:
      lex.next()

    elif lex[OTkDedent]:
      return

    elif lex[OTkIndent]:
      result[^1]["body"].add nextLevel(lex, parseConf)

    elif lex[OTkListEnd]:
      return

    else:
      assert false, $lex

proc parseList*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  lex.skip(OTkListStart)
  result = parseNestedList(lex, parseConf)
  lex.skip(OTkListEnd)

func strip*(
    tokens: seq[OrgToken],
    leading: set[OrgTokenKind],
    trailing: set[OrgTokenKind] = leading,
    skipLeading: set[OrgTokenKind] = {},
    skipTrailing: set[OrgTokenKind] = skipLeading
  ): seq[OrgToken] =
  ## Strip leading and traling tokens from the list

  var leftmost = 0
  var rightmost = tokens.high()
  while leftmost <= rightmost and
        tokens[leftmost] of skipLeading + leading:
    inc leftmost

  while leftmost <= rightmost and
        tokens[rightmost] of skipTrailing + trailing:
    dec rightmost

  for idx, token in tokens:
    if idx < leftmost:
      if token of skipLeading:
        result.add token

    elif rightmost < idx:
      if token of skipTrailing:
        result.add token

    else:
      result.add token

proc parseLogbookClockEntry*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  lex.skip(OTkParagraphStart)
  lex.space()
  result = newTree(orgLogbookClock)
  assert lex.pop(OTkBigIdent).strVal() == "CLOCK"
  lex.skip(OTkColon)
  lex.space()
  result.add parseTime(lex, parseConf)
  lex.space()
  lex.skip(OTkParagraphEnd)


proc parseLogbookListEntry*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  lex.skip(OTkListDash)
  # TODO parse list items
  let pos = lex.find(OTkDoubleSlash, {OTkListItemEnd})
  let head = tern(
    pos == -1,
    # Logbook item can be delimited by a double slash or have no
    # attached note at all, in which case list item is going to be
    # cut out fully.
    lex.pop(lex.find(OTkListItemEnd) - 1),
    lex.pop(pos))

  block head_parser:
    var lex = initLexer(head)
    lex.skip(OTkStmtListOpen)
    lex.skip(OTkParagraphStart)
    if lex[OTkWord] and lex.get().strVal() == "State":
      result = newTree(orgLogbookStateChange)
      lex.skip(OTkWord)
      lex.space()
      lex.skip(OTkQuoteOpen)
      result["newstate"] = newTree(orgBigIdent, lex.pop(OTkBigIdent))
      lex.skip(OTkQuoteClose)
      lex.space()
      assert lex.pop(OTkWord).strVal() == "from"
      lex.space()
      if lex[OTkQuoteOpen]:
        lex.skip(OTkQuoteOpen)
        result["oldstate"] = newTree(orgBigIdent, lex.pop(OTkBigIdent))
        lex.skip(OTkQuoteClose)
        lex.space()

      result["time"] = parseTime(lex, parseConf)

    elif lex[OTkWord] and lex.get().strVal() == "Refiled":
      result = newTree(orgLogbookRefile)
      doAssert lex.pop(OTkWord).strVal() == "Refiled"
      lex.space()
      doAssert lex.pop(OTkWord).strVal() == "on"
      lex.space()
      result["time"] = parseTime(lex, parseConf)
      lex.space()
      doAssert lex.pop(OTkWord).strVal() == "from"
      lex.space()
      result["from"] = parseLink(lex, parseConf)

    elif lex[OTkWord] and lex.get().strVal() == "Note":
      result = newTree(orgLogbookNote)
      doAssert lex.pop(OTkWord).strVal() == "Note"
      lex.space()
      doAssert lex.pop(OTkWord).strVal() == "taken"
      lex.space()
      doAssert lex.pop(OTkWord).strVal() == "on"
      lex.space()
      result["time"] = parseTime(lex, parseConf)
      lex.space()
      if lex[OTkDoubleSlash]:
        lex.skip(OTkDoubleSlash)

    else:
      assert false, $lex

  block body_parser:
    if pos == -1:
      result["text"] = newEmptyNode()
      # List item close token is handled by the `parseListItemBody` call in
      # the `else` branch, but if there is no body close element must be
      # handled manually here.
      lex.skip(OTkListItemEnd)

    else:
      var tokens = @[
        # Starting paragraph is split in the middle by the double
        # slash, and the ends need to be handled properly. Moving
        # this logic to the lexer won't work, because the double
        # slash is a regular text token and can appear anywhere.
        # `head` is going to be parsed separately and don't need to
        # be handled explicitly.
        initFakeTok(OTkParagraphStart)
        # List item parser only needs statement list close, so only
        # adding paragraph delimiter.
      ] & lex.pop(lex.find(OTkListItemEnd))
      tokens = tokens.strip(
        leading = {OTkNewline, OTkSpace},
        trailing = {OTkNewline, OTkSpace},
        skipLeading = {
          OTkStmtListOpen,
          OTkParagraphStart,
        },
        skipTrailing = {
          OTkStmtListClose,
          OTkParagraphEnd,
          OTkListItemEnd
        }
      )

      var sub = initLexer(tokens)

      result["text"] = parseListItemBody(sub, parseConf)


proc parseLogbook*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  lex.skip(OTkColonLogbook)
  lex.skip(OTkLogbookStart)
  result = newTree(orgLogbook)
  while not lex[OTkLogbookEnd]:
    if lex[OTkListStart]:
      lex.skip(OTkListStart)
      let indented = lex[OTkIndent]
      if indented: lex.skip(OTkIndent)
      while lex[OTkListDash]:
        result.add parseLogbookListEntry(lex, parseConf)
        discard lex.trySkip(OTkSameIndent)

      if indented: lex.skip(OTkDedent)
      lex.skip(OTkListEnd)

    elif lex[OTkParagraphStart, OTkSpace, OTkBigIdent] and
         lex.get(+2).strVal() == "CLOCK":
      result.add parseLogbookClockEntry(lex, parseConf)

    else:
      assert false, $lex

  lex.skip(OTkLogbookEnd)
  lex.skip(OTkColonEnd)


proc parseSubtree*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgSubtree)
  block prefix:
    result.add newTree(orgRawText, lex.pop(OTkSubtreeStars))

  block todo_status:
    if lex[OTkSubtreeTodoState]:
      result.add newTree(orgBigIdent, lex.pop(OTkSubtreeTodoState))

    else:
      result.add newEmptyNode()

  block urgency:
    # IMPLEMENT
    result.add newEmptyNode()

  block subtree_title:
    result.add parseParagraph(lex, parseConf)

  block subtree_completion:
    # IMPLEMENT
    result.add newEmptyNode()

  block tree_tags:
    # IMPLEMENT
    if lex[OTkSubtreeTag]:
      var tags = newTree(orgInlineStmtList)
      while lex[OTkSubtreeTag]:
        tags.add newTree(orgOrgTag, lex.pop(OTkSubtreeTag))

      result.add(tags)

    else:
      result.add newEmptyNode()

  block subtree_time:
    var times = newTree(orgStmtList)
    while lex[OTkSubtreeTime] or lex[OTkBracketTime]:
      var time = newTree(orgTimeAssoc)
      if lex[OTkSubtreeTime]:
        time.add newTree(orgBigIdent, lex.pop(OTkSubtreeTime))

      else:
        time.add newEmptyNode()

      time.add newTree(orgTimeStamp, lex.pop(OTkBracketTime))
      times.add time

    if times.len() == 0:
      result.add newEmptyNode()

    else:
      result.add times

  block tree_drawer:
    var drawer = newTree(orgDrawer, {
      "properties": newEmptyNode(),
      "logbook": newEmptyNode(),
      "description": newEmptyNode()
    })

    while lex[] in {
      OTkColonProperties,
      OTkColonLogbook,
      OTKColonDescription
    }:
      case lex[]:
        of OTkColonProperties:
          lex.skip(OTkColonProperties)
          var properties = newTree(orgPropertyList)

          while lex[{OTkColonIdent, OTkColonAddIdent}]:
            properties.add newTree(
              tern(lex[OTkColonIdent], orgProperty, orgPropertyAdd),
              newTree(orgRawText, lex.pop({OTkColonIdent, OTkColonAddIdent})),
              newEmptyNode(), # IMPLEMENT property subname handling
              newTree(orgRawText, lex.pop(OTkRawProperty))
            )

          lex.skip(OTkColonEnd)
          drawer["properties"] = properties

        of OTkColonLogbook:
          drawer["logbook"] = parseLogbook(lex, parseConf)

        of OTkColonDescription:
          lex.skip(OTkColonDescription)
          drawer["description"] = newTree(orgSubtreeDescription)
          drawer["description"]["text"] = parseParagraph(lex, parseConf)
          lex.skip(OTkColonEnd)

        else:
          discard

    result.add drawer

  block content:
    result.add newTree(orgStmtList)

  lex.skip(OTkSubtreeEnd)

proc skipLineCommand(lex: var Lexer) =
  lex.skip(OTkCommandPrefix)
  lex.skip(OTkLineCommand)
  lex.skip(OTkColon)

template inDelimiters(
    lex: var Lexer,
    start, finish: OrgTokenKind, body: untyped
): untyped =
  lex.skip(start)
  while ?lex and not lex[finish]:
    body

  lex.skip(finish)

template inCommandArguments(lex: var Lexer, body: untyped): untyped =
  inDelimiters(
    lex, OTkCommandArgumentsBegin, OTkCommandArgumentsEnd, body)

proc parseOrgFile*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  result = newTree(orgFile)
  if lex[OTkQuoteOpen]:
    lex.next()
    result.add newTree(orgRawText, lex.pop(OTkRawText))
    lex.skip(OTkQuoteClose)

  else:
    result.add newTree(orgRawText, lex.pop(OTkRawText))


proc parseLineCommand*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  let kind = classifyCommand(lex.get(+1).strVal())
  case kind:
    of ockInclude:
      lex.skipLineCommand()
      lex.skip(OTkCommandArgumentsBegin)
      result = newTree(orgCommandInclude)
      result.add parseOrgFile(lex, parseConf)
      if lex[OTkCommandValue]:
        result.add newTree(orgIdent, lex.pop(OTkCommandValue))

      else:
        result.add newEmptyNode()

      if lex[OTkCommandValue]:
        result.add newTree(orgIdent, lex.pop(OTkCommandValue))

      else:
        result.add newEmptyNode()

      result.add parseSrcArguments(lex, parseConf)

      lex.skip(OTkCommandArgumentsEnd)

    of ockAttrHtml:
      lex.skipLineCommand()
      lex.skip(OTkCommandArgumentsBegin)
      result = newTree(
        orgCommandAttrHtml, parseSrcArguments(lex, parseConf))

      lex.skip(OTkCommandArgumentsEnd)

    of ockTitle:
      lex.skipLineCommand()
      result = newTree(
        orgCommandTitle, parseParagraph(lex, parseConf))

    of ockCaption:
      lex.skipLineCommand()
      result = newTree(
        orgCommandCaption, parseParagraph(lex, parseConf))

    of ockCreator:
      lex.skipLineCommand()
      result = newTree(orgCommandCreator)
      inCommandArguments(lex):
        result.add newTree(orgRawText, lex.pop())

    of ockLanguage:
      lex.skipLineCommand()
      result = newTree(orgCommandLanguage)
      inCommandArguments(lex):
        result.add newTree(orgIdent, lex.pop())

    of ockAuthor:
      lex.skipLineCommand()
      result = newTree(orgCommandAuthor)
      inCommandArguments(lex):
        result.add newTree(orgRawText, lex.pop())

    of ockOptions:
      lex.skipLineCommand()
      result = newTree(orgCommandOptions)
      inCommandArguments(lex):
        result.add newTree(orgRawText, lex.pop())

    of ockFiletags:
      lex.skipLineCommand()
      result = newTree(orgFiletags)
      inCommandArguments(lex):
        result.add newTree(orgOrgTag, lex.pop(OtkSubtreeTag))

    of ockColumns:
      lex.skipLineCommand()
      result = newTree(orgColumns)
      inCommandArguments(lex):
        result.add newTree(orgRawText, lex.pop())

    of ockProperty:
      lex.skipLineCommand()
      inCommandArguments(lex):
        result = newTree(
          orgProperty,
          newTree(orgRawText, lex.pop(OTkIdent)),
          newEmptyNode(), # IMPLEMENT property subname handling
          newTree(orgRawText, lex.pop(OTkRawProperty))
        )


    else:
      raise newUnexpectedKindError(kind, $lex)


proc parseToplevelItem*(
    lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  ## Parse single toplevel entry from the input token stream - paragraph,
  ## list, table, subtree (not recursively), source code block, quote etc.
  case lex[]:
    of OTkParagraphStart:
      result = parseParagraph(lex, parseConf)

    of OTkTableBegin:
      result = parseTable(lex, parseConf)

    of OTkSubtreeStars:
      result = parseSubtree(lex, parseConf)

    of OTkListStart:
      result = parseList(lex, parseConf)

    of OTkTextSeparator:
      result = newTree(orgTextSeparator, lex.pop())

    of OTkCommandPrefix:
      case classifyCommand(lex.get(+1).strVal()):
        of ockBeginSrc:
          result = parseSrc(lex, parseConf)

        of ockBeginQuote:
          result = parseQuote(lex, parseConf)

        else:
          result = parseLineCommand(lex, parseConf)

    else:
      raise newUnexpectedKindError(lex[], $lex)


proc foldSubtrees(nodes: seq[OrgNode]): OrgNode =
  ## Fold the tree structure into the final document output.

  # Folding is done using simple recursive descent parser that treats flat
  # toplevel document nodes as tokens that need to be folded.
  var pos = 0
  proc tok(): OrgNode = nodes[pos]

  proc atLevel(): int =
    result = tok()["prefix"].strVal().len()

  proc aux(): OrgNode =
    if tok() of orgSubtree:
      result = tok()
      let currentLevel = atLevel()
      inc pos
      while pos < nodes.len():
        if tok() of orgSubtree:
          if currentLevel < atLevel():
            result["body"].add aux()

          else:
            return

        else:
          result["body"].add tok()
          inc pos

    else:
      result = tok()
      inc pos
    
  result = newTree(orgStmtList)
  while pos < nodes.len():
    result.add aux()


  

proc parseTop*(lex: var Lexer, parseConf: ParseConf): OrgNode {.parse.} =
  ## Parse a whole document from start to finish, recursively arranging
  ## nested subtrees.
  var collect: seq[OrgNode]
  while lex.hasNext():
    if lex[] of OTkComment:
      lex.next()

    else:
      let top = parseToplevelItem(lex, parseConf)
      collect.add(top)

  result = foldSubtrees(collect)



proc orgLex*(str: string): seq[OrgToken] =
  var str = initPosStr(str)
  return lexAll(str, lexGlobal())

proc orgParse*(
    tokens: seq[OrgToken],
    parseConf: ParseConf = defaultParseConf
  ): OrgNode =

  # for idx, tok in tokens:
  #   echov idx, hshow(tok)

  var lex = Lexer(tokens: tokens)
  result = parseTop(lex, parseConf)

proc orgParse*(
    str: string,
    parseConf: ParseConf = defaultParseConf
  ): OrgNode =

  orgParse(orgLex(str), parseConf)
