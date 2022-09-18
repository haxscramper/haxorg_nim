import
  haxorg/[
    enum_types,
    types,
    lexer
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


type
  Lexer = object
    tokens*: seq[OrgToken]
    pos*: int


func hShow*(e: OrgToken, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  result = "(" & hshow(e.kind, opts)

  if not e.strVal().empty():
    result &= " "
    result &= hshow(e.strVal(), opts)

  result &= ")"

func hShow*(e: Lexer, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  result = hshow(e.pos, opts)
  result &= " "
  for tok in e.pos .. min(e.tokens.high, e.pos + 5):
    result &= " "
    result &= hshow(e.tokens[tok], opts)

func `$`*(e: Lexer): string = $hshow(e)

func `[]`*(lex: Lexer, offset: int = 0): OrgTokenKind =
  lex.tokens[lex.pos + offset].kind

func get*(lex: Lexer): OrgToken = lex.tokens[lex.pos]

func next*(lex: var Lexer) =
  inc lex.pos

func skip*(lex: var Lexer, expected: OrgTokenKind) =
  assert lex[] == expected, &"wanted: {expected}, got: {lex[]}"
  lex.next()

func skip*(lex: var Lexer, expected: set[OrgTokenKind]) =
  assert lex[] in expected, &"wanted: {expected}, got: {lex[]}"
  lex.next()

func pop*(lex: var Lexer): OrgToken =
  result = lex.tokens[lex.pos]
  inc lex.pos

func pop*(lex: var Lexer, expected: OrgTokenKind): OrgToken =
  assert lex[] == expected, &"wanted: {expected}, got: {lex[]}"
  return lex.pop()

func hasNext*(lex: Lexer, offset: int = 0): bool =
  (lex.pos + offset) < lex.tokens.len

func `?`*(lex: Lexer): bool = hasNext(lex)

func `[]`*(lex: Lexer, kind: OrgTokenKind): bool =
  lex.hasNext(0) and lex[] == kind

func `[]`*(lex: Lexer, kind1, kind2: OrgTokenKind): bool =
  lex.hasNext(1) and lex[] == kind1 and lex[+1] == kind2

const
  orgKindMap = toMapArray {
    {OTxBoldOpen,      OTxBoldClose,      OTxBoldInline}:      orgBold,
    {OTxItalicOpen,    OTxItalicClose,    OTxItalicInline}:    orgItalic,
    {OTxVerbatimOpen,  OTxVerbatimClose,  OTxVerbatimInline}:  orgVerbatim,
    {OTxBacktickOpen,  OTxBacktickClose,  OTxBacktickInline}:  orgBacktick,
    {OTxUnderlineOpen, OTxUnderlineClose, OTxUnderlineInline}: orgUnderline,
    {OTxStrikeOpen,    OTxStrikeClose,    OTxStrikeInline}:    orgStrike,
    {OTxMonospaceOpen, OTxMonospaceClose, OTxmonospaceInline}: orgMonospace,
    {OTxQuoteOpen,     OTxQuoteClose,     otNone}:             orgQuote,
  }

  OTxOpenKinds = {
    OTxBoldOpen,
    OTxItalicOpen,
    OTxVerbatimOpen,
    OTxBacktickOpen,
    OTxUnderlineOpen,
    OTxStrikeOpen,
    OTxMonospaceOpen,
    OTxQuoteOpen
  }

  OTxCloseKinds = {
    OTxBoldClose,
    OTxItalicClose,
    OTxVerbatimClose,
    OTxBacktickClose,
    OTxUnderlineClose,
    OTxStrikeClose,
    OTxMonospaceClose,
    OTxQuoteClose
  }

  OTxInlineKinds = {
    OTxBoldInline,
    OTxItalicInline,
    OTxVerbatimInline,
    OTxBacktickInline,
    OTxUnderlineInline,
    OTxStrikeInline,
    OTxMonospaceInline
  }




proc getInside(lex: var Lexer, start, finish: set[OrgTokenKind]): Lexer =
  while lex[] in start: lex.next()
  while lex[] notin finish: result.tokens.add lex.pop()
  while ?lex and lex[] in finish: lex.next()


proc parseText*(lex: var Lexer, parseConf: ParseConf): seq[OrgNode]
proc parseParagraph(lex: var Lexer, parseConf: ParseConf): OrgNode
proc parseTop(lex: var Lexer, parseConf: ParseConf): OrgNode

proc parseMacro*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = newTree(orgMacro)
  lex.skip(OTxMacroOpen)
  result.add newTree(orgIdent, lex.pop(OTxMacroName))
  if lex[] == OTxParOpen:
    lex.skip(OTxParOpen)
    while lex[] == OTxMacroArg:
      result.add newTree(orgRawText, lex.pop(OTxMacroArg))
      if lex[] == OTxComma:
        lex.next()

    lex.skip(OTxParClose)

  lex.skip(OTxMacroClose)

proc parseLink*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = newTree(orgLink)

  lex.skip(OTxLinkOpen)
  lex.skip(OTxLinkTargetOpen)
  result.add newTree(orgIdent, lex.pop(OTxLinkProtocol))
  result.add newTree(orgRawText, lex.pop(OTxLinkTarget))
  lex.skip(OTxLinkTargetClose)
  if lex[] == OTxLinkDescriptionOpen:
    var sub = lex.getInside(
      {OTxLinkDescriptionOpen},
      {OTxLinkDescriptionClose})

    result.add newTree(orgParagraph, sub.parseText(parseConf))

  else:
    result.add newEmptyNode()

  lex.skip(OTxLinkClose)

proc parseInlineMath*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  ## Parse inline math expression, starting with any of `$`, `$$`, `\(`,
  ## and `\[`.
  let start = lex[]
  const
    regular = {OtxDollarOpen, OTxLatexParOpen}
    display = {OTxDoubleDollarOpen, OTxLatexBraceOpen}

  lex.skip(regular + display)

  let close =
    case start:
      of OTxDollarOpen: OTxDollarClose
      of OTxDoubleDollarOpen: OTxDoubleDollarClose
      of OTxLatexParOpen: OTxLatexParClose
      of OTxLatexBraceOpen: OTxLatexBraceClose
      else: raise newUnexpectedKindError(lex[])

  result = newTree(
    tern(start in regular, orgInlineMath, orgDisplayMath),
    newTree(orgRawText, lex.pop(OTxLatexInlineRaw)))

  lex.skip(close)

proc parseSymbol*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  lex.skip(OTxSymbolStart)
  result = newTree(orgSymbol, newTree(orgIdent, lex.pop(OTxIdent)))
  if lex[OTxMetaBraceOpen]:
    assert false

  else:
    result.add newEmptyNode()

  while lex[OTxMetaArgsOpen]:
    lex.skip(OTxMetaArgsOpen)
    # IMPLEMENT handle the arguments
    lex.skip(OTxMetaArgsBody)
    lex.skip(OTxMetaArgsClose)

proc parseHashtag*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = newTree(
    orgHashTag, newTree(orgRawText, lex.pop(OTxHashTag)))

  if lex[OTxHashTagSub]:
    lex.skip(OTxHashTagSub)
    if lex[OTxHashTag]:
      result.add(lex.parseHashTag(parseConf))

    else:
      lex.skip(OTxHashTagOpen)
      while ?lex and not lex[OTxHashTagClose]:
        result.add lex.parseHashTag(parseConf)
        if lex[OTxComma]:
          lex.next()

      lex.skip(OTxHashTagClose)

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
        of OTxNewline: newTree(orgNewline, word)
        of OTxRawText: newTree(orgRawText, word)
        of OTxEscaped: newTree(orgEscaped, word)
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
    of OTxOpenKinds:
      # Start of the regular, constrained markup section.
      # Unconditinally push new layer.
      stack.pushBuf(buf)
      stack.pushWith(true, newTree(orgKindMap[lex[]]))

    of OTxCloseKinds:
      # End of regular constrained section, unconditionally close
      # current layer, possibly with warnings for things like
      # `*/not-fully-italic*`
      stack.pushBuf(buf)
      let layer = stack.getLayerOpen(lex.get())
      if layer != -1:
        stack.closeAllWith(layer, lex.get())

      else:
        buf.add lex.pop()

    of OTxInlineKinds:
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
      of OTxOpenKinds,
         OTxCloseKinds,
         OTxInlineKinds,
         OTxWord,
         OTxRawText,
         OTxSpace,
         OTxBigIdent,
         OTxEscaped,
         OTxNewline:
        stack.parseInline(buf, lex, parseConf)

      of OTxDollarOpen,
         OTxLatexParOpen,
         OTxLatexBraceOpen,
         OTxDoubleDollarOpen:
        stack.pushBuf(buf)
        stack.pushClosed(parseInlineMath(lex, parseConf))

      # of OTxDoubleAt, OTxAtBracket, OTxAtMetaTag, OTxAtMention:
      #   stack.pushBuf(buf)
      #   stack.pushClosed(parseAtEntry(lex, parseConf))

      # of OTxHashTag:
      #   stack.pushBuf(buf)
      #   stack.pushClosed(parseHashTag(lex, parseConf))

      # of OTxSlashEntry:
      #   let node = lex.parseSlashEntry(parseConf)
      #   if not node.isNil:
      #     stack.pushClosed(node)

      # of OTxInlineSrc:
      #   stack.pushClosed(
      #     lex.popAsStr().initCodeLexer().asVar().parseSrcInline(parseConf))

      # of OTxInlineCall:
      #   stack.pushClosed(
      #     lex.popAsStr().initCodeLexer().asVar().parseCallInline(parseConf))

      # of OTxTargetOpen:
      #   lex.skip(OTxTargetOpen)
      #   stack.pushClosed(
      #     newTree(orgTarget, newTree(orgRawText, lex.popAsStr({OTxRawText}))))

      #   lex.skip(OTxTargetClose)

      # of OTxRadioTargetOpen:
      #   lex.skip(OTxRadioTargetOpen)
      #   stack.pushClosed(newTree(orgRadioTarget,
      #     newTree(orgRawText, lex.popAsStr({OTxRawText}))))

      #   lex.skip(OTxRadioTargetClose)

      of OTxMacroOpen:
        stack.pushClosed lex.parseMacro(parseConf)

      of OTxLinkOpen:
        stack.pushClosed lex.parseLink(parseConf)

      of OTxSymbolStart:
        stack.pushClosed lex.parseSymbol(parseConf)

      of OTxAtMention:
        stack.pushClosed newTree(orgAtMention, lex.pop())

      of OTxHashTag:
        stack.pushClosed lex.parseHashtag(parseConf)

      else:
        raise newUnexpectedKindError(lex[])

  stack.pushBuf(buf)
  while stack.len > 1:
    stack.closeWith(newOrgEmptyNode())


  result = stack.first().mapIt(it.node)


proc parseTable(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = newTree(orgTable)

  lex.skip(OTbTableBegin)
  lex.skip(OTbCmdArguments) # TODO parse & handle properly
  result.add newEmptyNode()

  proc parseContent(lex: var Lexer): OrgNode =
    var sub = lex.getInside({OTbContentStart}, {OTbContentEnd})
    result = parseTop(sub, parseConf)

  # TODO parse cell and column parameters properly instead of skipping
  # everyting. Handle row annotations in the `#+row` version.
  while lex[] != OTbTableEnd:
    case lex[]:
      of OTbPipeOpen:
        var row = newTree(orgTableRow, newEmptyNode(), newEmptyNode())
        lex.skip(OTbPipeOpen)
        row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        while lex[] == OTbPipeSeparator:
          lex.skip(OTbPipeSeparator)
          row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))

        lex.skip(OTbPipeClose)
        result.add row

      of OTbPipeCellOpen:
        var row = newTree(orgTableRow, newEmptyNode(), newEmptyNode())
        lex.skip(OTbPipeCellOpen)
        row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        while lex[] == OTbPipeCellOpen:
          lex.skip(OTbPipeCellOpen)
          row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))

        result.add row

      of OTbRowSpec:
        var row = newTree(orgTableRow, newEmptyNode())
        lex.skip(OTbRowSpec)
        lex.skip(OTbCmdArguments) # TODO parse and handle properly
        row.add newEmptyNode()

        row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        while lex[] == OTbCellSpec:
          lex.skip(OTbCellSpec)
          lex.skip(OTbCmdArguments) # TODO parse content
          row.add newTree(orgTableCell, newEmptyNode(), parseContent(lex))
        result.add row

      of OTbTableEnd:
        discard

      else:
        assert false, $lex[]

  lex.skip(OTbTableEnd)



proc parseParagraph(lex: var Lexer, parseConf: ParseConf): OrgNode =
  var sub = lex.getInside({OTxParagraphStart}, {OTxParagraphEnd})
  result = newTree(orgParagraph, parseText(sub, parseConf))

proc parseTop(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = newTree(orgStmtList)
  while lex.hasNext():
    case lex[]:
      of OTxParagraphStart:
        result.add parseParagraph(lex, parseConf)

      of OTbTableBegin:
        result.add parseTable(lex, parseConf)

      of otEof:
        lex.next()

      else:
        raise newUnexpectedKindError(lex[])

proc orgLex*(str: string): seq[OrgToken] =
  var str = initPosStr(str)
  return lexAll(str, lexGlobal())

proc orgParse*(
    tokens: seq[OrgToken],
    parseConf: ParseConf = defaultParseConf
  ): OrgNode =

  var lex = Lexer(tokens: tokens)
  result = parseTop(lex, parseConf)

proc orgParse*(
    str: string,
    parseConf: ParseConf = defaultParseConf
  ): OrgNode =

  orgParse(orgLex(str), parseConf)
