import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all

import
  std/[sequtils, algorithm]

import
  ../defs/[org_types, impl_org_node],
  ./parse_org_code

type
  OrgTextTokenKind* = enum
    ottNone

    ottBoldOpen, ottBoldClose, ottBoldInline
    ottItalicOpen, ottItalicClose, ottItalicInline
    ottVerbatimOpen, ottVerbatimClose, ottVerbatimInline
    ottMonospaceOpen, ottMonospaceClose, ottMonospaceInline
    ottBacktickOpen, ottBacktickClose, ottBacktickInline
    ottUnderlineOpen, ottUnderlineClose, ottUnderlineInline
    ottStrikeOpen, ottStrikeClose, ottStrikeInline
    ottQuoteOpen, ottQuoteClose
    ottAngleOpen, ottAngleClose,

    ottWord
    ottSpace
    ottBigIdent
    ottRawText
    ottInlineSrc ## Inline source code block: `src_nim[]{}`
    ottInlineCall

    ottDollarOpen ## Opening dollar inline latex math
    ottDollarClose ## Closing dollar for inline latex math
    ottLatexParOpen ## Opening `\(` for inline latex math
    ottLatexParClose ## Closing `\)` for inline latex math
    ottLatexBraceOpen ## Opening `\[` for inline display latex equation
    ottLatexBraceClose ## Closing `\]` for inline display latex equation

    ottDoubleAt ## Inline backend passthrough `@@`
    ottAtBracket ## Inline annotation
    ottAtMetaTag
    ottAtMention
    ottTagParams

    ottLink

    ottSlashEntry

    ottHashTag


    ottEof

  OrgTextToken* = HsTok[OrgTextTokenKind]
  OrgTextLexer* = HsLexer[OrgTextToken]

proc newTree*(kind: OrgNodeKind, tok: OrgTextToken): OrgNode =
  newTree(kind, initPosStr(tok))

const
  markupConfig = {
    '*': (ottBoldOpen,      ottBoldClose,      ottBoldInline),
    '/': (ottItalicOpen,    ottItalicClose,    ottItalicInline),
    '=': (ottVerbatimOpen,  ottVerbatimClose,  ottVerbatimInline),
    '`': (ottBacktickOpen,  ottBacktickClose,  ottBacktickInline),
    '_': (ottUnderlineOpen, ottUnderlineClose, ottUnderlineInline),
    '+': (ottStrikeOpen,    ottStrikeClose,    ottStrikeInline),
    '"': (ottQuoteOpen,     ottQuoteClose,     ottNone),
    '<': (ottAngleOpen,     ottAngleClose,     ottNone)
  }

  orgKindMap = toMapArray {
    {ottBoldOpen,      ottBoldClose,      ottBoldInline}:  orgBold,
    {ottItalicOpen,    ottItalicClose,    ottItalicInline}:  orgItalic,
    {ottVerbatimOpen,  ottVerbatimClose,  ottVerbatimInline}:  orgVerbatim,
    {ottBacktickOpen,  ottBacktickClose,  ottBacktickInline}:  orgBacktick,
    {ottUnderlineOpen, ottUnderlineClose, ottUnderlineInline}:  orgUnderline,
    {ottStrikeOpen,    ottStrikeClose,    ottStrikeInline}:  orgStrike,
    {ottQuoteOpen,     ottQuoteClose,     ottNone}:  orgQuote,
    {ottAngleOpen,     ottAngleClose,     ottNone}:  orgAngle,
  }

  markOpenKinds = markupConfig.mapIt(it[1][0]).toSet()
  markCloseKinds = markupConfig.mapIt(it[1][1]).toSet()
  markInlineKinds = markupConfig.mapIt(it[1][2]).toSet()

  markupTable = toMapArray markupConfig
  markupKeys = toKeySet markupConfig


proc lexText*(str: var PosStr): seq[OrgTextToken] =
  if not ?str:
    result.add str.initEof(ottEof)

  else:
    case str[]:
      of MaybeLetters:
        var allUp = true

        str.startSlice()
        while ?str and str[MaybeLetters]:
          if not str[HighAsciiLetters]:
            allUp = false

          str.advance()

        result.add str.initSliceTok(if allUp: ottBigIdent else: ottWord)

      of ' ':
        result.add str.initTok(str.popWhileSlice({' '}), ottSpace)

      of '~':
        if str[+1, '~']:
          result.add str.initTok(
            str.popPointSlice(advance = 2), ottMonospaceInline)

        if str[-1, ' '] or str.atStart():
          result.add str.initTok(str.popPointSlice(), ottMonospaceOpen)

        result.add str.initTok(str.popUntilSlice({'~'}), ottRawText)

        if str[+1, ' '] or str.beforeEnd():
          result.add str.initTok(str.popPointSlice(), ottMonospaceClose)

        else:
          result.add str.initTok(
            str.popPointSlice(advance = 2), ottMonospaceInline)


      of markupKeys:
        let ch = str[]
        let (kOpen, kClose, kInline) = markupTable[ch]

        if str[+1, ch]:
          result.add str.initTok(
            str.popPointSlice(advance = 2), kInline)

        elif str[-1, ' '] or str.atStart():
          result.add str.initTok(str.popPointSlice(), kOpen)

        elif str[+1, ' '] or str.beforeEnd():
          result.add str.initTok(str.popPointSlice(), kClose)

        else:
          raise newImplementError($str)

      else:
        raise newUnexpectedCharError(str)



using
  lexer: var OrgTextLexer
  parseConf: ParseConf

proc initTextLexer*(str: var PosStr): OrgTextLexer =
  initLexer(str, lexText)


proc parseBracket*(lexer, parseConf): OrgNode

proc parseAtEntry*(lexer, parseConf): OrgNode =
  ## Parse any entry starting with `@` sign - metatags, annotations, inline
  ## backend passes.
  when false:
    if lexer["@@"]:
      # Inline backend pass
      discard

    elif lexer["@["]:
      # Annotation start
      discard

    elif lexer[] == '@' and lexer[+1] in OIdentChars:
      # Metatag start OR random `@` in the text
      lexer.advance()
      let id = lexer.parseIdent(oskMetaTagIdent)
      if lexer[] == '[':
        result = newTree(
          orgMetaTag,
          id,
          newTree(
            orgRawText,
            oskMetaTagArgs,
            lexer.getInsideSimple('[', ']').toSlice(lexer))
        )

      elif lexer[] == '{':
        result = newTree(orgMetaTag, id)
        result.add newEmptyNode(oskMetatagArgs)

      else:
        raise newImplementError(lexer.error("22").msg)

      if id.strVal() == "import":
        var sub = lexer.getInsideBalanced('{', '}').newSublexer()
        var buf: PosStr
        result.add parseBracket(sub, parseConf, buf)

      else:
        result.add orgStmtList.newTree()
        while lexer[] == '{':
          result[^1].add newTree(
            orgRawText, oskMetatagText, lexer.getInsideBalanced('{', '}'))

    else:
      raise lexer.error("Expected @-entry")

proc parseSlashEntry*(lexer, parseConf): OrgNode =
  when false:
    assert lexer[] == '\\'
    if lexer[+1] in OIdentStartChars:
      var ahead = lexer
      ahead.advance()
      if ahead.allUntil(OIdentChars, OWhitespace + {'{', OEndOfFile}):
        lexer.advance()
        result = orgSymbol.newTree(
          lexer.getSkipWhile(OIdentChars).toSlice(lexer))

        if lexer["{}"]:
          lexer.advance(2)

      else:
        buf.add lexer.pop()
        buf.add lexer.pop()

    else:
      buf.add lexer.pop()
      buf.add lexer.pop()


proc parseBracket*(lexer, parseConf): OrgNode =
  ## Parse any square bracket entry starting at current lexer position, and
  ## return it.

  when false:
    if not lexer.isBalancedToEOL():
      buf.add lexer.pop()
      return

    var ahead = lexer
    if lexer[0..1] == "[[":
      # Link start
      result = orgLink.newTree()
      lexer.advance()
      result.add orgRawText.newTree(lexer.getInsideBalanced('[', ']'))
      if lexer[] == '[':
        result.add lexer.getInsideBalanced('[', ']').newSublexer().withResIt do:
          parseParagraph(it, parseConf)

      else:
        lexer.advance(2)
        result.add newEmptyNode()

      lexer.advance()

    elif lexer[] == '[':
      if lexer[+1 .. +3] == "fn:":
        var notelexer = lexer.getInsideBalanced(
          '[', ']').newSublexer()

        notelexer.advance(3)
        result = orgFootnote.newTree()
        if notelexer[] != ':':
          result.add notelexer.parseIdent()

        else:
          result.add newEmptyNode()

        notelexer.advance()

        result.add notelexer.parseParagraph(parseConf)


      else:
        const start = {'!', '>', '<', '*', '#', '?', '@'} + {'A' .. 'Z'}

        if lexer[+1] in start and
           lexer[+2] in start:
          let body = lexer.getInsideBalanced('[', ']').split('|')
          result = orgBracTag.newTree()
          for slice in body:
            result.add orgBareIdent.newTree(
              slice.toSlice(lexer).strip().toSlice(lexer))

        else:
          let body = lexer.getInsideBalanced('[', ']')
          result = orgTimeStamp.newTree(body)




proc parseMacro*(lexer, parseConf): OrgNode =
  when false:
    lexer.skipExpected("{{")
    result = orgMacro.newTree(
      orgRawText.newTree(
        lexer.getInsideBalanced('{', '}')))

    lexer.skipExpected("}}")


proc parseOptMacro*(lexer, parseConf): OrgNode =
  when false:
    case lexer.nextSet({'{'}, OBareIdentChars - {'{'}):
      of 0:
        discard lexer.getSkipUntil({'{'})
        if lexer["{{{"]:
          return orgResult.newTree(lexer.parseMacro(parseConf))

        else:
          return newEmptyNode()

      of 1:
        return newEmptyNode()


proc parseHashTag*(lexer, parseConf): OrgNode =
  when false:
    assert lexer[] == '#'
    lexer.advance()

    proc aux(lexer, parseConf): OrgNode =
      result = onkHashTag.newTree()
      # `#tag`
      result.add lexer.parseIdent()

      # `#tag##[sub1, sub2]`
      if lexer[0 .. 2] == "##[":
        lexer.advance(3)

        while lexer[] != ']':
          # TODO on broken tags this would cause compilation errors and/or
          # whole text getting dragged into single tag body.
          result.add aux(lexer, parseConf)
          lexer.skip()
          if lexer[] != ']':
            lexer.skipExpected(",")
            lexer.skip()

        lexer.advance()

      # `#tag##sub`
      elif lexer[0 .. 1] == "##":
        lexer.advance(2)
        result.add aux(lexer, parseConf)



    return aux(lexer, parseConf)

proc parseInlineMath*(lexer, parseConf): OrgNode =
  ## Parse inline math expression, starting with any of `$`, `$$`, `\(`,
  ## and `\[`.

  when false:
    assert lexer[] == '$'
    lexer.advance()
    result = orgMath.newTree(lexer.getSkipUntil({'$'}).toSlice(lexer))
    lexer.advance()




proc parseText*(lexer, parseConf): seq[OrgNode] =
  # Text parsing is implemented using non-recusive descent parser that
  # maintains stack explicitly (instead of constructing it via function
  # calls). This is made in order to provide support for stack
  # introspection at any given moment of parsing, and perform context-aware
  # decisions. Input lexer is parsed *until the end* - i.e. you need to
  # always pass sublexer.

  # TODO implement support for additional formatting options, delimited
  # pairs, and punctuation. `<placeholder>`, `(structured-punctuation)`.

  # TODO parse big idents - note that things like `MUST NOT`, `SHALL NOT`
  # need to be parsed as single node.
  var stack: seq[seq[tuple[pending: bool,
                           node: OrgNode]]]


  template getLayerOpen(tok: OrgTextToken): int =
    var layerOpen = -1
    for idx, layer in pairs(stack):
      if layer.len > 0 and
         layer.last().pending and
         layer.last().node.kind == orgKindMap[tok.kind]:
        layerOpen = idx + 1

    layerOpen


  template closeAllWith(inLayerOpen: int, tok: OrgTextToken): untyped =
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


  template closeWith(closeNode: OrgNode): untyped =
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

  template pushWith(newPending: bool, node: OrgNode): untyped =
    # Add new node to parse stack. If last-last one (last layer, last node
    # in layer) is pending opening, add new layer, otherwise push to the
    # same layer. All pending nodes will be closed in `closeWith`.
    if (stack.last.len > 0 and stack.last2.pending):
      stack.add @[@[(newPending, node)]]

    else:
      stack.last.add (newPending, node)

  var inVerbatim = false
  # FIXME account for different kinds of verbatim formatting - current
  # implementation will trigger no-verbatim mode for closing `~` after
  # opening `=`

  var buf: seq[OrgTextToken]
  template pushBuf(): untyped =
    # If buffer is non-empty push it as new word. Most of the logic in this
    # template is for dealing with whitespaces in buffers and separating
    # them into smaller things. For example `"buffer with space"` should be
    # handled as five different `Word`, instead of a single one.

    # Buffer is pushed before parsing each inline entry such as `$math$`,
    # `#tags` etc.
    if len(buf) > 0:
      for word in buf:
        pushWith(false, newTree(orgWord, word))

      buf.clear()


  stack.add @[]

  while ?lexer:
    # More sophisticated heuristics should be used to detect edge cases
    # like `~/me~`, `*sentence*.` and others. Since particular details are
    # not fully fleshed out I will leave it as it is now, and concentrate
    # on other parts of the document.

    case lexer[].kind:
      of markOpenKinds + markCloseKinds + markInlineKinds:
        var hadPop = false
        if lexer[markOpenKinds]:
          # Start of the regular, constrained markup section.
          # Unconditinally push new layer.
          pushBuf()
          pushWith(true, newTree(orgKindMap[lexer[].kind]))

        elif lexer[markCloseKinds]:
          # End of regular constrained section, unconditionally close
          # current layer, possibly with warnings for things like
          # `*/not-fully-italic*`
          hadPop = true
          buf.add lexer.pop()

        elif lexer[markInlineKinds]:
          # Detected unconstrained formatting block, will handle it
          # regardless.
          let layerOpen = getLayerOpen(lexer[])
          let isOpening = layerOpen == -1

          if isOpening:
            # Push new markup opening, no verbatim currently active
            pushWith(true, newTree(orgKindMap[lexer[].kind]))
            lexer.advance()

          else:
            # Push new markup opening, no verbatim currently active
            closeAllWith(layerOpen, lexer[])
            lexer.advance()

        else:
          hadPop = true
          buf.add lexer.pop()

        if not hadPop:
          lexer.advance()


      of ottDollarOpen, ottLatexParOpen:
        pushBuf()
        pushWith(false, parseInlineMath(lexer, parseConf))

      of ottDoubleAt, ottAtBracket, ottAtMetaTag, ottAtMention:
        pushBuf()
        pushWith(false, parseAtEntry(lexer, parseConf))

      of ottHashTag:
        pushBuf()
        pushWith(false, parseHashTag(lexer, parseConf))

      of ottLink:
        pushBuf()
        let node = parseBracket(lexer, parseConf)

      of ottSlashEntry:
        let node = lexer.parseSlashEntry(parseConf)
        if not node.isNil:
          pushWith(false, node)

      of ottWord:
        buf.add lexer.pop()

      of ottInlineSrc:
        pushWith(
          false,
          lexer.popAsStr().asVar().initCodeLexer().asVar().parseSrcInline(parseConf))

      of ottInlineCall:
        pushWith(
          false,
          lexer.popAsStr().asVar().initCodeLexer().asVar().parseCallInline(parseConf))

      else:
        raise newUnexpectedTokenError(lexer)

  pushBuf()
  while stack.len > 1:
    closeWith(newOrgEmptyNode())

  return stack[0].mapIt(it.node)



proc getLastLevel(node: var OrgNode, level: int): var OrgNode =
  when false:
    case level:
      of 0: return node
      of 1: return node[^1]
      of 2: return node[^1][^1]
      of 3: return node[^1][^1][^1]
      else: return getLastLevel(node, level - 4)

proc getLastLevel(node: OrgNode, level: int): OrgNode =
  when false:
    case level:
      of 0: node
      of 1: node[^1]
      else: getLastLevel(node, level - 2)


    # lexer.parseIdent()

proc parseAngleEntry*(lexer, parseConf; buf: var PosStr): OrgNode =
  when false:
    if not lexer.isBalancedToEOL():
      buf.add lexer.pop()

    else:
      var lexer = lexer.getInsideBalanced('<', '>').newSublexer()
      if lexer[0] == '+' and lexer[^1] == '+':
        lexer.advance()
        var buf = lexer.initEmptyStrRanges()
        while not lexer.atEnd():
          buf.add lexer.pop()

        buf.pop()

        var textlexer = newSublexer(buf.toSlice(lexer))
        result = orgPlaceholder.newTree(
          textlexer.parseParagraph(parseConf))

      elif lexer[0] == '<' and lexer[^1] == '>':
        lexer.advance()
        var buf = lexer.initStrRanges()
        while not lexer.atEnd():
          buf.add lexer.pop()

        buf.pop()

        result = orgRadioTarget.newTree(
          orgRawText.newTree(buf.toSlice(lexer)))

      else:
        result = orgPlaceholder.newTree(lexer.parseParagraph(parseConf))

proc parseText*(str: PosStr, conf: ParseConf): OrgNode =
  var str = str
  var lexer = initLexer(str, lexText)
  result = newTree(orgParagraph, parseText(lexer, conf))
