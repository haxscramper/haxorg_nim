import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all

import
  std/[sequtils, algorithm, parseutils]

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

    ottPlaceholderOpen, ottPlaceholderClose
    ottTargetOpen, ottTargetClose
    ottRadioTargetOpen, ottRadioTargetClose

    ottLinkOpen, ottLinkClose
    ottLinkTargetOpen, ottLinkTargetClose
    ottLinkDescriptionOpen, ottLinkDescriptionClose
    ottLinkTarget

    ottWord
    ottMaybeWord
    ottSpace
    ottBigIdent
    ottRawText
    ottInlineSrc ## Inline source code block: `src_nim[]{}`
    ottInlineCall

    ottDollarOpen ## Opening dollar inline latex math
    ottDollarClose ## Closing dollar for inline latex math
    ottDoubleDollarOpen
    ottDoubleDollarClose
    ottLatexParOpen ## Opening `\(` for inline latex math
    ottLatexParClose ## Closing `\)` for inline latex math
    ottLatexBraceOpen ## Opening `\[` for inline display latex equation
    ottLatexBraceClose ## Closing `\]` for inline display latex equation
    ottLatexInlineRaw

    ottDoubleAt ## Inline backend passthrough `@@`
    ottAtBracket ## Inline annotation
    ottAtMetaTag
    ottAtMention
    ottTagParams

    ottLink

    ottSlashEntry

    ottHashTag

    ottMacroOpen, ottMacroBody, ottMacroClose

    ottSrcOpen, ottSrcName, ottSrcArgs, ottSrcBody, ottSrcClose

    ottCallOpen, ottCallName, ottCallInsideHeader,
    ottCallArgs, ottEndHeader, ottCallClose


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
  }

  orgKindMap = toMapArray {
    {ottBoldOpen,      ottBoldClose,      ottBoldInline}:  orgBold,
    {ottItalicOpen,    ottItalicClose,    ottItalicInline}:  orgItalic,
    {ottVerbatimOpen,  ottVerbatimClose,  ottVerbatimInline}:  orgVerbatim,
    {ottBacktickOpen,  ottBacktickClose,  ottBacktickInline}:  orgBacktick,
    {ottUnderlineOpen, ottUnderlineClose, ottUnderlineInline}:  orgUnderline,
    {ottStrikeOpen,    ottStrikeClose,    ottStrikeInline}:  orgStrike,
    {ottQuoteOpen,     ottQuoteClose,     ottNone}:  orgQuote,
  }

  markOpenKinds = markupConfig.mapIt(it[1][0]).toSet()
  markCloseKinds = markupConfig.mapIt(it[1][1]).toSet()
  markInlineKinds = markupConfig.mapIt(it[1][2]).toSet()

  markupTable = toMapArray markupConfig
  markupKeys = toKeySet markupConfig



proc lexText*(str: var PosStr): seq[OrgTextToken]

proc lexBracket*(str: var PosStr): seq[OrgTextToken] =
  let start = str.getPos()
  result.add str.scanTok(ottLinkOpen, '[')

  block link_token:
    result.add str.scanTok(ottLinkTargetOpen, '[')
    var target: PosStr = str.asSlice str.skipUntil({']'})
    result.add str.initTok(target, ottRawText)
    result.add str.scanTok(ottLinkTargetClose, ']')


  block description_token:
    if str['[']:
      result.add str.scanTok(ottLinkDescriptionOpen, '[')
      var desc: PosStr = str.asSlice str.skipUntil({']'})
      result.add lexText(desc)
      result.add str.scanTok(ottLinkDescriptionClose, ']')

  result.add str.scanTok(ottLinkClose, ']')



proc lexText*(str: var PosStr): seq[OrgTextToken] =
  const TextChars = MaybeLetters + Digits + { '.', ',' }

  if not ?str:
    result.add str.initEof(ottEof)

  else:
    case str[]:
      of TextChars:
        var isStructure: bool = false

        if str["src"]:
          let pos = str.getPos()
          var buf: seq[OrgTextToken]
          buf.add str.initTok(str.asSlice str.skip("src"), ottSrcOpen)
          if str[{'_', '-'}]:
            str.next()

          if str[IdentStartChars]:
            result.add buf

            result.add str.initTok(str.asSlice str.skipWhile(IdentChars), ottSrcName)
            result.add str.initTok(
              str.asSlice(str.skipBalancedSlice({'{'}, {'}'}), rightShift = -2, leftShift = 1),
              ottSrcBody
            )

            result.add str.initTok(ottSrcClose)
            isStructure = true

          else:
            str.setPos(pos)


        elif str["call"]:
          echov str
          raise newImplementError(str)


        if not isStructure:
          var allUp = true

          str.startSlice()
          while ?str and str[TextChars + {'-'}]:
            if not str[HighAsciiLetters]:
              allUp = false

            str.next()

          result.add str.initSliceTok(if allUp: ottBigIdent else: ottWord)

      of ' ':
        result.add str.initTok(str.popWhileSlice({' '}), ottSpace)

      of '#':
        str.startSlice()
        str.skip('#')
        str.skipWhile(IdentChars)
        if str['#']:
          str.skip({'#'}, {'#'})
          if str[IdentChars]:
            str.skip(IdentChars)
            str.skipWhile(IdentChars)

          else:
            str.skipBalancedSlice(
              {'['}, {']'},
              endChars = TextLineChars - IdentChars - {' '})

        result.add str.initTok(str.popSlice(), ottHashTag)

      of '$', '\\':
        if str['$']:
          if str[+1, '$']:
            result.add str.scanTok(ottDollarOpen, '$', '$')
            str.startSlice()
            var hasEnd = false
            while ?str and not hasEnd:
              while ?str and not str['$']:
                str.next()

              if str['$', '$']:
                result.add str.initTok(str.popSlice(), ottLatexInlineRaw)
                hasEnd = true

              else:
                raise newImplementError()

            result.add str.scanTok(ottDollarClose, '$', '$')

          else:
            result.add str.scanTok(ottDollarOpen, '$')
            result.add str.initTok(
              str.asSlice str.skipUntil({'$'}),
              ottLatexInlineRaw)

            result.add str.scanTok(ottDollarClose, '$')


        elif str[+1, {'[', '('}]:
          raise newImplementError()

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

      of '<':
        if str['<', '<', '<']:
          result.add str.initAdvanceTok(3, ottRadioTargetOpen)

          # TODO More sophisicated lexer that checks for `>>` and `>``
          result.add str.initTok(str.asSlice str.skipUntil({ '>' }), ottRawText)
          result.add str.initTok(
            str.asSlice((str.skip('>'); str.skip('>'); str.skip('>'))),
            ottRadioTargetClose)


        elif str['<', '<']:
          result.add str.initAdvanceTok(2, ottTargetOpen)
          result.add str.initTok(str.asSlice str.skipUntil({ '>' }), ottRawText)
          result.add str.initTok(str.asSlice((str.skip('>'); str.skip('>'))), ottTargetClose)

        else:
          result.add str.initAdvanceTok(1, ottPlaceholderOpen)
          result.add str.initTok(str.asSlice str.skipUntil({ '>' }), ottRawText)
          result.add str.initTok(str.asSlice str.skip('>'), ottPlaceholderClose)


      of markupKeys - { '<' }:
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
          raise newImplementError($str & " " & $str.getPos())

      of '[':
        result = lexBracket(str)

      of '{':
        if str["{{{"]:
          result.add str.initAdvanceTok(3, ottMacroOpen)
          result.add str.initTok(
            asSlice(str, inWhile(?str and not str["}}}"], str.next())),
            ottMacroBody
          )

          if ?str:
            result.add str.initAdvanceTok(3, ottMacroClose)


        else:
          result.add str.initAdvanceTok(1, ottMaybeWord)

      else:
        raise newUnexpectedCharError(str)



using
  lexer: var OrgTextLexer
  parseConf: ParseConf

proc initTextLexer*(str: var PosStr): OrgTextLexer =
  initLexer(str, lexText)


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
  newTree(orgIdent, lexer.popAsStr())

proc parseInlineMath*(lexer, parseConf): OrgNode =
  ## Parse inline math expression, starting with any of `$`, `$$`, `\(`,
  ## and `\[`.

  let close =
    case lexer[].kind:
      of ottDollarOpen: ottDollarClose
      of ottDoubleDollarOpen: ottDoubleDollarClose
      of ottLatexParOpen: ottLatexParClose
      of ottLatexBraceOpen: ottLatexBraceClose
      else: raise newUnexpectedTokenError(lexer)

  lexer.next()
  result = newTree(orgMath, lexer.popAsStr({ottLatexInlineRaw}))
  lexer.skip({close})






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

  template pushClosed(node: OrgNode): untyped =
    pushWith(false, node)

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
      of markOpenKinds + markCloseKinds + markInlineKinds,
         ottWord, ottSpace, ottBigIdent:
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
          pushBuf()
          let layer = getLayerOpen(lexer[])
          if layer != -1:
            closeAllWith(layer, lexer[])

          else:
            buf.add lexer.pop()

        elif lexer[markInlineKinds]:
          # Detected unconstrained formatting block, will handle it
          # regardless.
          let layerOpen = getLayerOpen(lexer[])
          let isOpening = layerOpen == -1

          if isOpening:
            # Push new markup opening, no verbatim currently active
            pushWith(true, newTree(orgKindMap[lexer[].kind]))
            lexer.next()

          else:
            # Push new markup opening, no verbatim currently active
            closeAllWith(layerOpen, lexer[])
            lexer.next()

        else:
          hadPop = true
          buf.add lexer.pop()

        if not hadPop:
          lexer.next()


      of ottDollarOpen, ottLatexParOpen:
        pushBuf()
        pushClosed(parseInlineMath(lexer, parseConf))

      of ottDoubleAt, ottAtBracket, ottAtMetaTag, ottAtMention:
        pushBuf()
        pushClosed(parseAtEntry(lexer, parseConf))

      of ottHashTag:
        pushBuf()
        pushClosed(parseHashTag(lexer, parseConf))

      # of ottLink:
      #   pushBuf()
      #   pushClosed parseBracket(lexer, parseConf)

      of ottSlashEntry:
        let node = lexer.parseSlashEntry(parseConf)
        if not node.isNil:
          pushClosed(node)

      of ottInlineSrc:
        pushClosed(
          lexer.popAsStr().initCodeLexer().asVar().parseSrcInline(parseConf))

      of ottInlineCall:
        pushClosed(
          lexer.popAsStr().initCodeLexer().asVar().parseCallInline(parseConf))

      of ottTargetOpen:
        lexer.skip(ottTargetOpen)
        pushClosed(
          newTree(orgTarget, newTree(orgRawText, lexer.popAsStr({ottRawText}))))

        lexer.skip(ottTargetClose)

      of ottRadioTargetOpen:
        lexer.skip(ottRadioTargetOpen)
        pushClosed(newTree(orgRadioTarget,
          newTree(orgRawText, lexer.popAsStr({ottRawText}))))

        lexer.skip(ottRadioTargetClose)

      of ottLinkOpen:
        var link = newTree(orgLink)

        lexer.skip(ottLinkOpen)
        lexer.skip(ottLinkTargetOpen)

        var target = newTree(orgLinkTarget, lexer.popAsStr({ottRawText}))
        let str = target.strVal()
        link.add target

        if str[0] == '(' and str[^1] == ')':
          link.subKind = oskLinkCallout

        elif str[0] == '*':
          link.subKind = oskLinkSubtree

        elif str[0] == '#':
          link.subKind = oskLinkId

        elif str[0] in {'.', '/'}:
          link.subKind = oskLinkFile

        else:
          let colon = str.skipWhile({'a' .. 'z'})
          if colon < str.len and str[colon] == ':':
            let protocol = str[0 .. colon]
            case protocol:
              of "file": link.subKind = oskLinkFile
              else: raise newImplementKindError(protocol)

          else:
            link.subKind = oskLinkImplicit

        lexer.skip(ottLinkTargetClose)


        if lexer[ottLinkDescriptionOpen]:
          lexer.skip(ottLinkDescriptionOpen)
          var desc: seq[OrgTextToken]
          while ?lexer and not lexer[ottLinkDescriptionClose]:
            desc.add lexer.pop()

          var descLexer = initLexer(desc)

          link.add newTree(orgParagraph, parseText(descLexer, parseConf))
          lexer.skip(ottLinkDescriptionClose)

        else:
          link.add newOrgEmpty()

        lexer.skip(ottLinkClose)

        pushClosed(link)


      else:
        raise newUnexpectedTokenError(lexer)

  pushBuf()
  while stack.len > 1:
    closeWith(newOrgEmptyNode())


  result = stack.first().mapIt(it.node)

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
        lexer.next()
        var buf = lexer.initEmptyStrRanges()
        while not lexer.atEnd():
          buf.add lexer.pop()

        buf.pop()

        var textlexer = newSublexer(buf.toSlice(lexer))
        result = orgPlaceholder.newTree(
          textlexer.parseParagraph(parseConf))

      elif lexer[0] == '<' and lexer[^1] == '>':
        lexer.next()
        var buf = lexer.initStrRanges()
        while not lexer.atEnd():
          buf.add lexer.pop()

        buf.pop()

        result = orgRadioTarget.newTree(
          orgRawText.newTree(buf.toSlice(lexer)))

      else:
        result = orgPlaceholder.newTree(lexer.parseParagraph(parseConf))

proc parseText*(str: PosStr, conf: ParseConf): OrgNode =
  var lexer = initLexer(str, lexText)
  result = newTree(orgParagraph, parseText(lexer, conf))
