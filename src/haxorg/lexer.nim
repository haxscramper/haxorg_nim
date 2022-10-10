## Universal lexer implementation

import
  hmisc/algo/[
    hlex_base,
    hparse_base,
    hstring_algo
  ],
  haxorg/[
    parse_org_common,
    enum_types,
    types,
    common
  ],
  hmisc/core/all,
  std/[
    algorithm,
    re,
    sequtils
  ]

export enum_types, hlex_base, hparse_base

const
  markupConfig = {
    '*': (start: OTkBoldOpen,
          finish: OTkBoldClose,
          inline: OTkBoldInline),
    '/': (start: OTkItalicOpen,
          finish: OTkItalicClose,
          inline: OTkItalicInline),
    '=': (start: OTkVerbatimOpen,
          finish: OTkVerbatimClose,
          inline: OTkVerbatimInline),
    '`': (start: OTkBacktickOpen,
          finish: OTkBacktickClose,
          inline: OTkBacktickInline),
    '~': (start: OTkMonospaceOpen,
          finish: OTkMonospaceClose,
          inline: OTkMonospaceInline),
    '_': (start: OTkUnderlineOpen,
          finish: OTkUnderlineClose,
          inline: OTkUnderlineInline),
    '+': (start: OTkStrikeOpen,
          finish: OTkStrikeClose,
          inline: OTkStrikeInline),
    '"': (start: OTkQuoteOpen,
          finish: OTkQuoteClose,
          inline: otNone),
  } ## Table of the markup config information, to reduce usage of the
    ## chracter literals directly in the code.

  markupTable = toMapArray markupConfig
  markupKeys = toKeySet markupConfig


# proc initLexCode*(): HsLexCallback[OrgToken] =
#   var state = newLexerState(oblsNone)
#   return proc(str: var PosStr): seq[OrgToken] =
#     if not ?str:
#       discard

#     else:
#       case state.topFlag():
#         of oblsNone:
#           result.add str.initTok(
#             str.asSlice str.skip({'#'}, {'+'}), OTkCommandPrefix)

#           result.add str.initTok(
#             str.asSlice str.skipWhile(IdentChars + {'-', '_'}),
#             OTkCommandBegin)

#           state.toFlag oblsInHeader

#         of oblsInHeader:
#           result.addInitTok(str, OTkLangName):
#             str.skipWhile(IdentChars)

#           if ?str and str[' ']:
#             str.space()
#             result.addInitTok(str, OTkCommandArgs):
#               while ?str: str.next()


#           else:
#             result.add str.initFakeTok(OTkCommandArgs)

#           state.toFlag oblsInBody

#         of oblsInBody:
#           while ?str:
#             case str[]:
#               of '<':
#                 if str[+1, '<']:
#                   result.add str.initAdvanceTok(2, OTkNowebOpen)

#                 else:
#                   # TODO merge with previous token if it has the same kind
#                   result.add str.initAdvanceTok(1, OTkTextBlock)

#               of '\n':
#                 result.add str.initAdvanceTok(1, OTkNewline)

#               of '(':
#                 if str["(ref:"]:
#                   result.add str.initAdvanceTok(5, OTkParOpen)
#                   result.add str.scanTok(OTkIdent, @')')
#                   result.add str.initAdvanceTok(1, OTkParClose)

#                 else:
#                   result.addOrJoin(str.initAdvanceTok(1, OTkTextBlock))

#               else:
#                 str.pushSlice()
#                 while ?str and not str[{'<', '\n', '('}]:
#                   # NOTE can detect string literals and other constructs in
#                   # the code and skip them. This can be configured? (tangle
#                   # string literals or not)
#                   str.next()

#                 result.addOrJoin(str.initTok(str.popSlice(), OTkTextBlock))

#           state.toFlag oblsEnded

#         of oblsEnded:
#           str.skip({'#'}, {'+'})
#           let id = str.asSlice str.skipWhile(IdentChars + {'-', '_'})
#           result.add initTok(id, OTkCommandEnd)
#           state.toFlag oblsComplete

#         of oblsComplete:
#           assert false, "complete stage was reached, no longer parsing"

proc initLexTable*(): HsLexCallback[OrgToken] =
  var state = newLexerState(oblsNone)
  proc impl(str: var PosStr): seq[OrgToken] =
    if not ?str:
      discard

    else:
      case str[]:
        of '#':
          let pos = str.getPos()
          var isTableCmd = true
          str.next()
          if str['+']:
            str.skip({'+'})
            let id = str.asSlice(str.skipWhile(OCommandChars))
            let kind = id.classifyCommand()
            case kind:
              of ockBeginTable: result.add id.initTok(
                id.strVal(), OTkTableBegin)
              of ockRow:        result.add id.initTok(id.strVal(), OTkRowSpec)
              of ockCell:       result.add id.initTok(id.strVal(), OTkCellSpec)
              of ockEndTable:   result.add id.initTok(id.strVal(), OTkTableEnd)
              else: isTableCmd = false

            if isTableCmd:
              state.toFlag(oblsInHeader)
              str.space()
              if ?str:
                if result.last().kind == OTkTableEnd:
                  state.toFlag oblsComplete

                else:
                  result.addInitTok(str, OTkCmdArguments):
                    str.skipUntil('\n', including = true)

                  if ?str:
                    str.skip('\n')
                    state.toFlag(oblsInBody)

          else:
            isTableCmd = false

          if not isTableCmd:
            str.setPos(pos)
            result.addInitTok(str, OTkContent):
              str.skipPastEol()

        of '|':
          let pos = str.getPos()
          str.skipBeforeEol()
          if str['|']:
            str.setPos(pos)

            var first = true
            dowhile ?str and str['|']:
              result.add str.initTok(
                tern(first, OTkPipeOpen, OTkPipeSeparator),
                str.asSlice str.skip('|'))

              first = false
              str.space()
              if ?str and not str['\n']:
                result.addInitTok(str, OTkContent):
                  str.skipBefore({'|', '\n'})
                  if str[' ']:
                    while str[' ']: str.back()
                    if not str[' ']: str.next()

                  else:
                    str.next()
                str.space()

            discard result.pop()
            result.add str.initTok(OTkPipeClose)

          else:
            str.setPos(pos)
            result.add str.initTok(OTkPipeCellOpen, str.asSlice str.skip('|'))
            str.space()
            result.add str.initTok(OTkContent, str.asSlice str.skipToEol())

          if ?str:
            str.skip('\n')

        of '\n':
          str.next()
          result.add impl(str)

        else:
          if state of oblsInHeader:
            result.addInitTok(str, OTkCmdArguments):
              str.skipPastEol()

          else:
            result.addInitTok(str, OTkContent):
              while ?str and not str[{'|', '#'}]:
                str.skipPastEol()

              if ?str:
                str.back()

            if ?str:
              str.next()


  proc lexTable(str: var PosStr): seq[OrgToken] =
    while ?str and not (state of oblsComplete):
      result.add impl(str)

    if state of oblsComplete:
      if str[Newline]:
        str.next()

  return lexTable


proc lexText*(str: var PosStr): seq[OrgToken]

proc lexTime*(str: var PosStr): seq[OrgToken] =
  proc maybeTimeRange(str: var PosStr): seq[OrgToken] =
    if str[re"\s*=>\s*\d+:\d+"]:
      str.space()
      result.addInitTok(str, OTkTimeArrow):
        str.skip("=>")

      str.space()
      result.addInitTok(str, OTkTimeDuration):
        str.skipWhile(Digits)
        str.skip(':')
        str.skipWhile(Digits)

  if str['<']:
    if str["<%%"]:
      result.addInitTok(str, OTkDiaryTime):
        str.skip("<%%")
        str.skipBalancedSlice({'('}, {')'})
        str.skip(">")

    else:
      result.add initTok(
        OTkAngleTime, str.scanSlice('<', @{'>', '\n'}, '>'))

      if str["--"]:
        result.add initTok(OTkTimeDash, str.scanSlice("--"))
        result.add initTok(
          OTkAngleTime, str.scanSlice('<', @{'>', '\n'}, '>'))

        result.add maybeTimeRange(str)

  elif str['[']:
    result.add initTok(
      OTkBracketTime, str.scanSlice('[', @{']', '\n'}, ']'))

    if str["--"]:
      result.add initTok(OTkTimeDash, str.scanSlice("--"))
      result.add initTok(
        OTkBracketTime, str.scanSlice('[', @{']', '\n'}, ']'))

      result.add maybeTimeRange(str)

  else:
    raise newUnexpectedCharError(str)



proc lexLinkTarget*(str: var PosStr): seq[OrgToken] =
  # Link protocols whose links are better kept intact
  if str["https"] or str["http"]:
    result.add str.initTok(str, OTkLinkFull)

  # File protocl
  elif str["file"] or
       str["attachment"] or
       str["docview"] or
       str['/'] or
       str["./"]:

    if str['.'] or str['/']:
      result.add initFakeTok(str, OTkLinkProtocol, "file")

    else:
      result.addInitTok(str, OTkLinkProtocol):
        str.skipTo(':')
      str.skip(':')

    result.addInitTok(str, OTkLinkTarget):
      while ?str and not str["::"]:
        str.next()

    if str["::"]:
      result.addInitTok(str, OTkLinkExtraSeparator):
        str.next(2)

      result.addInitTok(str, OTkLinkExtra):
        str.skipPastEOF()

  # Simple, non-URI protocols that don't have trailing extra separator
  # parametrization and all other cases (including user-provided link
  # templates)
  else:
    if str.hasAhead({':'}):
      result.addInitTok(str, OTkLinkProtocol):
        str.skipTo(':')

      str.skip(':')
      result.addInitTok(str, OTkLinkTarget):
        str.skipPastEOF()

    else:
      result.addInitTok(str, OTkLinkInternal):
        str.skipPastEOF()


proc lexBracket*(str: var PosStr): seq[OrgToken] =
  if str["[["]:
    result.add str.scanTok(OTkLinkOpen, '[')

    block link_token:
      result.add str.scanTok(OTkLinkTargetOpen, '[')
      result.add str.asSlice(str.skipUntil({']'})).asVar().lexLinkTarget()
      result.add str.scanTok(OTkLinkTargetClose, ']')

    block description_token:
      if str['[']:
        result.add str.scanTok(OTkLinkDescriptionOpen, '[')
        var desc = str.asSlice str.skipUntil({']'})
        while ?desc:
          result.add lexText(desc)

        result.add str.scanTok(OTkLinkDescriptionClose, ']')

    result.add str.scanTok(OTkLinkClose, ']')

  elif str["[fn:"]:
    result.add str.scanTok(OTkFootnoteStart, '[')
    str.skip("fn")
    if str["::"]:
      result.add str.scanTok(OTkDoubleColon, "::")
      result.addInitTok(str, OTkText): str.skipTo(']')

    else:
      result.addInitTok(str, OTkColon): str.skip(':')
      result.addInitTok(str, OTkIdent): str.skipTo(']')

    result.add str.scanTok(OTkFootnoteEnd, ']')

  else:
    return lexTime(str)


const TextChars = MaybeLetters + Digits + { '.', ',', '-'}
proc lexTextChars*(str: var PosStr): seq[OrgToken] =
  var isStructure: bool = false
  if str["src"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("src"), OTkSrcOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf

      result.add str.initTok(str.asSlice str.skipWhile(IdentChars), OTkSrcName)
      if str['[']:
        result.add str.initTok(
          str.asSlice(str.skipBalancedSlice({'['}, {']'}), 1, -2),
          OTkSrcArgs
        )

      result.add str.initTok(
        str.asSlice(str.skipBalancedSlice({'{'}, {'}'}), 1, -2),
        OTkSrcBody
      )

      result.add str.initTok(OTkSrcClose)
      isStructure = true

    else:
      str.setPos(pos)


  elif str["call"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("call"), OTkCallOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf

      result.add str.initTok(
        str.asSlice str.skipWhile(IdentChars), OTkCallName)

      if str['[']:
        result.add str.initTok(
          str.asSlice(str.skipBalancedSlice({'['}, {']'}), 1, -2),
          OTkCallInsideHeader
        )

      result.add str.initTok(
        str.asSlice(str.skipBalancedSlice({'('}, {')'}), 1, -2),
        OTkCallArgs
      )

      result.add str.initTok(OTkCallClose)
      isStructure = true

    else:
      str.setPos(pos)


  if not isStructure:
    var allUp = true

    str.startSlice()
    while ?str and str[TextChars + {'-'}]:
      if not str[HighAsciiLetters]:
        allUp = false

      str.next()

    result.add str.initSliceTok(if allUp: OTkBigIdent else: OTkWord)


proc lexParenArguments(str: var PosStr): seq[OrgToken] =
  result.add str.initAdvanceTok(1, OTkParOpen)
  while not str[')']:
    # Read argument until the first comma or closing parent
    result.addInitTok(str, OTkRawText):
      # TODO handle quoted strings and escaped commas
      str.skipUntil({',', ')'})

    # maybe lex comma
    if str[',']:
      result.add str.initAdvanceTok(1, OTkComma)

    # optional space, not significant for argument passing
    str.space()

  result.add str.initAdvanceTok(1, OTkParClose)


proc lexText*(str: var PosStr): seq[OrgToken] =
  const
    NonText = TextLineChars - AsciiLetters - Utf8Any

  case str[]:
    of TextChars:
      result.add str.lexTextChars()

    of '\n':
      result.add str.initAdvanceTok(1, OTkNewline)

    of ' ':
      result.add str.initTok(str.popWhileSlice({' '}), OTkSpace)

    of '#':
      proc rec(str: var PosStr): seq[OrgToken] =
        result.addInitTok(str, OTkHashTag):
          if str['#']:
            str.skip('#')

          str.skipWhile(IdentChars)

        while str["##"] and not str["##["]:
          result.addInitTok(str, OTkHashTagSub):
            str.skip('#')

          result.addInitTok(str, OTkHashTag):
            str.skip('#')
            str.skipWhile(IdentChars)

        if str["##["]:
          result.addInitTok(str, OTkHashTagSub):
            str.skip('#')

          result.addInitTok(str, OTkHashTagOpen):
            str.skip('#')
            str.skip('[')

          while ?str and not str[']']:
            result.add rec(str)
            str.space()
            if str[',']:
              result.addInitTok(str, OTkComma):
                str.skip(',')

              str.space()

          result.addInitTok(str, OTkHashTagClose):
            str.skip(']')

      result.add rec(str)

    of '@':
      if str[+1, IdentChars]:
        result.addInitTok(str, OTkAtMention):
          str.skip('@')
          str.skipWhile(IdentChars)

      else:
        raise newImplementError()

    of '$':
      if str[+1, '$']:
        result.add str.scanTok(OTkDollarOpen, '$', '$')
        str.startSlice()
        var hasEnd = false
        while ?str and not hasEnd:
          while ?str and not str['$']:
            str.next()

          if str['$', '$']:
            result.add str.initTok(str.popSlice(), OTkLatexInlineRaw)
            hasEnd = true

          else:
            raise newImplementError()

        result.add str.scanTok(OTkDollarClose, '$', '$')

      else:
        result.add str.scanTok(OTkDollarOpen, '$')
        result.add str.initTok(
          str.asSlice str.skipUntil({'$'}),
          OTkLatexInlineRaw)

        result.add str.scanTok(OTkDollarClose, '$')

    of '\\':
      case str[+1]:
        of '[', '(':
          let inline = str[+1, {'('}]
          if inline:
            result.add str.initTok(OTkLatexParOpen, str.scanSlice(r"\("))

          else:
            result.add str.initTok(OTkLatexBraceOpen, str.scanSlice(r"\["))

          result.addInitTok(str, OTkLatexInlineRaw):
            while not str[tern(inline, r"\)", r"\]")]:
              str.next()

          if inline:
            result.add str.initTok(OTkLatexParClose, str.scanSlice(r"\)"))

          else:
            result.add str.initTok(OTkLatexBraceClose, str.scanSlice(r"\]"))

        of OMarkupChars:
          result.addInitTok(str, OTkEscaped):
            str.next(2)

        of IdentStartChars - {'_'}:
          result.addInitTok(str, OTkSymbolStart):
            str.skip({'\\'})

          result.addInitTok(str, OTkIdent):
            str.skipWhile(IdentChars)

          if str['[']:
            result.addInitTok(str, OTkMetaBraceOpen):
              str.skip('[')

            result.addInitTok(str, OTkMetaBraceBody):
              str.skipBalancedSlice(
                {'['},
                {']'},
                skippedStart = true,
                consumeLast = false
              )

            result.addInitTok(str, OTkMetaBraceClose):
              str.skip(']')

          while str['{']:
            result.addInitTok(str, OTkMetaArgsOpen):
              str.skip('{')

            result.addInitTok(str, OTkMetaArgsBody):
              str.skipBalancedSlice(
                {'{'},
                {'}'},
                skippedStart = true,
                consumeLast = false,
              )

            result.addInitTok(str, OTkMetaArgsClose):
              str.skip('}')

        of '\\':
          result.addInitTok(str, OTkDoubleSlash):
            str.skip('\\')
            str.skip('\\')

        else:
          raise newImplementError($str)

    of '~', '`', '=':
      let start = str[]
      if str[+1, start]:
        # Inline verbatim text
        result.add str.initTok(
          str.popPointSlice(advance = 2), markupTable[start].inline)

        result.addInitTok(str, OTkRawText):
          while not str[start, start]:
            str.next()

        result.add str.initTok(
          str.popPointSlice(advance = 2),
          markupTable[start].inline)

      else:
        # Open/close pair in the text
        if str[-1, NonText] or str.atStart():
          result.add str.initTok(
            str.popPointSlice(),
            markupTable[start].start)

          result.addInitTok(str, OTkRawText):
            str.skipTo(start)

        else:
          echov str

        if str[+1, NonText] or str.beforeEnd():
          result.add str.initTok(
            str.popPointSlice(),
            markupTable[start].finish)

    of '<':
      if str['<', '<', '<']:
        result.add str.initAdvanceTok(3, OTkRadiOTkrgetOpen)

        # TODO More sophisicated lexer that checks for `>>` and `>``
        result.addInitTok(str, OTkRawText):
          str.skipUntil({ '>' })

        result.addInitTok(str, OTkRadiOTkrgetClose):
          str.skip('>')
          str.skip('>')
          str.skip('>')


      elif str['<', '<']:
        result.add str.initAdvanceTok(2, OTkTargetOpen)
        result.addInitTok(str, OTkRawtext, str.skipUntil({ '>' }))
        result.addInitTok(str, OTkTargetClose):
          str.skip('>')
          str.skip('>')

      elif str["<%%"]:
        result.add str.lexTime()

      else:
        result.add str.initAdvanceTok(1, OTkPlaceholderOpen)
        result.add str.initTok(str.asSlice str.skipUntil({ '>' }), OTkRawText)
        result.add str.initTok(str.asSlice str.skip('>'), OTkPlaceholderClose)

    of markupKeys - { '<', '~', '`', '=' }:
      let ch = str[]
      let (kOpen, kClose, kInline) = markupTable[ch]

      if str[+1, ch]:
        result.add str.initTok(
          str.popPointSlice(advance = 2), kInline)

      elif str[-1, NonText] or str.atStart():
        result.add str.initTok(str.popPointSlice(), kOpen)

      elif str[+1, NonText] or str.beforeEnd():
        result.add str.initTok(str.popPointSlice(), kClose)

      else:
        raise newImplementError($str & " " & $str.getPos())

    of '[':
      result = lexBracket(str)

    of '(':
      result.addInitTok(str, OTkParOpen):
        str.next()

    of ')':
      result.addInitTok(str, OTkParClose):
        str.next()

    of ':':
      result.addInitTok(str, OTkColon):
        str.next()

    of '{':
      if str["{{{"]:
        result.add str.initAdvanceTok(3, OTkMacroOpen)
        result.add str.initTok(
          asSlice(str, inWhile(?str and
                               not str['('] and
                               not str["}}}"],
                               str.next())),
          OTkIdent
        )

        if str['(']:
          result.add str.lexParenArguments()

        if ?str:
          result.add str.initAdvanceTok(3, OTkMacroClose)

      else:
        result.add str.initAdvanceTok(1, OTkMaybeWord)

    else:
      raise newUnexpectedCharError(str)





proc lexSubtree(str: var PosStr): seq[OrgToken] =
  result.add str.initTok(str.asSlice str.skipWhile({'*'}), OTkSubtreeStars)
  str.skipWhile({' '})
  block todo:
    var tmp = str
    tmp.startSlice()
    tmp.skipWhile(HighAsciiLetters)
    if tmp[' ']:
      result.add tmp.initTok(tmp.popSlice(), OTkSubtreeTodoState)
      str = tmp

  str.skipWhile({' '})

  if str["[#"]:
    str.pushSlice()
    str.next(2)

    # org-mode only supports one-letter importance markers, but I
    # think it is too limiting, so this implementation also handles
    # `[#URGENT]`,
    str.skip(HighAsciiLetters) # But at least one letter has to be specified
    str.skipWhile(HighAsciiLetters)
    str.skip({']'})

    result.add str.initTok(str.popSlice(), OTkSubtreeUrgency)

    str.skipWhile({' '})

  var
    body = str.asSlice(str.skipToEol())
    headerTokens: seq[OrgToken]

  body.skipToEof()
  if body[':']:
    let finish = body.getPos()
    body.back()

    var tagEnded = false
    while ?body and not tagEnded:
      while ?body and body[IdentChars]:
        body.back()

      body.skipBack({':'})
      if body[' ']:
        tagEnded = true

    let start = body.getPos(+1)
    headerTokens.add body.initTok(
      str.sliceBetween(start, finish), OTkSubtreeTag)

    while body[' ']:
      body.back()

  if body[']']:
    let finish = body.getPos()
    body.skipBack({']'})
    body.skipBack(Digits)
    while body[Digits]:
      body.back()

    if str['%']:
      body.back()

    else:
      body.skipBack({'/'})
      body.skipBack(Digits)
      while body[Digits]:
        body.back()

    body.skipBack({'['})

    let start = body.getPos(+1)

    headerTokens.add body.initTok(
      str.sliceBetween(start, finish), OTkSubtreeCompletion)

    while body[' ']:
      body.back()

  block:
    let finish = body.getPos()
    body.skipToSof()
    let start = body.getPos()

    headerTokens.add body.initTok(
      str.sliceBetween(start, finish), OTkText)


  result.add headerTokens.reversed()
  discard str.trySkip('\n')

  var times = str
  times.space()

  if times[HighAsciiLetters]:
    let tag = times.asSlice times.skipWhile(HighAsciiLetters)
    if tag.strValNorm() in ["deadline", "closed"]:
      result.add initTok(tag, OTkSubtreeTime)
      times.skip({':'})
      times.space()
      result.add times.lexTime()
      times.skip({'\n'})
      str = times

  var drawer = str
  drawer.space()
  var drawerEnded = false
  if drawer[':']:
    while ?drawer and not drawerEnded:
      drawer.space()
      let id = drawer.scanSlice(':', *\Id, ':')
      drawer.skip({'\n'})
      case id.strValNorm():
        of ":properties:":
          result.add initTok(id, OTkColonProperties)
          var hasEnd = false

          while ?drawer and not hasEnd:
            drawer.space()
            var isAdd = false
            let id = drawer.scanSlice(':', *\DId, ?'+' -> (isAdd = true), ':')
            if id.strValNorm() == ":end:":
              hasEnd = true
              result.add initTok(id, OTkColonEnd)

            else:
              result.add initTok(
                id, tern(isAdd, OTkColonAddIdent, OTkColonIdent))

              if drawer[IdentStartChars]:
                result.addInitTok(drawer, OTkIdent):
                  while ?drawer and drawer[DashIdentChars]:
                    drawer.next()

                drawer.skip(':')

              drawer.space()

              result.addInitTok(drawer, OTkRawProperty):
                drawer.skipToEol()

              drawer.skip('\n')

        of ":logbook:":
          result.add initTok(id, OTkColonLogbook)
          drawer.startSlice()

          var hasEnd = false
          while ?drawer and not hasEnd:
            while ?drawer and not drawer[rei":end:"]:
              drawer.next()

            result.add initTok(drawer.popSlice(), OTkRawLogbook)
            let id = drawer.asSlice:
              drawer.skip({':'})
              drawer.skipWhile(IdentChars)
              drawer.skip(':')

            result.add initTok(id, OTkColonEnd)
            hasEnd = true


        else:
          raise newImplementKindError(id.strValNorm())

      var ahead = drawer
      ahead.space()
      if ahead.trySkip('\n'):
        ahead.space()
        if ahead.trySkip('\n'):
          drawerEnded = true
          drawer = ahead

      if not drawerEnded:
        drawer.skipWhile({'\n'})



    str = drawer

  result.add str.initFakeTok(OTkSubtreeEnd)


proc lexSourceBlockContent(str: var PosStr): seq[OrgToken] =
  while ?str:
    if str["<<"]:
      var
        failedAt = -1
        tmp = str
        tmpRes: seq[OrgToken]

      block try_tangle:
        tmpRes.addInitTok(tmp, OTkNowebOpen):
          tmp.skip("<<")

        if tmp[IdentChars]:
          tmpRes.addInitTok(tmp, OTkIdent):
            tmp.skipWhile(IdentChars)

        else:
          failedAt = tmp.pos
          break try_tangle

        if tmp['(']:
          tmpRes.add str.lexParenArguments()

        if tmp[">>"]:
          tmpRes.addInitTok(tmp, OTkNowebClose):
            tmp.skip(">>")

        else:
          failedAt = tmp.pos
          break try_tangle

      if failedAt != -1:
        result.addInitTok(str, OTkCodeText):
          while str.pos < failedAt:
            str.next()

        str.pos = failedAt

      else:
        result.add tmpRes
        str = tmp

    elif str["(refs:"]:
      result.addInitTok(str, OTkParOpen):
        str.skip('(')

      result.addInitTok(str, OTkIdent):
        str.skip("refs")

      result.addInitTok(str, OTkColon):
        str.skip(':')

      result.addInitTok(str, OTkIdent):
        str.skipWhile(IdentChars + {'-'})

      result.addInitTok(str, OTkParClose):
        str.skip(')')

    elif str['\n']:
      result.addInitTok(str, OTkNewline):
        str.next()

    else:
      result.addInitTok(str, OTkCodeText):
        while ?str and
              not (str["<<"] or str["(refs:"] or str['\n']):
          str.next()


proc lexCommandContent(
    str: var PosStr, kind: OrgCommandKind): seq[OrgToken] =
  result.add str.initTok(OTkCommandContentStart)

  case kind:
    of ockBeginQuote:
      result.addInitTok(str, OTkText):
        str.skipPastEof()

    of ockBeginDynamic:
      str.space()
      result.addInitTok(str, OTkText):
        str.skipPastEof()

    of ockBeginSrc:
      str.space()
      result.add str.initFakeTok(OTkCodeContentBegin)
      var code = str.lexSourceBlockContent()
      if code.last() of OTkCodeText and
         code.last().strVal().allIt(it in {' '}):
        discard code.pop

      if code.last() of OTkNewline:
        discard code.pop()

      result.add code
      result.add str.initFakeTok(OTkCodeContentEnd)

    else:
      assert false, $kind

  result.add str.initTok(OTkCommandContentEnd)

proc lexDelimited(
    str: var PosStr,
    start, finish: tuple[text: char, kind: OrgTokenKind],
    middle: OrgTokenKind
  ): seq[OrgToken] =

  result.addInitTok(str, start.kind):
    str.skip(start.text)

  result.addInitTok(str, middle):
    while ?str and not str[finish.text]:
      if str['\\']:
        str.next(2)

      else:
        str.next()

  if ?str:
    result.addInitTok(str, finish.kind):
      str.next()

proc lexCommandArguments(
    str: var PosStr, kind: OrgCommandKind): seq[OrgToken] =

  let (wrapStart, wrapEnd) = case kind:
    of ockTitle:
      (OTkParagraphStart, OTkParagraphEnd)

    else:
      (OTkCommandArgumentsBegin, OTkCommandArgumentsEnd)

  result.add str.initTok(wrapStart)

  proc lexKeyValue(str: var PosStr): seq[OrgToken] =
    while ?str:
      case str[]:
        of '-':
          result.addInitTok(str, OTkCommandFlag):
            str.skipWhile({'-'} + IdentChars)

        of ':':
          result.addInitTok(str, OTkCommandKey):
            str.skipWhile(IdentChars + {'-', ':'})

        of ' ':
          str.space()

        else:
          result.addInitTok(str, OTkCommandValue):
            var hasColon = false
            while ?str and not hasColon:
              while ?str and not str[HorizontalSpace]:
                str.next()

              if ?str:
                var tmp = str
                tmp.space()
                if not tmp[':']:
                  tmp.next()
                  str = tmp
                else:
                  hasColon = true

  case kind:
    of ockBeginQuote:
      discard

    of ockTitle:
      while ?str:
        result.add lexText(str)

    of ockOptions:
      while ?str:
        case str[]:
          of {'\'', '*', '|', ':', '<', '\n', '^'}:
            result.addInitTok(str, OTkRawText):
              str.next()

          of ' ':
            str.space()

          else:
            result.addInitTok(str, OTkRawText):
              while ?str and not str[HorizontalSpace]:
                str.next()

    of ockCaption:
      result.addInitTok(str, OTkText):
        str.skipPastEof()

    of ockBeginSrc:
      result.addInitTok(str, OTkWord):
        str.skipWhile(IdentChars)

      str.space()
      result.add lexKeyValue(str)

    of ockBeginTable, ockAttrHtml:
      result.add lexKeyValue(str)

    of ockBeginDynamic:
      result.addInitTok(str, OTkWord):
        str.skipWhile(IdentChars)

      str.space()
      result.add lexKeyValue(str)

    of ockAuthor, ockCreator, ockLanguage:
      str.space()
      result.addInitTok(str, OTkRawText):
        str.skipPastEof()

    of ockProperty:
      str.space()
      result.addInitTok(str, OTkIdent):
        str.skipUntil({' ', ':'})


      if str[':']:
        result.addInitTok(str, OTkColon):
          str.next()

        result.addInitTok(str, OTkIdent):
          str.skipUntil({' '})

      str.space()
      result.addInitTok(str, OTkRawProperty):
        str.skipPastEof()

    of ockFiletags:
      while str[':']:
        str.skip(':')
        if ?str:
          result.addInitTok(str, OTkSubtreeTag):
            while ?str and not str[':']:
              next(str)

    of ockInclude:
      str.space()
      if str['"']:
        result.add str.lexDelimited(
          start = ('"', OTkQuoteOpen),
          finish = ('"', OTkQuoteClose),
          middle = OTkRawText
        )

      else:
        result.addInitTok(str, OTkRawText):
          while ?str and not str[HorizontalSpace]:
            str.next()

      result.add lexKeyValue(str)

    of ockColumns:
      result.addInitTok(str, OTkRawText):
        str.skipPastEof()

    else:
      raise newUnexpectedKindError(kind, $str)

      result.addInitTok(str, OTkRawText):
        str.skipPastEof()

  result.add str.initTok(wrapEnd)

proc lexCommandBlock(str: var PosStr): seq[OrgToken] =
  # Store position of the command start - content be dedented or indented
  # arbitrarily, so `#+begin_src` starting at column 2 might have content
  # that starts on the column 0.
  let column = str.column
  result.add str.initAdvanceTok(2, OTkCommandPrefix)
  let id = str.asSlice(): str.skipWhile(OCommandChars)

  if id.strValNorm().startsWith("begin"):
    result.add initTok(id, OTkCommandBegin)
    let sectionName = id.strVal().normalize().dropPrefix("begin")
    let kind = id.strVal().classifyCommand()
    if kind == ockBeginDynamic:
      str.skip(':')

    str.space()

    var arguments = str.asSlice(str.skipPastEol(), -2)
    result.add lexCommandArguments(arguments, kind)

    var found = false
    str.pushSlice()
    while not found and ?str:
      while ?str and not(str.column == column and str["#+"]):
        str.next()

      assert ?str
      let prefix = str.asSlice str.next(2)
      let id: PosStr = str.asSlice str.skipWhile(OCommandChars)
      if id.strVal().normalize() == "end" & sectionName:
        found = true
        var slice = str.popSlice(-(
            1 #[ default offest ]# +
            id.strVal().len() #[ `end_<xxx>` ]# +
            3 #[ `#+` and trailing newline ]#
        ))

        result.add slice.lexCommandContent(kind)
        result.add initTok(prefix, OTkCommandPrefix)
        result.add initTok(id, OTkCommandEnd)

    if kind == ockBeginDynamic:
      str.skip(':')

  else:
    result.add initTok(id, OTkLineCommand)
    result.add str.initAdvanceTok(1, OTkColon, {':'})
    str.space()
    var args = str.asSlice(): str.skipToEol()
    result.add lexCommandArguments(args, id.strVal().classifyCommand())

    if ?str:
      str.skip('\n')

proc isFirstOnLine*(str: var PosStr): bool =
  var pos = 0
  while str[pos] in HorizontalSpace:
    dec pos

  return str[pos] in Newline + {'\x00'}

proc atLogClock(str: var PosStr): bool =
  if str[rei"\s*CLOCK:\s+\["]:
    # Log clock entry should only start at the first column in the text.
    result = str.isFirstOnLine()

proc lexList(str: var PosStr): seq[OrgToken] =
  # Create temporary state to lex content of the list
  var state = newLexerState()

  proc aux(str: var PosStr): seq[OrgToken] =
    template popIndents(): untyped =
      let skipped = state.skipIndent(str)
      for indent in skipped:
        case indent:
          of likIncIndent:  result.add str.initTok(OTkIndent)
          of likDecIndent:  result.add str.initTok(OTkDedent)
          of likSameIndent: result.add str.initTok(OTkSameIndent)
          of likNoIndent:   result.add str.initTok(OTkNoIndent)
          of likEmptyLine:  raise newImplementError()

    
    case str[]:
      of '-':
        let indent = str.column
        result.add str.initTok(
          str.asSlice str.skip({'-'}), OTkListDash)

        str.space()

        if str['[']:
          result.add str.scanTok(OTkCheckbox, '[', {'X', 'x', ' ', '-'}, ']')
          str.space()

        # create slice for the whole content of the list item
        str.startSlice()
        var atEnd = false
        var nextList = true
        # extend slice until new list start is not found - either via new
        # nested item or by indentation decrease.
        while ?str and not atEnd:
          if str.atLogClock():
            str.next()
            atEnd = true
            nextList = false

          else:
            # go to the start of the next line
            str.skipPastEol()
            while str.trySkipEmptyLine(): discard
            # Decide based on the indentation what to do next
            # indentation decreased, end of the list item
            if str.getIndent() < indent:
              atEnd = true

            else:
              # indentation is the same or increased. Make temporarily lexer
              # copy and look ahead
              var store = str
              store.skipWhile({' '})
              # check if we are at the start of the new list - if we are, stop
              # parsing completely and apply all withheld lexer changes,
              # otherwise don't touch `atEnd` in order to continue parsing.
              if store["- "]: # HACK user proper list start checking
                atEnd = true
                # nextList = indent <= store.column


        result.add str.initTok(str.popSlice(-1), OTkStmtList)
        result.add str.initTok(OTkListItemEnd)
        if nextList:
          # current list contains nested items - skip necessary indentation
          # levels and recursively call lexer from this point onwards.
          popIndents()
          result.add aux(str)

      of '\n', '\x00':
        for level in 0 ..< state.getIndentLevels():
          result.add str.initTok(OTkDedent)

        if ?str:
          str.next()

        state.clearIndent()

      of ' ':
        popIndents()

      else:
        raise newUnexpectedCharError(
          str,
          parsing = "ordered or unordered list")

  result = aux(str)
  while state.hasIndent():
    discard state.popIndent()
    result.add str.initTok(OTkDedent)


proc lexParagraph*(str: var PosStr): seq[OrgToken] =
  var ended = false
  str.startSlice()
  while ?str and not ended:
    while not ended and ?str and str[TextLineChars]:
      str.next()

    if str['\n']:
      str.next()
      if not ?str:
        ended = true

      else:
        case str[]:
          of MaybeLetters, {'-', ' '}:
            discard

          of '\n':
            str.next()
            ended = true

          else:
            raise newUnexpectedCharError(str, parsing = "paragraph")


  let tok = str.initTok(str.popSlice(tern(
    ended,
    # last trailing newline and pargraph separator newline
    tern(?str, -3, -2),
    -1)), OTkText)

  result.add tok

proc lexComment*(str: var PosStr): seq[OrgToken] =
  result.addInitTok(str, OTkComment):
    str.skipToEol()

proc lexStructure*(): HsLexCallback[OrgToken] =
  ## Create lexer for toplevel structure of the org document
  proc aux(str: var PosStr): seq[OrgToken] =
    # This procedure dispatches into toplevel lexer routines that are meant
    # to produce entries for the high-level document structure -
    # paragraphs, lists, subtrees, command blocks and so on.
    case str[]:
      of '#':
        case str[+1]:
          of '+':
            if str[rei"#\+begin[-_]?table"]:
              result = initLexTable()(str)

            else:
              result = lexCommandBlock(str)

          of IdentChars:
            # Parapgraph starts with the hashtag `#testing`
            result = lexParagraph(str)

          of ' ':
            result = lexComment(str)

          else:
            raise newImplementError($str)

      of '\x00':
        discard

      of '*':
        # No whitespace between a start and a character means it is a bold
        # word, otherwise it is a structural element - either subtree or a
        # unordered list.
        let hasSpace = str.startsWith(skip = {'*'}, search = {' '})
        if str.column == 0:
          if hasSpace:
            # `*bold*` world at the start of the paragraph
            result.add lexSubtree(str)

          else:
            # `* subtree` starting on the first line
            result = lexParagraph(str)

        else:
          if hasSpace:
            # `__* list item` on a line
            result = lexList(str)

          else:
            result = lexParagraph(str)

      of '-':
        result = lexList(str)

      of '\n', ' ':
        str.skipWhile({' ', '\n'})
        result = str.aux()

      of MaybeLetters, {'~', '['}:
        result = lexParagraph(str)

      of '\\', '{', '@':
        # Text starts with inline or display latex math equation, `\symbol`
        # or a macro call.
        result = lexParagraph(str)

      else:
        raise newUnexpectedCharError(str)

  return aux

proc lexGlobal*(): HsLexCallback[OrgToken] =
  ## Lex global structure of the org document
  var structure = lexStructure()
  proc auxGlobal(token: OrgToken): seq[OrgToken] =
    ## Recursively lex token that might contain contain complex nested content.
    var content = initPosStr(token)
    while ?content:
      result.add lexGlobal()(content)

  proc auxExpand(token: OrgToken): seq[OrgToken] =
    ## Re-lex 'container' tokens
    case token.kind:
      of OTkText:
        # generic 'text' token was found somewhere in the main structure
        # of the document - list content, `#+caption` element etc. In
        # that context it only had defined boundaries but further lexing
        # was deferred until now, to avoid repeating the same construct
        # dozen times.
        result.add token.initFakeTok(OTkParagraphStart)

        var content = initPosStr(token)
        while ?content:
          result.add lexText(content)

        result.add token.initFakeTok(OTkParagraphEnd)

      of OTkRawLogbook:
        result.add token.initFakeTok(OTkLogbookStart)
        var content = initPosStr(token)
        # Logbook is made up of several list entries which in turn (that's
        # why the first pass is constrained to list and second is not
        # constrained to anything) might contain complex nested elements
        while ?content:
          if content.atLogClock():
            result.add auxExpand(content.initTok(
              OTkText, content.asSlice content.skipToEol()))

            # text processing about should not include end of line.
            if content[Newline]:
              content.next()
              # If this is a joined list of log entires skip only newline,
              # otherwise cut all leading spaces to avoid messing up
              # indentation in the list parser.
              if not content.atLogClock():
                content.space()

          else:
            for entry in lexList(content):
              result.add auxExpand(entry)

        result.add token.initFakeTok(OTkLogbookEnd)

      of OTkContent:
        result.add token.initFakeTok(OTkContentStart)
        # Table might contain any structure, including more complex
        # elements such as lists, code blocks, other tables and so on.
        # This is an imporvement on top of the regular org-mode syntax
        # (although IIUC elisp parser would also allow for structures
        # like these)
        result.add auxGlobal(token)

        result.add token.initFakeTok(OTkContentEnd)

      of OTkStmtList:
        result.add token.initFakeTok(OTkStmtListOpen)
        result.add auxGlobal(token)
        result.add token.initFakeTok(OTkStmtListClose)

      else:
        result.add token

 
  proc aux(str: var PosStr): seq[OrgToken] =
    for token in structure(str):
      result.add auxExpand(token)

  return aux
