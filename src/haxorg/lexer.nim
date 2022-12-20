## Universal lexer implementation

import
  hmisc/algo/[
    hlex_base,
    hparse_base,
    hstring_algo,
    clformat
  ],
  haxorg/[
    parse_org_common,
    enum_types,
    types,
    common
  ],
  hmisc/core/[
    all,
    code_errors
  ],
  std/[
    algorithm,
    re,
    sequtils
  ]

export enum_types, hlex_base, hparse_base, types

func hShow*(e: OrgToken, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  result = "(" & hshow(e.kind, opts)

  if not e.strVal().empty():
    result &= " "
    result &= hshow(e.strVal(), opts)

  result &= ")"

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

proc auxExpand(token: OrgToken, lexConf: LexConf): seq[OrgToken]

template addExpandTok(
    res: var seq[OrgToken],
    str: var PosStr,
    kind: OrgTokenKind,
    body: untyped
  ): untyped =
  var tmp: seq[OrgToken]
  tmp.addInitTok(str, kind):
    body

  for tok in tmp:
    res.add auxExpand(tok, lexConf)

proc initLexTable*(lexConf: LexConf): HsLexCallback[OrgToken] =
  var state = newLexerState(oblsNone)
  proc impl(str: var PosStr): seq[OrgToken] {.lexx.} =
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
              of ockBeginTable:
                logAddTok(result, str):
                  result.add id.initTok(id.strVal(), OTkTableBegin)
              of ockRow:
                logAddTok(result, str):
                  result.add id.initTok(id.strVal(), OTkRowSpec)

              of ockCell:
                logAddTok(result, str):
                  result.add id.initTok(id.strVal(), OTkCellSpec)

              of ockEndTable:
                logAddTok(result, str):
                  result.add id.initTok(id.strVal(), OTkTableEnd)

              else:
                isTableCmd = false

            if isTableCmd:
              state.toFlag(oblsInHeader)
              str.space()
              if ?str:
                if result.last().kind == OTkTableEnd:
                  state.toFlag oblsComplete

                else:
                  result.logAddTok(str, OTkCmdArguments):
                    str.skipUntil('\n', including = true)

                  if ?str:
                    str.skip('\n')
                    state.toFlag(oblsInBody)

          else:
            isTableCmd = false

          if not isTableCmd:
            str.setPos(pos)
            result.logAddTok(str, OTkContent):
              str.skipPastEol()

        of '|':
          let pos = str.getPos()
          str.skipBeforeEol()
          if str['|']:
            str.setPos(pos)

            var first = true
            dowhile ?str and str['|']:
              logAddTok(result, str):
                result.add str.initTok(
                  tern(first, OTkPipeOpen, OTkPipeSeparator),
                  str.asSlice str.skip('|'))

              first = false
              str.space()
              if ?str and not str['\n']:
                result.logAddTok(str, OTkContent):
                  str.skipBefore({'|', '\n'})
                  if str[' ']:
                    while str[' ']: str.back()
                    if not str[' ']: str.next()

                  else:
                    str.next()
                str.space()

            discard result.pop()
            logAddTok(result, str):
              result.add str.initTok(OTkPipeClose)

          else:
            str.setPos(pos)
            logAddTok(result, str):
              result.add str.initTok(
                OTkPipeCellOpen, str.asSlice str.skip('|'))
            str.space()
            logAddTok(result, str):
              result.add str.initTok(
                OTkContent, str.asSlice str.skipToEol())

          if ?str:
            str.skip('\n')

        of '\n':
          str.next()
          logAddTok(result, str):
            result.add impl(str)

        else:
          if state of oblsInHeader:
            result.logAddTok(str, OTkCmdArguments):
              str.skipPastEol()

          else:
            result.logAddTok(str, OTkContent):
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


proc lexText*(str: var PosStr, lexConf: LexConf): seq[OrgToken]

proc maybeTimeRange(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =

  if str[re"\s*=>\s*\d+:\d+"]:
    str.space()
    result.logAddTok(str, OTkTimeArrow):
      str.skip("=>")

    str.space()
    result.logAddTok(str, OTkTimeDuration):
      str.skipWhile(Digits)
      str.skip(':')
      str.skipWhile(Digits)


# REFACTOR is this template really needed? It feels like an absolute hack
# that is used only twice in the whole code.
template trySpecific(
    str: var PosStr,
    failKind: OrgTokenKind,
    failAdvance: int,
    call: untyped
  ): untyped =
  ## Try to parse a specific language structure or skip `failAdvance`
  ## characters on failure, producing `failKind` token.
  var result: seq[OrgToken]
  try:
    var tmp = str
    result.add call(tmp, lexConf)
    str = tmp

  except UnexpectedCharError:
    result.logAddTok(str, failKind):
      str.next(failAdvance)

  result

proc lexAngle(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  if str["<%%"]:
    result.logAddTok(str, OTkDiaryTime):
      str.skip("<%%")
      str.skipBalancedSlice({'('}, {')'})
      str.skip(">")

  elif str['<', '<', '<']:
    logAddTok(result, str):
      result.add str.initAdvanceTok(3, OTkTripleAngleOpen)

    # TODO More sophisicated lexer that checks for `>>` and `>``
    result.logAddTok(str, OTkRawText):
      str.skipUntil({ '>' })

    result.logAddTok(str, OTkTripleAngleClose):
      str.skip('>')
      str.skip('>')
      str.skip('>')


  elif str['<', '<']:
    logAddTok(result, str):
      result.add str.initAdvanceTok(2, OTkDoubleAngleOpen)
    result.logAddTok(str, OTkRawtext, str.skipUntil({ '>' }))
    result.logAddTok(str, OTkDoubleAngleClose):
      str.skip('>')
      str.skip('>')

  elif str[+1, Digits]:
    logAddTok(result, str):
      result.add initTok(
        OTkAngleTime, str.scanSlice('<', @{'>', '\n'}, '>'))

    if str["--"]:
      logAddTok(result, str):
        result.add initTok(OTkTimeDash, str.scanSlice("--"))

      logAddTok(result, str):
        result.add initTok(
          OTkAngleTime, str.scanSlice('<', @{'>', '\n'}, '>'))

      result.add maybeTimeRange(str, lexConf)

  else:
    # `<placeholder>` vs `<2020-04-04>`
    logAddTok(result, str):
      result.add str.initAdvanceTok(1, OTkAngleOpen)

    logAddTok(result, str):
      result.add str.initTok(
        str.asSlice str.skipUntil({ '>' }), OTkRawText)

    logAddTok(result, str):
      result.add str.initTok(str.asSlice str.skip('>'), OTkAngleClose)


proc lexTime*(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =

  if str['<']:
    logAddTok(result, str):
      result.add lexAngle(str, lexConf)

  elif str['[']:
    logAddTok(result, str):
      result.add initTok(
        OTkBracketTime, str.scanSlice('[', @{']', '\n'}, ']'))

    if str["--"]:
      logAddTok(result, str):
        result.add initTok(OTkTimeDash, str.scanSlice("--"))

      logAddTok(result, str):
        result.add initTok(
          OTkBracketTime, str.scanSlice('[', @{']', '\n'}, ']'))

      result.add maybeTimeRange(str, lexConf)

  else:
    raise newUnexpectedCharError(str)



proc lexLinkTarget*(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  # Link protocols whose links are better kept intact
  if str["https"] or str["http"]:
    logAddTok(result, str):
      result.add str.initTok(str, OTkLinkFull)

  # File protocl
  elif str["file"] or
       str["attachment"] or
       str["docview"] or
       str['/'] or
       str["./"]:

    if str['.'] or str['/']:
      logAddTok(result, str):
        result.add initFakeTok(str, OTkLinkProtocol, "file")

    else:
      result.logAddTok(str, OTkLinkProtocol):
        str.skipTo(':')
      str.skip(':')

    result.logAddTok(str, OTkLinkTarget):
      while ?str and not str["::"]:
        str.next()

    if str["::"]:
      result.logAddTok(str, OTkLinkExtraSeparator):
        str.next(2)

      result.logAddTok(str, OTkLinkExtra):
        str.skipPastEOF()

  # Simple, non-URI protocols that don't have trailing extra separator
  # parametrization and all other cases (including user-provided link
  # templates)
  else:
    if str.hasAhead({':'}):
      result.logAddTok(str, OTkLinkProtocol):
        str.skipTo(':')

      str.skip(':')
      result.logAddTok(str, OTkLinkTarget):
        str.skipPastEOF()

    else:
      result.logAddTok(str, OTkLinkInternal):
        str.skipPastEOF()


proc lexBracket*(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  if str["[["]:
    logAddTok(result, str):
      result.add str.scanTok(OTkLinkOpen, '[')

    block link_token:
      logAddTok(result, str):
        result.add str.scanTok(OTkLinkTargetOpen, '[')
      logAddTok(result, str):
        result.add str.asSlice(
          str.skipUntil({']'})).asVar().lexLinkTarget(lexConf)

      logAddTok(result, str):
        result.add str.scanTok(OTkLinkTargetClose, ']')

    block description_token:
      if str['[']:
        logAddTok(result, str):
          result.add str.scanTok(OTkLinkDescriptionOpen, '[')
        var desc = str.asSlice:
          var count = 0
          while ?str and (str[] != ']' or (0 < count)):
            if str['[']: inc count
            if str[']']: dec count
            str.next()

        while ?desc:
          result.add lexText(desc, lexConf)

        logAddTok(result, str):
          result.add str.scanTok(OTkLinkDescriptionClose, ']')

    logAddTok(result, str):
      result.add str.scanTok(OTkLinkClose, ']')

  elif str["[fn:"]:
    result.add str.scanTok(OTkFootnoteStart, '[')
    str.skip("fn")
    if str["::"]:
      logAddTok(result, str):
        result.add str.scanTok(OTkDoubleColon, "::")

      result.addExpandTok(str, OTkText):
        str.skipTo(']')

    else:
      result.logAddTok(str, OTkColon): str.skip(':')
      result.logAddTok(str, OTkIdent):
        str.skipTo(']')

    logAddTok(result, str):
      result.add str.scanTok(OTkFootnoteEnd, ']')

  else:
    result.add trySpecific(str, OTkPunctuation, 1, lexTime)


const TextChars = MaybeLetters + Digits + { '.', ',', '-'}
proc lexTextChars*(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  var isStructure: bool = false
  if str[rei"src[_-]?\w+(\[|\{)"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("src"), OTkSrcOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf
      logAddTok(result, str):
        result.add str.initTok(
          str.asSlice str.skipWhile(IdentChars), OTkSrcName)
      if str['[']:
        logAddTok(result, str):
          result.add str.initTok(
            str.asSlice(str.skipBalancedSlice({'['}, {']'}), 1, -2),
            OTkSrcArgs
          )

      logAddTok(result, str):
        result.add str.initTok(
          str.asSlice(str.skipBalancedSlice(
            openChars = {'{'},
            closeChars = {'}'},
            endChars = {}
          ), 1, -2),
          OTkSrcBody
        )

      logAddTok(result, str):
        result.add str.initTok(OTkSrcClose)

      isStructure = true

    else:
      str.setPos(pos)


  elif str[rei"call[_-]?\w+(\[|\{)"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("call"), OTkCallOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf
      logAddTok(result, str):
        result.add str.initTok(
          str.asSlice str.skipWhile(IdentChars), OTkCallName)

      if str['[']:
        logAddTok(result, str):
          result.add str.initTok(
            str.asSlice(str.skipBalancedSlice({'['}, {']'}), 1, -2),
            OTkCallInsideHeader
          )

      logAddTok(result, str):
        result.add str.initTok(
          str.asSlice(str.skipBalancedSlice({'('}, {')'}), 1, -2),
          OTkCallArgs
        )

      logAddTok(result, str):
        result.add str.initTok(OTkCallClose)

      isStructure = true

    else:
      str.setPos(pos)

  elif str["https://"] or str["http://"]:
    result.logAddTok(str, OTkRawUrl):
      str.skipUntil(Whitespace)


  if not isStructure:
    var allUp = true

    str.startSlice()
    while ?str and str[TextChars + {'-'}]:
      if not str[HighAsciiLetters]:
        allUp = false

      str.next()

    logAddTok(result, str):
      result.add str.initSliceTok(if allUp: OTkBigIdent else: OTkWord)


proc lexParenArguments(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  logAddTok(result, str):
    result.add str.initAdvanceTok(1, OTkParOpen)

  while not str[')']:
    # Read argument until the first comma or closing parent
    result.logAddTok(str, OTkRawText):
      # TODO handle quoted strings and escaped commas
      str.skipUntil({',', ')'})

    # maybe lex comma
    if str[',']:
      logAddTok(result, str):
        result.add str.initAdvanceTok(1, OTkComma)

    # optional space, not significant for argument passing
    str.space()

  logAddTok(result, str):
    result.add str.initAdvanceTok(1, OTkParClose)


proc lexText*(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  ## Lex single text entry starting at current position
  const
    NonText = TextLineChars - AsciiLetters - Utf8Any + {
      '\n', '/'
    }

  case str[]:
    of TextChars:
      result.add str.lexTextChars(lexConf)

    of '\n':
      logAddTok(result, str):
        result.add str.initAdvanceTok(1, OTkNewline)

    of ' ':
      result.logAddTok(str, OTkSpace):
        while ?str and str[' ']:
          str.next()

    of '#':
      proc rec(str: var PosStr): seq[OrgToken] =
        result.logAddTok(str, OTkHashTag):
          if str['#']:
            str.skip('#')

          str.skipWhile(IdentChars)

        while str["##"] and not str["##["]:
          result.logAddTok(str, OTkHashTagSub):
            str.skip('#')

          result.logAddTok(str, OTkHashTag):
            str.skip('#')
            str.skipWhile(IdentChars)

        if str["##["]:
          result.logAddTok(str, OTkHashTagSub):
            str.skip('#')

          result.logAddTok(str, OTkHashTagOpen):
            str.skip('#')
            str.skip('[')

          while ?str and not str[']']:
            result.add rec(str)
            str.space()
            if str[',']:
              result.logAddTok(str, OTkComma):
                str.skip(',')

              str.space()

          result.logAddTok(str, OTkHashTagClose):
            str.skip(']')

      result.add rec(str)

    of '@':
      const AtChars = IdentChars + Utf8Any
      if str[+1, AtChars]:
        result.logAddTok(str, OTkAtMention):
          str.skip('@')
          str.skipWhile(AtChars)

      else:
        result.logAddTok(str, OTkPunctuation):
          str.next()

    of '$':
      # Try parsing inline latex delimited by the `$` characters. If
      # parsing failed bail out
      var
        tmp = str
        buf: seq[OrgToken]

      try:
        if tmp[+1, '$']:
          buf.add tmp.scanTok(OTkDollarOpen, '$', '$')
          tmp.startSlice()
          var hasEnd = false
          while ?tmp and not hasEnd:
            while ?tmp and not tmp['$']:
              tmp.next()

            if tmp['$', '$']:
              buf.add tmp.initTok(tmp.popSlice(), OTkLatexInlineRaw)
              hasEnd = true

            else:
              raise newImplementError()

          buf.add tmp.scanTok(OTkDollarClose, '$', '$')

        else:
          buf.add tmp.scanTok(OTkDollarOpen, '$')
          buf.add tmp.initTok(
            tmp.asSlice tmp.skipUntil({'$'}),
            OTkLatexInlineRaw)

          buf.add tmp.scanTok(OTkDollarClose, '$')

        result.add buf
        str = tmp

      except UnexpectedCharError:
        result.logAddTok(str, OTkPunctuation):
          str.skipWhile({'$'})

    of '\\':
      case str[+1]:
        of '[', '(':
          let inline = str[+1, {'('}]
          if inline:
            logAddTok(result, str):
              result.add str.initTok(OTkLatexParOpen, str.scanSlice(r"\("))

          else:
            logAddTok(result, str):
              result.add str.initTok(OTkLatexBraceOpen, str.scanSlice(r"\["))

          result.logAddTok(str, OTkLatexInlineRaw):
            while not str[tern(inline, r"\)", r"\]")]:
              str.next()

          if inline:
            logAddTok(result, str):
              result.add str.initTok(OTkLatexParClose, str.scanSlice(r"\)"))

          else:
            logAddTok(result, str):
              result.add str.initTok(OTkLatexBraceClose, str.scanSlice(r"\]"))

        of OMarkupChars:
          result.logAddTok(str, OTkEscaped):
            str.next(2)

        of IdentStartChars - {'_'}:
          result.logAddTok(str, OTkSymbolStart):
            str.skip({'\\'})

          result.logAddTok(str, OTkIdent):
            str.skipWhile(IdentChars)

          if str['[']:
            result.logAddTok(str, OTkMetaBraceOpen):
              str.skip('[')

            result.logAddTok(str, OTkMetaBraceBody):
              str.skipBalancedSlice(
                {'['},
                {']'},
                skippedStart = true,
                consumeLast = false
              )

            result.logAddTok(str, OTkMetaBraceClose):
              str.skip(']')

          while str['{']:
            result.logAddTok(str, OTkMetaArgsOpen):
              str.skip('{')

            result.logAddTok(str, OTkMetaArgsBody):
              str.skipBalancedSlice(
                {'{'},
                {'}'},
                skippedStart = true,
                consumeLast = false,
              )

            result.logAddTok(str, OTkMetaArgsClose):
              str.skip('}')

        of '\\':
          result.logAddTok(str, OTkDoubleSlash):
            str.skip('\\')
            str.skip('\\')

        else:
          result.logAddTok(str, OTkEscaped):
            str.next(2)

    of '~', '`', '=':
      let start = str[]
      if str[+1, start]:
        # Inline verbatim text
        logAddTok(result, str):
          result.add str.initTok(
            str.popPointSlice(advance = 2), markupTable[start].inline)

        result.logAddTok(str, OTkRawText):
          while not str[start, start]:
            str.next()

        logAddTok(result, str):
          result.add str.initTok(
            str.popPointSlice(advance = 2),
            markupTable[start].inline)

      else:
        # Open/close pair in the text
        if str[-1, NonText] or str.atStart():
          logAddTok(result, str):
            result.add str.initTok(
              str.popPointSlice(),
              markupTable[start].start)

          result.logAddTok(str, OTkRawText):
            str.skipTo(start)

          if str[+1, NonText] or str.beforeEnd():
            logAddTok(result, str):
              result.add str.initTok(
                str.popPointSlice(), markupTable[start].finish)

        else:
          result.logAddTok(str, OTkPunctuation):
            str.next()

    of '<':
      result.add trySpecific(str, OTkPunctuation, 1, lexAngle)

    of markupKeys - { '<', '~', '`', '=' }:
      let ch = str[]
      let (kOpen, kClose, kInline) = markupTable[ch]

      # Depending on the surrounding context, pop markup character or skip
      # part of the word. Things `in//line//` is a double 'inline',
      # `thing/or/t` is a `["thing", "/", "or", "/", "t"]` word sequence.
      if str[+1, ch]:
        logAddTok(result, str):
          result.add str.initTok(
            str.popPointSlice(advance = 2), kInline)

      elif str[-1, NonText] or str.atStart():
        logAddTok(result, str):
          result.add str.initTok(str.popPointSlice(), kOpen)

      elif str[+1, NonText] or str.beforeEnd():
        logAddTok(result, str):
          result.add str.initTok(str.popPointSlice(), kClose)

      else:
        logAddTok(result, str):
          result.add str.initTok(str.popPointSlice, OTkWord)

    of '[':
      result = lexBracket(str, lexConf)

    of '(':
      result.logAddTok(str, OTkParOpen):
        str.next()

    of ')':
      result.logAddTok(str, OTkParClose):
        str.next()

    of ':':
      result.logAddTok(str, OTkColon):
        str.next()

    of '\'', '?', '!', '%', ']', '|', '&', ';', '}', '>':
      result.logAddTok(str, OTkPunctuation):
        str.next()

    of '{':
      if str["{{{"]:
        logAddTok(result, str):
          result.add str.initAdvanceTok(3, OTkMacroOpen)
        logAddTok(result, str):
          result.add str.initTok(
            asSlice(str, inWhile(?str and
                                 not str['('] and
                                 not str["}}}"],
                                 str.next())),
            OTkIdent
          )

        if str['(']:
          logAddTok(result, str):
            result.add str.lexParenArguments(lexConf)

        if ?str:
          logAddTok(result, str):
            result.add str.initAdvanceTok(3, OTkMacroClose)

      else:
        logAddTok(result, str):
          result.add str.initAdvanceTok(1, OTkMaybeWord)

    of '^':
      result.logAddTok(str, OTkCircumflex):
        str.next()

    else:
      raise newUnexpectedCharError(str)



proc lexProperties(
    id: PosStr, str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  logAddTok(result, str):
    result.add initTok(id, OTkColonProperties)
  var hasEnd = false

  while ?str and not hasEnd:
    str.space()
    var isAdd = false
    let id = str.scanSlice(':', *\DId, ?'+' -> (isAdd = true), ':')
    if id.strValNorm() == ":end:":
      hasEnd = true
      logAddTok(result, str):
        result.add initTok(id, OTkColonEnd)

    else:
      logAddTok(result, str):
        result.add initTok(
          id, tern(isAdd, OTkColonAddIdent, OTkColonIdent))

      if str[IdentStartChars]:
        result.logAddTok(str, OTkIdent):
          while ?str and str[DashIdentChars + {'/'}]:
            str.next()

        str.skip(':')

      str.space()

      result.logAddTok(str, OTkRawProperty):
        str.skipToEol()

      str.skip('\n')

proc lexDescription(
    id: PosStr, str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  logAddTok(result, str):
    result.add initTok(id, OTkColonDescription)
  str.startSlice()

  var hasEnd = false
  while ?str and not hasEnd:
    while ?str and not str[rei":end:"]:
      str.next()

    logAddTok(result, str):
      result.add initTok(str.popSlice(), OTkText)

    let id = str.asSlice:
      str.skip({':'})
      str.skipWhile(IdentChars)
      str.skip(':')

    logAddTok(result, str):
      result.add initTok(id, OTkColonEnd)
    hasEnd = true


proc lexLogbook(
    id: PosStr, str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  logAddTok(result, str):
    result.add initTok(id, OTkColonLogbook)
  str.startSlice()
  var hasEnd = false
  # REFACTOR weird implementation, not sure if there is a need for
  # two nested loops.
  while ?str and not hasEnd:
    while ?str and not str[rei":end:"]:
      str.next()

    logAddTok(result, str):
      result.add initTok(str.popSlice(), OTkRawLogbook)

    let id = str.asSlice:
      str.skip({':'})
      str.skipWhile(IdentChars)
      str.skip(':')

    logAddTok(result, str):
      result.add initTok(id, OTkColonEnd)

    hasEnd = true



proc lexDrawer(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  var strEnded = false

  while ?str and not strEnded:
    str.space()
    let id = str.scanSlice(':', *\Id, ':')
    str.skip({'\n'})
    case id.strValNorm():
      of ":properties:": result.add lexProperties(id, str, lexConf)
      of ":logbook:": result.add lexLogbook(id, str, lexConf)
      of ":description:": result.add lexDescription(id, str, lexConf)
      else:
        raise newImplementKindError(id.strValNorm(), $str)

    var ahead = str
    ahead.space()
    if ahead.trySkip('\n'):
      ahead.space()
      if not ahead[':']:
        strEnded = true
        str = ahead

    if not strEnded:
      str.skipWhile({'\n'})

proc lexSubtreeTodo(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =

  var tmp = str
  tmp.startSlice()
  tmp.skipWhile(HighAsciiLetters)
  if tmp[' ']:
    logAddTok(result, tmp):
      result.add tmp.initTok(tmp.popSlice(), OTkSubtreeTodoState)
    str = tmp

proc lexSubtreeUrgency(
    str: var PosStr, lexconf: LexConf): seq[OrgToken] {.lexx.} =

  if str["[#"]:
    str.pushSlice()
    str.next(2)

    # org-mode only supports one-letter importance markers, but I
    # think it is too limiting, so this implementation also handles
    # `[#URGENT]`,
    str.skip(HighAsciiLetters) # But at least one letter has to be specified
    str.skipWhile(HighAsciiLetters)
    str.skip({']'})

    logAddTok(result, str):
      result.add str.initTok(str.popSlice(), OTkSubtreeUrgency)

    str.skipWhile({' '})

proc lexSubtreeTitle(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  var
    body = str.asSlice(str.skipToEol())
    headerTokens: seq[OrgToken]

  body.skipToEof()
  if body[':']:
    body.back()

    var tagEnded = false
    while ?body and not tagEnded:
      let finish = body.getPos()
      while ?body and body[IdentChars + {'#', '@'}]:
        body.back()

      let start = body.getPos(+1)
      body.skipBack({':'})
      headerTokens.add body.initTok(
        body.sliceBetween(start, finish), OTkSubtreeTag)

      if body[' ']:
        tagEnded = true

    while body[' ']:
      body.back()

  if body[']']:
    var tmp = body
    try:
      # Try lex trailing subtree completion from the end of the input.
      let finish = tmp.getPos()
      tmp.skipBack({']'})
      tmp.skipBack(Digits)
      while tmp[Digits]:
        tmp.back()

      if str['%']:
        tmp.back()

      else:
        tmp.skipBack({'/'})
        tmp.skipBack(Digits)
        while tmp[Digits]:
          tmp.back()

      tmp.skipBack({'['})

      let start = tmp.getPos(+1)
      body = tmp

      headerTokens.add body.initTok(
        str.sliceBetween(start, finish), OTkSubtreeCompletion)

      while body[' ']:
        body.back()

    except UnexpectedCharError:
      # Ignore the error if subtree completion is not correct - later on it
      # can will be lexed as a regular subtree header element.
      discard


  block:
    var finish = body.getPos()
    body.skipToSof()
    let start = body.getPos()
    var slice = str.sliceBetween(start, finish)
    headerTokens.add body.initTok(slice, OTkText)


  result.add headerTokens.reversed()

proc lexSubtreeTimes(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =

  str.space()
  var hadTimes = false
  while str[HighAsciiLetters]:
    hadTimes = true
    var times = str
    times.space()
    let tag = times.asSlice times.skipWhile(HighAsciiLetters)
    if tag.strValNorm() in ["deadline", "closed", "scheduled"]:
      logAddTok(result, tag):
        result.add initTok(tag, OTkSubtreeTime)
      times.skip({':'})
      times.space()
      logAddTok(result, times):
        result.add times.lexTime(lexConf)
      times.space()
      str = times

    else:
      break

    times.space()

  if hadTimes:
    str.skip({'\n'})


proc lexSubtree(str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  # pprintStackTrace()
  logAddTok(result, str):
    result.add str.initTok(str.asSlice str.skipWhile({'*'}), OTkSubtreeStars)
  str.space()
  result.add str.lexSubtreeTodo(lexConf)
  str.space()
  result.add str.lexSubtreeUrgency(lexConf)
  str.space()
  result.add str.lexSubtreeTitle(lexConf)
  discard str.trySkip('\n')
  result.add str.lexSubtreeTimes(lexConf)
  var drawer = str
  drawer.space()
  if drawer[':']:
    result.add lexDrawer(drawer, lexConf)
    str = drawer

  logAddTok(result, str):
    result.add str.initFakeTok(OTkSubtreeEnd)

proc lexSourceBlockContent(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  while ?str:
    if str["<<"]:
      var
        failedAt = -1
        tmp = str
        tmpRes: seq[OrgToken]

      block try_tangle:
        tmpRes.logAddTok(tmp, OTkDoubleAngleOpen):
          tmp.skip("<<")

        if tmp[IdentChars]:
          tmpRes.logAddTok(tmp, OTkIdent):
            tmp.skipWhile(IdentChars)

        else:
          failedAt = tmp.pos
          break try_tangle

        if tmp['(']:
          tmpRes.add str.lexParenArguments(lexConf)

        if tmp[">>"]:
          tmpRes.logAddTok(tmp, OTkDoubleAngleClose):
            tmp.skip(">>")

        else:
          failedAt = tmp.pos
          break try_tangle

      if failedAt != -1:
        result.logAddTok(str, OTkCodeText):
          while str.pos < failedAt:
            str.next()

        str.pos = failedAt

      else:
        result.add tmpRes
        str = tmp

    elif str["(refs:"]:
      result.logAddTok(str, OTkParOpen):
        str.skip('(')

      result.logAddTok(str, OTkIdent):
        str.skip("refs")

      result.logAddTok(str, OTkColon):
        str.skip(':')

      result.logAddTok(str, OTkIdent):
        str.skipWhile(IdentChars + {'-'})

      result.logAddTok(str, OTkParClose):
        str.skip(')')

    elif str['\n']:
      result.logAddTok(str, OTkNewline):
        str.next()

    else:
      result.logAddTok(str, OTkCodeText):
        while ?str and
              not (str["<<"] or str["(refs:"] or str['\n']):
          str.next()


proc lexCommandContent(
    str: var PosStr,
    kind: OrgCommandKind, lexConf: LexConf): seq[OrgToken] {.lexx.} =

  logAddTok(result, str):
    result.add str.initTok(OTkCommandContentStart)

  case kind:
    of ockBeginQuote, ockBeginCenter, ockBeginAdmonition:
      result.logAddTok(str, OTkText):
        str.skipPastEof()

    of ockBeginExample:
      result.logAddTok(str, OTkRawText):
        str.skipPastEof()

    of ockBeginDynamic:
      str.space()
      result.logAddTok(str, OTkText):
        str.skipPastEof()

    of ockBeginSrc:
      str.space()
      logAddTok(result, str):
        result.add str.initFakeTok(OTkCodeContentBegin)
      var code = str.lexSourceBlockContent(lexConf)
      if code.last() of OTkCodeText and
         code.last().strVal().allIt(it in {' '}):
        discard code.pop

      if code.last() of OTkNewline:
        discard code.pop()

      result.add code
      logAddTok(result, str):
        result.add str.initFakeTok(OTkCodeContentEnd)

    else:
      raise newUnexpectedKindError(kind)

  logAddTok(result, str):
    result.add str.initTok(OTkCommandContentEnd)

proc lexDelimited(
    str: var PosStr,
    start, finish: tuple[text: char, kind: OrgTokenKind],
    middle: OrgTokenKind,
    lexConf: LexConf
  ): seq[OrgToken] =

  result.logAddTok(str, start.kind):
    str.skip(start.text)

  result.logAddTok(str, middle):
    while ?str and not str[finish.text]:
      if str['\\']:
        str.next(2)

      else:
        str.next()

  if ?str:
    result.logAddTok(str, finish.kind):
      str.next()

proc lexCommandArguments(
    str: var PosStr,
    kind: OrgCommandKind, lexConf: LexConf): seq[OrgToken] {.lexx.} =

  let (wrapStart, wrapEnd) = case kind:
    # Most commands use DSL as arguments, but for title and caption regular
    # paragraph must be used.
    of ockTitle, ockCaption:
      (OTkParagraphStart, OTkParagraphEnd)

    else:
      (OTkCommandArgumentsBegin, OTkCommandArgumentsEnd)

  logAddTok(result, str):
    result.add str.initTok(wrapStart)

  proc lexKeyValue(
      str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
    while ?str:
      case str[]:
        of '-':
          result.logAddTok(str, OTkCommandFlag):
            str.skipWhile({'-'} + IdentChars)

        of ':':
          result.logAddTok(str, OTkCommandKey):
            str.skipWhile(IdentChars + {'-', ':'})

        of ' ':
          str.space()

        else:
          result.logAddTok(str, OTkCommandValue):
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
        result.add lexText(str, lexConf)

    of ockOptions:
      while ?str:
        case str[]:
          of {'\'', '*', '|', ':', '<', '\n', '^'}:
            result.logAddTok(str, OTkRawText):
              str.next()

          of ' ':
            str.space()

          else:
            result.logAddTok(str, OTkRawText):
              while ?str and not str[HorizontalSpace]:
                str.next()

    of ockCaption:
      result.logAddTok(str, OTkText):
        str.skipPastEof()

    of ockCall:
      # FIXME lex call arguments with a proper scheme, similar to the
      # inline call handling for macros.
      str.space()
      result.logAddTok(str, OTkCallName):
        str.skipWhile(IdentChars)

      if str['[']:
        result.logAddTok(str, OTkCallInsideHeader):
          str.skipBalancedSlice({'['}, {']'})

      result.logAddTok(str, OTkCallArgs):
        str.skipBalancedSlice({'('}, {')'})

      if ?str:
        result.logAddTok(str, OTkRawText):
          str.skipPastEof()

    of ockBeginSrc:
      result.logAddTok(str, OTkWord):
        str.skipWhile(IdentChars)

      str.space()
      result.add lexKeyValue(str, lexConf)

    of ockBeginTable, ockAttrHtml:
      result.add lexKeyValue(str, lexConf)

    of ockBeginDynamic:
      result.logAddTok(str, OTkWord):
        str.skipWhile(IdentChars)

      str.space()
      result.add lexKeyValue(str, lexConf)

    of ockHeader:
      str.space()
      result.add lexKeyValue(str, lexConf)

    of ockAuthor, ockCreator, ockLanguage:
      str.space()
      result.logAddTok(str, OTkRawText):
        str.skipPastEof()

    of ockProperty:
      str.space()
      result.logAddTok(str, OTkIdent):
        str.skipUntil({' ', ':'})


      if str[':']:
        result.logAddTok(str, OTkColon):
          str.next()

        result.logAddTok(str, OTkIdent):
          str.skipUntil({' '})

      str.space()
      result.logAddTok(str, OTkRawProperty):
        str.skipPastEof()

    of ockFiletags:
      while str[':']:
        str.skip(':')
        if ?str:
          result.logAddTok(str, OTkSubtreeTag):
            while ?str and not str[':']:
              next(str)

    of ockInclude:
      str.space()
      if str['"']:
        result.add str.lexDelimited(
          start = ('"', OTkQuoteOpen),
          finish = ('"', OTkQuoteClose),
          middle = OTkRawText,
          lexConf = lexConf
        )

      else:
        result.logAddTok(str, OTkRawText):
          while ?str and not str[HorizontalSpace]:
            str.next()

      result.add lexKeyValue(str, lexConf)

    of ockName,
       ockLatexClass,
       ockLatexCompiler,
       ockBeginAdmonition:
      str.space()
      result.logAddTok(str, OTkIdent):
        str.skipPastEof()

    of ockColumns,
       ockBeginExample,
       ockResults,
       ockLatexHeader,
       ockHtmlHead,
       ockBeginCenter,
       ockLatexClassOptions:
      result.logAddTok(str, OTkRawText):
        str.skipPastEof()

    else:
      raise newUnexpectedKindError(kind, $str)

      result.logAddTok(str, OTkRawText):
        str.skipPastEof()

  logAddTok(result, str):
    result.add str.initTok(wrapEnd)

proc lexCommandBlock(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  # Store position of the command start - content be dedented or indented
  # arbitrarily, so `#+begin_src` starting at column 2 might have content
  # that starts on the column 0.
  let column = str.column
  logAddTok(result, str):
    result.add str.initAdvanceTok(2, OTkCommandPrefix)
  let id = str.asSlice(): str.skipWhile(OCommandChars)

  if id.strValNorm().startsWith("begin"):
    logAddTok(result, str):
      result.add initTok(id, OTkCommandBegin)
    let sectionName = id.strVal().normalize().dropPrefix("begin")
    let kind = id.strVal().classifyCommand()
    if kind == ockBeginDynamic:
      str.skip(':')

    str.space()

    var arguments = str.asSlice(str.skipPastEol(), -2)
    result.add lexCommandArguments(arguments, kind, lexConf)

    var found = false
    str.pushSlice()
    while not found and ?str:
      while ?str and not(str.column == column and str["#+"]):
        str.next()

      if not ?str:
        echov str
        echov str.baseStr[].len()

      assert(?str, $str)
      let prefix = str.asSlice str.next(2)
      let id: PosStr = str.asSlice str.skipWhile(OCommandChars)
      if id.strVal().normalize() == "end" & sectionName:
        found = true
        var slice = str.popSlice(-(
            1 #[ default offest ]# +
            id.strVal().len() #[ `end_<xxx>` ]# +
            3 #[ `#+` and trailing newline ]#
        ))

        result.add slice.lexCommandContent(kind, lexConf)
        logAddTok(result, str):
          result.add initTok(prefix, OTkCommandPrefix)
        logAddTok(result, str):
          result.add initTok(id, OTkCommandEnd)

    if kind == ockBeginDynamic:
      str.skip(':')

  else:
    logAddTok(result, str):
      result.add initTok(id, OTkLineCommand)
    logAddTok(result, str):
      result.add str.initAdvanceTok(1, OTkColon, {':'})
    str.space()
    var args = str.asSlice(): str.skipToEol()
    result.add lexCommandArguments(
      args, id.strVal().classifyCommand(), lexConf)

    if ?str:
      str.skip('\n')

proc isFirstOnLine*(str: var PosStr): bool =
  # If string is positioned at the very first element in the list, test for
  # newline directly
  if str[-1] in Newline + { '\x00' }:
    return true

  # Then start moving backwards if there is an empty space.
  var pos = 0
  while str[pos] in HorizontalSpace:
    dec pos

  return str[pos] in Newline + {'\x00'}

proc atLogClock(str: var PosStr): bool =
  ## Check if the string is positioned at the start of a logbook `CLOCK:`
  ## entry.
  let
    ahead = str.getAllFromPos(stopChars = {'['})
    space = ahead.find('C')

  if 0 <= space:
    for ch in ahead[0 ..< space]:
      if ch != ' ':
        return false

    let content = ahead[space .. ^1]
    result = content.startsWith("CLOCK:")


proc atConstructStart*(str: var PosStr): bool =
  ## Check if string is positioned at the start of toplevel language
  ## construct.
  if not str.isFirstOnLine():
    return false

  if str.getIndent() == 0 and str['*']:
    # One or more leading asterisks followed by space
    var shift = 0
    while str[shift] in { '*' }:
      inc shift

    result = str[shift] in { ' ' }

  else:
    result = (
      # Command start
      (str["#+"]) or
      # Separator start
      (str["---"])
    )

proc lexParagraph*(str: var PosStr, lexConf: LexConf): seq[OrgToken]

proc skipIndents(
    state: var HsLexerStateSimple,
    str: var PosStr,
    lexConf: LexConf
  ): seq[OrgToken] {.lexx.} =

  let skipped = state.skipIndent(str)
  for indent in skipped:
    case indent:
      of likIncIndent:  result.add str.initTok(OTkIndent)
      of likDecIndent:  result.add str.initTok(OTkDedent)
      of likSameIndent: result.add str.initTok(OTkSameIndent)
      of likNoIndent:   result.add str.initTok(OTkNoIndent)
      of likEmptyLine:  raise newImplementError()

const ListStart = {'-', '+', '*'} + Digits + AsciiLetters

proc tryListStart(
    str: var PosStr,
    lexConf: LexConf): seq[OrgToken] {.lexx.} =

  if str.atConstructStart():
    return @[]

  ## Attempt to parse list start dash

  var tmp = str
  if tmp[{'-', '+'}] or (0 < tmp.getIndent() and tmp[{'*'}]):
    logAddTok(result, tmp):
      result.add tmp.initTok(
        tmp.asSlice tmp.skip(ListStart), OTkListDash)

    if not tmp.trySkip(' '): return @[]
    tmp.space()

  elif tmp[Digits + AsciiLetters]:
    logAddTok(result, tmp):
      result.logAddTok(tmp, OTkListDash):
        # HACK only handle lists that start with 1-3 characters, `AAAAA.`
        # won't be hadled. This is a temporary workaround to allow
        # `regularWord.` at the start of the text. Ideally list detection
        # should consider the context.
        # for _ in 0 .. 2:
        #
        # NOTE seems like org-mode only handles list elements with a single
        # starting letter and I don't think there is any reason to
        # implement a different handlig mode.

        if tmp[Digits + AsciiLetters]:
          tmp.next()

        else:
          return @[]

    if tmp[')'] or tmp['.']: tmp.next() else: return @[]
    if tmp[' ']: tmp.next() else: return @[]

  else:
    if tmp[{'-', '+', '*'}]: tmp.next() else: return @[]
    if tmp[' ']: tmp.next() else: return @[]

  str = tmp

proc lexListItems(
    str: var PosStr,
    state: var  HsLexerStateSimple,
    lexConf: LexConf): seq[OrgToken]


proc listAhead(str: var PosStr, lexConf: LexConf): bool =
  if not str.isFirstOnLine():
    return false

  if globalTick() mod 12000 == 0: echov $str
  let init = $str
  var str = str
  str.space()
  if str[] in ListStart:
    result = not tryListStart(str, lexConf).empty()

proc lexListItem(
    str: var PosStr,
    indent: int,
    state: var  HsLexerStateSimple,
    lexConf: LexConf
  ): seq[OrgToken] {.lexx.} =

  ## Lex head starting from current position onwards. `indent` is the
  ## indentation of the original list prefix -- dash, number or letter.
  if str[rei"\[[Xx -]\]"]:
    logAddTok(result, str):
      result.add str.scanTok(OTkCheckbox, '[', {'X', 'x', ' ', '-'}, ']')
    str.space()

  # create slice for the whole content of the list item
  block:
    var
      tmp = str
      buf = @[
        initFakeTok(tmp, OTkListDescOpen),
        initFakeTok(tmp, OTkParagraphStart)
      ]

    while ?tmp and not tmp[Newline]:
      if tmp[':'] and tmp["::"]:
        buf.add initFakeTok(tmp, OTkParagraphEnd)
        buf.add initFakeTok(tmp, OTkListDescClose)
        buf.logAddTok(tmp, OTkDoubleColon):
          tmp.skip("::")
        str = tmp
        result.add buf
        break

      else:
        buf.add lexText(tmp, lexConf)

  str.startSlice()
  var atEnd = false
  # extend slice until new list start is not found - either via new
  # nested item or by indentation decrease.
  while ?str and not atEnd:
    # Special handlig of `CLOCK:` entries in the subtree logging drawer to
    # make sure the content is skipped in the right place.
    if str.atLogClock():
      str.next()
      atEnd = true

    elif str.atConstructStart() and str.getIndent() <= indent:
      # If we are at the language construct start and it is placed at the
      # same level as prefix dash, treat it as list end
      atEnd = true

    elif str.listAhead(lexConf):
      # check if we are at the start of the new list - if we are, stop
      # parsing completely and apply all withheld lexer changes,
      # otherwise don't touch `atEnd` in order to continue parsing.
      atEnd = true

    else:
      block:
        # Two empty lines after list items should be treated as list
        # separator.
        var testTwoSpace = str
        testTwoSpace.space()
        if testTwoSpace[Newline]:
          testTwoSpace.next()
          testTwoSpace.space()
          if testTwoSpace[Newline]:
            atEnd = true

      if not atEnd:
        var testIndent = str
        # go to the start of the next line
        testIndent.skipPastEol()
        while testIndent.trySkipEmptyLine(): discard
        # Decide based on the indentation what to do next indentation
        # decreased, end of the list item
        if testIndent.getIndent() < indent:
          atEnd = true

        str.skipPastEol()

  var slice = str.popSlice(-1)
  while str.atAbsolute(slice.absEnd()) == '\n':
    slice.decEnd()

  logAddTok(result, slice):
    result.add str.initTok(slice, OTkStmtList)

  logAddTok(result, str):
    result.add str.initTok(OTkListItemEnd)

proc lexListItems(
    str: var PosStr,
    state: var  HsLexerStateSimple,
    lexConf: LexConf
  ): seq[OrgToken] {.lexx.} =
  assert(str[] notin {'\n'}, $str)
  while str.listAhead(lexConf) or str.atLogClock():
    # Minor hack -- in order to avoid logic duplication for logbook and
    # non-logbook parsers this function handles both edge cases. The
    # `CLOCK` entries are simply skipped, so the list lexer is not
    # especially troubled by the indentation levels: from the standpoint of
    # `skipIndents()` processing only happens on the well-formed and
    # well-indented list (not sure how often this holds in reality though)
    assert(str[] notin {'\n'}, $str)
    if str.atLogClock():
      result.add str.initFakeTok(OTkListClock)
      str.startSlice()
      str.skipToEol()
      var slice = str.popSlice()
      result.add lexParagraph(slice, lexConf)
      str.next()
      result.add str.initFakeTok(OTkListItemEnd)

    else:
      result.add state.skipIndents(str, lexConf)
      # List start detection should handle several edge cases that are hard
      # to distinguish from each other, so first lexing is /tried/, on
      # success all changes are applied, on failure entry is processed like a
      # normal text element, without going into deeper nesting levels.
      let indent = str.column
      var tmp = str
      let tokens = tryListStart(tmp, lexConf)
      if tokens.empty():
        result.add lexParagraph(str, lexConf)

      else:
        str = tmp
        result.add tokens
        result.add lexListItem(str, indent, state, lexConf)

  str.skipToEol()

proc lexList(str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  # Create temporary state to lex content of the list
  var state = newLexerState()
  result.add str.initFakeTok(OTkListStart)
  let tokens = str.lexListItems(state, lexConf)
  if not(tokens[0] of OTkSameIndent):
    result.add tokens[0]

  result.add tokens[1 .. ^1]

  while state.hasIndent():
    discard state.popIndent()
    result.add str.initFakeTok(OTkDedent)

  result.add str.initFakeTok(OTKListEnd)


proc lexParagraph*(
    str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  let indent = str.getIndent()
  var ended = false
  str.startSlice()
  while ?str and not ended:
    if str.getIndent() == indent and
       (str.atConstructStart() or
        str.listAhead(lexConf)):
      ended = true

    elif str['\n']:
      str.next()
      if not ?str:
        ended = true

      else:
        case str[]:
          of TextLineChars:
            discard

          of '\n':
            str.next()
            ended = true

          else:
            raise newUnexpectedCharError(str, parsing = "paragraph")

    else:
      str.next()

  let slice = str.popSlice(tern(
    ended,
    # last trailing newline and pargraph separator newline
    tern(?str, tern(str.atConstructStart(), -1, -3), -2),
    -1))

  let tok = str.initTok(slice, OTkText)
  result.add tok

proc lexComment*(str: var PosStr, lexConf: LexConf): seq[OrgToken] {.lexx.} =
  result.logAddTok(str, OTkComment):
    str.skipToEol()

proc lexStructure*(lexConf: LexConf): HsLexCallback[OrgToken] =
  ## Create lexer for toplevel structure of the org document
  proc aux(str: var PosStr): seq[OrgToken] {.lexx.} =
    # This procedure dispatches into toplevel lexer routines that are meant
    # to produce entries for the high-level document structure -
    # paragraphs, lists, subtrees, command blocks and so on.
    case str[]:
      of '#':
        case str[+1]:
          of '+':
            if str[rei"#\+begin[-_]?table"]:
              result = initLexTable(lexConf)(str)

            else:
              result = lexCommandBlock(str, lexConf)

          of IdentChars:
            # Parapgraph starts with the hashtag `#testing`
            result = lexParagraph(str, lexConf)

          of ' ':
            result = lexComment(str, lexConf)

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
            result.add lexSubtree(str, lexConf)

          else:
            # `* subtree` starting on the first line
            result = lexParagraph(str, lexConf)

        else:
          if hasSpace:
            # `__* list item` on a line
            result = lexList(str, lexConf)

          else:
            result = lexParagraph(str, lexConf)

      of '-':
        if str.atConstructStart():
          result.logAddTok(str, OTkTextSeparator):
            str.skipWhile({'-'})

          str.skip('\n')

        else:
          result = lexList(str, lexConf)

      of '\n', ' ':
        if str.listAhead(lexConf):
          result = lexList(str, lexConf)

        else:
          str.skipWhile({' ', '\n'})
          result = str.aux()

      of MaybeLetters, {'~', '['}:
        result = lexParagraph(str, lexConf)

      else:
        # Text starts with inline or display latex math equation,
        # `\symbol`, macro call or any other type of the text.
        result = lexParagraph(str, lexConf)

  return aux


proc lexGlobal*(lexConf: LexConf): HsLexCallback[OrgToken]

proc auxGlobal(token: OrgToken, lexConf: LexConf): seq[OrgToken] =
  ## Recursively lex token that might contain contain complex nested
  ## content.
  var content = initPosStr(token)
  while ?content:
    result.add lexGlobal(lexConf)(content)

proc auxExpand(token: OrgToken, lexConf: LexConf): seq[OrgToken] =
  ## Re-lex 'container' tokens
  case token.kind:
    of OTkText:
      # generic 'text' token was found somewhere in the main structure of
      # the document - list content, `#+caption` element etc. In that
      # context it only had defined boundaries but further lexing was
      # deferred until now, to avoid repeating the same construct dozen
      # times.
      var content = initPosStr(token)
      logAddTok(result, content):
        result.add token.initFakeTok(OTkParagraphStart)

      while ?content:
        result.add lexText(content, lexConf)

      logAddTok(result, content):
        result.add token.initFakeTok(OTkParagraphEnd)

    of OTkRawLogbook:
      var content = initPosStr(token)
      logAddTok(result, content):
        result.add token.initFakeTok(OTkLogbookStart)

      # Logbook is made up of several list entries which in turn (that's
      # why the first pass is constrained to list and second is not
      # constrained to anything) might contain complex nested elements
      while ?content:
        if content.atLogClock():
          result.add auxExpand(
            content.initTok(OTkText, content.asSlice content.skipToEol()),
            lexConf
          )

          # text processing about should not include end of line.
          if content[Newline]:
            content.next()
            # If this is a joined list of log entires skip only newline,
            # otherwise cut all leading spaces to avoid messing up
            # indentation in the list parser.
            if not content.atLogClock():
              content.space()

        else:
          for entry in lexList(content, lexConf):
            result.add auxExpand(entry, lexConf)

      logAddTok(result, content):
        result.add token.initFakeTok(OTkLogbookEnd)

    of OTkContent:
      result.add token.initFakeTok(OTkContentStart)
      # Table might contain any structure, including more complex
      # elements such as lists, code blocks, other tables and so on.
      # This is an imporvement on top of the regular org-mode syntax
      # (although IIUC elisp parser would also allow for structures
      # like these)
      result.add auxGlobal(token, lexConf)

      result.add token.initFakeTok(OTkContentEnd)

    of OTkStmtList:
      result.add token.initFakeTok(OTkStmtListOpen)
      result.add auxGlobal(token, lexConf)
      result.add token.initFakeTok(OTkStmtListClose)

    else:
      result.add token


proc lexGlobal*(lexConf: LexConf): HsLexCallback[OrgToken] =
  ## Lex global structure of the org document

  var structure = lexStructure(lexConf)
  proc aux(str: var PosStr): seq[OrgToken] =
    for token in structure(str):
      result.add auxExpand(token, lexConf)

  return aux
