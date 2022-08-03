## Universal lexer implementation

import
  hmisc/algo/[hlex_base, hparse_base, hstring_algo],
  hmisc/core/all,
  std/algorithm

import
  ./parse_org_common


type
  OrgTokenKind* = enum
    otNone
    otEof

    # === structure tokens begin
    OStCommandPrefix
    OStIdent
    OStLineCommand
    OStCommandBegin ## `#+begin` part of the multiline command.
    ## `begin_<block-type>` is split into two tokens - `begin_` prefix and
    ## `ockBegin<block-type>` section.
    OStCommandEnd



    OStBigIdent
    OStColon
    OStDoubleColon
    OStText
    OStListDash
    OStListPlus
    OStListStar
    OStListItemEnd ## End of the list item
    OStCheckbox ## List or subtree checkbox

    OStSubtreeTodoState
    OStSubtreeImportance ## Subtree importance marker
    OStSubtreeCompletion ## Subtree completion marker
    OStSubtreeStars ## Subtree prefix
    OStSubtreeTag ## Subtree tag
    OStSubtreeTime
    OStAngleTime
    OStDiaryTime
    OStImplicitTime ## You can write time ranges without any additional
    ## formatting for subtrees that have a diary timestamps. For example,
    ## you have a complex date predicate, but event occurs for
    ## `18:00-21:00`, so you write it in the random place in the subtree.
    OStBracketTime
    OStTimeDash

    OStComment ## line or inline comment
    OStListDoubleColon ## Double colon between description list tag and body
    OStCommandArguments ## List of command arguments
    OStCommandBracket ## `#+results[HASH...]`
    OStColonLiteral ## Literal block with `:`
    OStColonIdent ## Drawer or source code block wrappers with
    ## colon-wrapped identifiers. `:results:`, `:end:` etc.
    OStColonProperties
    OStColonEnd
    OStColonLogbook
    OStRawLogbook
    OStRawProperty

    OStLink ## Any kind of link
    OStHashTag ## Inline text hashtag

    OStCodeContent  ## Block of code inside `#+begin_src`
    OStTableContent ## Block of text inside `#+table`
    OStQuoteContent ## `#+quote` content

    OStBackendPass ## Backend-specific passthrough

    OStLogBook ## Logbook including content
    OStDrawer ## Drawer including content

    OStIndent ## Increase in indentation
    OStDedent ## Decrease in indentation
    OStSameIndent
    OStNoIndent

    # === structure tokens end
    #
    # === text tokens begin

    OTxBoldOpen, OTxBoldClose, OTxBoldInline
    OTxItalicOpen, OTxItalicClose, OTxItalicInline
    OTxVerbatimOpen, OTxVerbatimClose, OTxVerbatimInline
    OTxMonospaceOpen, OTxMonospaceClose, OTxMonospaceInline
    OTxBacktickOpen, OTxBacktickClose, OTxBacktickInline
    OTxUnderlineOpen, OTxUnderlineClose, OTxUnderlineInline
    OTxStrikeOpen, OTxStrikeClose, OTxStrikeInline
    OTxQuoteOpen, OTxQuoteClose

    OTxPlaceholderOpen, OTxPlaceholderClose
    OTxTargetOpen, OTxTargetClose
    OTxRadiOTbrgetOpen, OTxRadiOTbrgetClose

    OTxLinkOpen, OTxLinkClose
    OTxLinkTargetOpen, OTxLinkTargetClose
    OTxLinkInternal ## No protocol is used in the link, it is targeting
                    ## some internal named entry.
    OTxLinkProtocol ## Protocol used by the link - `file:`, `https:` etc.
    OTxLinkFull ## Full token for the link, used in cases where it does not
                ## make sense to fracture the token - regular https URLs
                ## etc.
    OTxLinkHost ## Host part of the URI used in link
    OTxLinkPath ## Path part of the link
    OTxLinkTarget ## Target of the link protocol that does not follow
                  ## regular URI encoding scheme - for example `id:`,
                  ## `elisp`, or `shell` links.
    OTxLinkExtraSeparator ## Separator of the extra content in the link, `::`
    OTxLinkExtra ## Additional parametrization for the link search
    OTxLinkDescriptionOpen, OTxLinkDescriptionClose

    OTxParagraphStart ## Fake token inserted by the lexer to delimit start
                      ## of the paragraph
    OTxParagraphEnd

    OTxFootnoteStart
    OTxFootnoteEnd

    OTxWord
    OTxNewline
    OTxMaybeWord
    OTxSpace
    OTxBigIdent
    OTxRawText
    OTxInlineSrc ## Inline source code block: `src_nim[]{}`
    OTxInlineCall

    OTxDollarOpen ## Opening dollar inline latex math
    OTxDollarClose ## Closing dollar for inline latex math
    OTxDoubleDollarOpen
    OTxDoubleDollarClose
    OTxLatexParOpen ## Opening `\(` for inline latex math
    OTxLatexParClose ## Closing `\)` for inline latex math
    OTxLatexBraceOpen ## Opening `\[` for inline display latex equation
    OTxLatexBraceClose ## Closing `\]` for inline display latex equation
    OTxLatexInlineRaw

    OTxDoubleAt ## Inline backend passthrough `@@`
    OTxAtBracket ## Inline annOTbtion
    OTxAtMetaTag
    OTxAtMention
    OTxTagParams

    OTxLink

    OTxSlashEntry

    OTxHashTag

    OTxMacroOpen, OTxMacroBody, OTxMacroClose
    OTxMetaOpen, OTxMetaName, OTxMetaBody, OTxMetaClose

    OTxSrcOpen, OTxSrcName, OTxSrcArgs, OTxSrcBody, OTxSrcClose

    OTxCallOpen, OTxCallName, OTxCallInsideHeader,
    OTxCallArgs, OTxEndHeader, OTxCallClose

    # === text tokens end
    #
    # === table tokens begin

    OTbCmdArguments

    OTbTableBegin
    OTbTableEnd
    OTbCellBody ## Unformatted table cell body
    OTbRowSpec ## `#+row` command together with parameters
    OTbCellSpec ## `#+cell` command with parameters

    OTbContent

    OTbPipeOpen
    OTbPipeSeparator ## Vertical pipe (`|`) cell separator
    OTbPipeClose
    OTbPipeCellOpen

    OTbDashSeparator ## Horizontal dash (`---`, `:---`, `---:` or `:---:`)
                      ## row separator
    OTbCornerPlus ## Corner plus (`+`)

    # === table tokens end
    #
    # === command tokens begin

    OCmCommand
    OCmCommandArgs
    OCmCommandBegin
    OCmCommandPrefix
    OCmCommandEnd
    OCmBody
    OCmLangName
    OCmNewline
    OCmNowebOpen ## `<<` - open for noweb placeholder
    OCmNowebClose ## `>>` - close for noweb placeholder
    OCmNowebName ## Name of the noweb placeholder
    OCmNowebLpar ## Lpar of the noweb placeholder arguments
    OCmNowebRpar ## RPar of the noweb placeholder arguments
    OCmNowebComma ## Noweb argument separator
    OCmNowebArg ## Noweb argument
    OCmTextBlock ## Code before noweb placeholder. Requires separate token
                  ## to handle `##<<commented>>` - prefix comment should be
                  ## duplicated for each line of the placeholder expansion.

    OCmCalloutOpen
    OCmCalloutName
    OCmCalloutClose

  OrgToken* = HsTok[OrgTokenKind]
  OrgLexer* = HsLexer[OrgToken]






const
  markupConfig = {
    '*': (OTxBoldOpen,      OTxBoldClose,      OTxBoldInline),
    '/': (OTxItalicOpen,    OTxItalicClose,    OTxItalicInline),
    '=': (OTxVerbatimOpen,  OTxVerbatimClose,  OTxVerbatimInline),
    '`': (OTxBacktickOpen,  OTxBacktickClose,  OTxBacktickInline),
    '~': (OTxMonospaceOpen, OTxMonospaceClose, OTxMonospaceInline),
    '_': (OTxUnderlineOpen, OTxUnderlineClose, OTxUnderlineInline),
    '+': (OTxStrikeOpen,    OTxStrikeClose,    OTxStrikeInline),
    '"': (OTxQuoteOpen,     OTxQuoteClose,     otNone),
  } ## Table of the markup config information, to reduce usage of the
    ## chracter literals directly in the code.

  markupTable = toMapArray markupConfig
  markupKeys = toKeySet markupConfig


proc initLexCode*(): HsLexCallback[OrgToken] =
  var state = newLexerState(oblsNone)
  return proc(str: var PosStr): seq[OrgToken] =
    if not ?str:
      result.add str.initEof(otEof)

    else:
      case state.topFlag():
        of oblsNone:
          result.add str.initTok(
            str.asSlice str.skip({'#'}, {'+'}), OCmCommandPrefix)

          result.add str.initTok(
            str.asSlice str.skipWhile(IdentChars + {'-', '_'}),
            OCmCommandBegin)

          state.toFlag oblsInHeader

        of oblsInHeader:
          result.addInitTok(str, OCmLangName):
            str.skipWhile(IdentChars)

          if ?str and str[' ']:
            str.space()
            result.addInitTok(str, OCmCommandArgs):
              while ?str: str.next()


          else:
            result.add str.initFakeTok(OCmCommandArgs)

          state.toFlag oblsInBody

        of oblsInBody:
          while ?str:
            case str[]:
              of '<':
                if str[+1, '<']:
                  result.add str.initAdvanceTok(2, OCmNowebOpen)

                else:
                  # TODO merge with previous token if it has the same kind
                  result.add str.initAdvanceTok(1, OCmTextBlock)

              of '\n':
                result.add str.initAdvanceTok(1, OCmNewline)

              of '(':
                if str["(ref:"]:
                  result.add str.initAdvanceTok(5, OCmCalloutOpen)
                  result.add str.scanTok(OCmCalloutName, @')')
                  result.add str.initAdvanceTok(1, OCmCalloutClose)

                else:
                  result.addOrJoin(str.initAdvanceTok(1, OCmTextBlock))

              else:
                str.pushSlice()
                while ?str and not str[{'<', '\n', '('}]:
                  # NOTE can detect string literals and other constructs in
                  # the code and skip them. This can be configured? (tangle
                  # string literals or not)
                  str.next()

                result.addOrJoin(str.initTok(str.popSlice(), OCmTextBlock))

          state.toFlag oblsEnded

        of oblsEnded:
          str.skip({'#'}, {'+'})
          let id = str.asSlice str.skipWhile(IdentChars + {'-', '_'})
          result.add initTok(id, OCmCommandEnd)



proc initLexTable*(): HsLexCallback[OrgToken] =
  var state = newLexerState(oblsNone)
  proc lexTable(str: var PosStr): seq[OrgToken] =
    if not ?str:
      result.add str.initEof(otEof)

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
              of ockBeginTable: result.add id.initTok(id.strVal(), OTbTableBegin)
              of ockRow:        result.add id.initTok(id.strVal(), OTbRowSpec)
              of ockCell:       result.add id.initTok(id.strVal(), OTbCellSpec)
              of ockEndTable:   result.add id.initTok(id.strVal(), OTbTableEnd)
              else: isTableCmd = false

            if isTableCmd:
              state.toFlag(oblsInHeader)
              str.space()
              if ?str:
                if result.last().kind != OTbTableEnd:
                  result.addInitTok(str, OTbCmdArguments):
                    str.skipUntil('\n', including = true)

                if ?str:
                  str.skip('\n')
                  state.toFlag(oblsInBody)

          else:
            isTableCmd = false

          if not isTableCmd:
            str.setPos(pos)
            result.addInitTok(str, OTbContent):
              str.skipPastEol()

        of '|':
          let pos = str.getPos()
          str.skipBeforeEol()
          if str['|']:
            str.setPos(pos)

            var first = true
            dowhile ?str and str['|']:
              result.add str.initTok(
                tern(first, OTbPipeOpen, OTbPipeSeparator),
                str.asSlice str.skip('|'))

              first = false
              str.space()
              if ?str and not str['\n']:
                result.addInitTok(str, OTbContent):
                  str.skipBefore({'|', '\n'})
                  if str[' ']:
                    while str[' ']: str.back()
                    if not str[' ']: str.next()

                  else:
                    str.next()
                str.space()

            discard result.pop()
            result.add str.initTok(OTbPipeClose)

          else:
            str.setPos(pos)
            result.add str.initTok(OTbPipeCellOpen, str.asSlice str.skip('|'))
            str.space()
            result.add str.initTok(OTbContent, str.asSlice str.skipToEol())

          if ?str:
            str.skip('\n')

        of '\n':
          str.next()
          result.add lexTable(str)

        else:
          if state of oblsInHeader:
            result.addInitTok(str, OTbCmdArguments):
              str.skipPastEol()

          else:
            result.addInitTok(str, OTbContent):
              while ?str and not str[{'|', '#'}]:
                str.skipPastEol()

              if ?str:
                str.back()

            if ?str:
              str.next()


  return lexTable


proc lexText*(str: var PosStr): seq[OrgToken]

proc lexTime*(str: var PosStr): seq[OrgToken] =
  if str['<']:
    if str["<%%"]:
      result.addInitTok(str, OStDiaryTime):
        str.skip("<%%")
        str.skipBalancedSlice({'('}, {')'})
        str.skip(">")

    else:
      result.add initTok(
        OStAngleTime, str.scanSlice('<', @{'>', '\n'}, '>'))
      if str["--"]:
        result.add initTok(OStTimeDash, str.scanSlice("--"))
        result.add initTok(
          OStAngleTime, str.scanSlice('<', @{'>', '\n'}, '>'))

  elif str['[']:
    result.add initTok(
      OStBracketTime, str.scanSlice('[', @{']', '\n'}, ']'))
    if str["--"]:
      result.add initTok(OStTimeDash, str.scanSlice("--"))
      result.add initTok(
        OStBracketTime, str.scanSlice('[', @{']', '\n'}, ']'))

  else:
    raise newUnexpectedCharError(str)

proc lexLinkTarget*(str: var PosStr): seq[OrgToken] =
  # Link protocols whose links are better kept intact
  if str["https"] or str["http"]:
    result.add str.initTok(str, OTxLinkFull)

  # File protocl
  elif str["file"] or
       str["attachment"] or
       str["docview"] or
       str['/'] or
       str["./"]:

    if str['.'] or str['/']:
      result.add initFakeTok(str, OTxLinkProtocol, "file")

    else:
      result.addInitTok(str, OTxLinkProtocol):
        str.skipTo(':')
      str.skip(':')

    result.addInitTok(str, OTxLinkTarget):
      while ?str and not str["::"]:
        str.next()

    if str["::"]:
      result.addInitTok(str, OTxLinkExtraSeparator):
        str.next(2)

      result.addInitTok(str, OTxLinkExtra):
        str.skipPastEOF()

  # Simple, non-URI protocols that don't have trailing extra separator
  # parametrization and all other cases (including user-provided link
  # templates)
  else:
    if str.hasAhead({':'}):
      result.addInitTok(str, OTxLinkProtocol):
        str.skipTo(':')

      str.skip(':')
      result.addInitTok(str, OTxLinkTarget):
        str.skipPastEOF()

    else:
      result.addInitTok(str, OTxLinkInternal):
        str.skipPastEOF()


proc lexBracket*(str: var PosStr): seq[OrgToken] =
  if str["[["]:
    result.add str.scanTok(OTxLinkOpen, '[')

    block link_token:
      result.add str.scanTok(OTxLinkTargetOpen, '[')
      result.add str.asSlice(str.skipUntil({']'})).asVar().lexLinkTarget()
      result.add str.scanTok(OTxLinkTargetClose, ']')


    block description_token:
      if str['[']:
        result.add str.scanTok(OTxLinkDescriptionOpen, '[')
        var desc = str.asSlice str.skipUntil({']'})
        while ?desc:
          result.add lexText(desc)

        result.add str.scanTok(OTxLinkDescriptionClose, ']')

    result.add str.scanTok(OTxLinkClose, ']')

  elif str["[fn:"]:
    result.add str.scanTok(OTxFootnoteStart, '[')
    str.skip("fn")
    if str["::"]:
      result.add str.scanTok(OStDoubleColon, "::")
      result.addInitTok(str, OStText): str.skipTo(']')

    else:
      result.addInitTok(str, OStColon): str.skip(':')
      result.addInitTok(str, OStIdent): str.skipTo(']')

    result.add str.scanTok(OTxFootnoteEnd, ']')

  else:
    return lexTime(str)


const TextChars = MaybeLetters + Digits + { '.', ',', '-'}
proc lexTextChars*(str: var PosStr): seq[OrgToken] =
  var isStructure: bool = false
  if str["src"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("src"), OTxSrcOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf

      result.add str.initTok(str.asSlice str.skipWhile(IdentChars), OTxSrcName)
      if str['[']:
        result.add str.initTok(
          str.asSlice(str.skipBalancedSlice({'['}, {']'}), 1, -2),
          OTxSrcArgs
        )

      result.add str.initTok(
        str.asSlice(str.skipBalancedSlice({'{'}, {'}'}), 1, -2),
        OTxSrcBody
      )

      result.add str.initTok(OTxSrcClose)
      isStructure = true

    else:
      str.setPos(pos)


  elif str["call"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("call"), OTxCallOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf

      result.add str.initTok(
        str.asSlice str.skipWhile(IdentChars), OTxCallName)

      if str['[']:
        result.add str.initTok(
          str.asSlice(str.skipBalancedSlice({'['}, {']'}), 1, -2),
          OTxCallInsideHeader
        )

      result.add str.initTok(
        str.asSlice(str.skipBalancedSlice({'('}, {')'}), 1, -2),
        OTxCallArgs
      )

      result.add str.initTok(OTxCallClose)
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

    result.add str.initSliceTok(if allUp: OTxBigIdent else: OTxWord)


proc lexText*(str: var PosStr): seq[OrgToken] =
  if not ?str:
    result.add str.initEof(otEof)

  else:
    case str[]:
      of TextChars:
        result.add str.lexTextChars()

      of '\n':
        result.add str.initAdvanceTok(1, OTxNewline)

      of ' ':
        result.add str.initTok(str.popWhileSlice({' '}), OTxSpace)

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

        result.add str.initTok(str.popSlice(), OTxHashTag)

      of '@':
        var buf: seq[OrgToken]
        let slice = str.asSlice(str.skip('@'))
        if str[IdentChars]:
          buf.add str.initTok(slice, OTxMetaOpen)
          buf.addInitTok(str, OTxMetaName):
            str.skipWhile(IdentChars)

          if str['{']:
            while str['{']:
              buf.add str.initTok(
                str.asSlice(str.skipBalancedSlice({'{'}, {'}'}), 1, -2),
                OTxMetaBody
              )

            result = buf
            result.add str.initTok(OTxMetaClose)

          else:
            raise newImplementError("@name")

        else:
          result.add str.initTok(slice, OTxMaybeWord)


      of '$', '\\':
        if str['$']:
          if str[+1, '$']:
            result.add str.scanTok(OTxDollarOpen, '$', '$')
            str.startSlice()
            var hasEnd = false
            while ?str and not hasEnd:
              while ?str and not str['$']:
                str.next()

              if str['$', '$']:
                result.add str.initTok(str.popSlice(), OTxLatexInlineRaw)
                hasEnd = true

              else:
                raise newImplementError()

            result.add str.scanTok(OTxDollarClose, '$', '$')

          else:
            result.add str.scanTok(OTxDollarOpen, '$')
            result.add str.initTok(
              str.asSlice str.skipUntil({'$'}),
              OTxLatexInlineRaw)

            result.add str.scanTok(OTxDollarClose, '$')


        elif str[+1, {'[', '('}]:
          raise newImplementError()

      of '~', '`', '=':
        let start = str[]
        if str[+1, start]:
          result.add str.initTok(
            str.popPointSlice(advance = 2), markupTable[start][2])

        if str[-1, ' '] or str.atStart():
          result.add str.initTok(
            str.popPointSlice(),
            markupTable[start][0])

        result.addInitTok(str, OTxRawText):
          str.skipTo(start)

        if str[+1, ' '] or str.beforeEnd():
          result.add str.initTok(
            str.popPointSlice(),
            markupTable[start][1])

        else:
          result.add str.initTok(
            str.popPointSlice(advance = 2),
            markupTable[start][2])

      of '<':
        if str['<', '<', '<']:
          result.add str.initAdvanceTok(3, OTxRadiOTbrgetOpen)

          # TODO More sophisicated lexer that checks for `>>` and `>``
          result.addInitTok(str, OTxRawText):
            str.skipUntil({ '>' })

          result.addInitTok(str, OTxRadiOTbrgetClose):
            str.skip('>')
            str.skip('>')
            str.skip('>')


        elif str['<', '<']:
          result.add str.initAdvanceTok(2, OTxTargetOpen)
          result.addInitTok(str, OTxRawtext, str.skipUntil({ '>' }))
          result.addInitTok(str, OTxTargetClose):
            str.skip('>')
            str.skip('>')

        elif str["<%%"]:
          result.add str.lexTime()

        else:
          result.add str.initAdvanceTok(1, OTxPlaceholderOpen)
          result.add str.initTok(str.asSlice str.skipUntil({ '>' }), OTxRawText)
          result.add str.initTok(str.asSlice str.skip('>'), OTxPlaceholderClose)


      of markupKeys - { '<', '~', '`', '=' }:
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
          result.add str.initAdvanceTok(3, OTxMacroOpen)
          result.add str.initTok(
            asSlice(str, inWhile(?str and not str["}}}"], str.next())),
            OTxMacroBody
          )

          if ?str:
            result.add str.initAdvanceTok(3, OTxMacroClose)


        else:
          result.add str.initAdvanceTok(1, OTxMaybeWord)

      else:
        raise newUnexpectedCharError(str)





proc lexSubtree(str: var PosStr): seq[OrgToken] =
  result.add str.initTok(str.asSlice str.skipWhile({'*'}), OStSubtreeStars)
  str.skipWhile({' '})
  block todo:
    var tmp = str
    tmp.startSlice()
    tmp.skipWhile(HighAsciiLetters)
    if tmp[' ']:
      result.add tmp.initTok(tmp.popSlice(), OStSubtreeTodoState)
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

    result.add str.initTok(str.popSlice(), OStSubtreeImportance)

    str.skipWhile({' '})

  var
    body = str.asSlice(str.skipToEol())
    headerTokens: seq[OrgToken]

  body.gotoEof()
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
      str.sliceBetween(start, finish), OStSubtreeTag)

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
      str.sliceBetween(start, finish), OStSubtreeCompletion)

    while body[' ']:
      body.back()

  block:
    let finish = body.getPos()
    body.goToSof()
    let start = body.getPos()

    headerTokens.add body.initTok(
      str.sliceBetween(start, finish), OStText)


  result.add headerTokens.reversed()
  discard str.trySkip('\n')

  var times = str
  times.space()

  if times[HighAsciiLetters]:
    let tag = times.asSlice times.skipWhile(HighAsciiLetters)
    if tag.strValNorm() in ["deadline", "closed"]:
      result.add initTok(tag, OStSubtreeTime)
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
          result.add initTok(id, OStColonProperties)
          var hasEnd = false

          while ?drawer and not hasEnd:
            drawer.space()
            let id = drawer.scanSlice(':', *\DId, ':')
            if id.strValNorm() == ":end:":
              hasEnd = true
              result.add initTok(id, OStColonEnd)

            else:
              result.add initTok(id, OStColonIdent)
              if drawer[IdentStartChars]:
                result.addInitTok(drawer, OStIdent):
                  while ?drawer and drawer[DashIdentChars]:
                    drawer.next()

                drawer.skip(':')

              drawer.space()

              result.addInitTok(drawer, OStRawProperty):
                drawer.skipToEol()

              drawer.skip('\n')

        of ":logbook:":
          result.add initTok(id, OStColonLogbook)
          drawer.startSlice()

          var hasEnd = false
          while ?drawer and not hasEnd:
            while ?drawer and not drawer[':']:
              drawer.next()

            if drawer[':']:
              let id = drawer.asSlice:
                drawer.skip({':'})
                drawer.skipWhile(IdentChars)
                discard drawer.trySkip(':')

              if id.strValNorm() == ":end:":
                result.add initTok(drawer.popSlice(), OStRawLogbook)
                result.add initTok(id, OStColonEnd)
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


proc whichArguments(kind: OrgCommandKind): OrgTokenKind =
  ## Token kind used as an argument for the inline or regular block command
  case kind:
    of ockCaption: OStText
    else: OStCommandArguments


proc lexCommandBlock(str: var PosStr): seq[OrgToken] =
  # Store position of the command start - content be dedented or indented
  # arbitrarily, so `#+begin_src` starting at column 2 might have content
  # that starts on the column 0.
  let column = str.column
  result.add str.initAdvanceTok(2, OStCommandPrefix)
  let id = str.asSlice str.skipWhile(OCommandChars)

  if id.strValNorm().startsWith("begin"):
    result.add initTok(id, OStCommandBegin)
    let sectionName = id.strVal().normalize().dropPrefix("begin")

    str.space()
    result.add str.initTok(
      str.asSlice(str.skipPastEol(), -2),
      id.strVal().classifyCommand().whichArguments())

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

        result.add str.initTok(slice, OStCodeContent)
        result.add initTok(prefix, OStCommandPrefix)
        result.add initTok(id, OStCommandEnd)

  else:
    result.add initTok(id, OStLineCommand)
    result.add str.initAdvanceTok(1, OStColon, {':'})
    str.space()
    result.addInitTok(str, id.strVal().classifyCommand().whichArguments()):
      str.skipToEol()

    if ?str:
      str.skip('\n')

proc lexList(str: var PosStr): seq[OrgToken] =
  # Create temporary state to lex content of the list
  var state = newLexerState()

  proc aux(str: var PosStr): seq[OrgToken] =
    template popIndents(): untyped =
      let skipped = state.skipIndent(str)
      for indent in skipped:
        case indent:
          of likIncIndent:  result.add str.initTok(OStIndent)
          of likDecIndent:  result.add str.initTok(OStDedent)
          of likSameIndent: result.add str.initTok(OStSameIndent)
          of likNoIndent:   result.add str.initTok(OStNoIndent)
          of likEmptyLine:  raise newImplementError()

    
    case str[]:
      of '-':
        let indent = str.column
        result.add str.initTok(
          str.asSlice str.skip({'-'}), OStListDash)

        str.space()

        if str['[']:
          result.add str.scanTok(OStCheckbox, '[', {'X', 'x', ' ', '-'}, ']')
          str.space()

        # create slice for the whole content of the list item
        str.startSlice()
        var atEnd = false
        var nextList = true
        # extend slice until new list start is not found - either via new
        # nested item or by indentation decrease.
        while ?str and not atEnd:
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


        result.add str.initTok(str.popSlice(-1), OStText)
        result.add str.initTok(OStListItemEnd)
        if nextList:
          # current list contains nested items - skip necessary indentation
          # levels and recursively call lexer from this point onwards.
          popIndents()
          result.add aux(str)

      of '\n', '\x00':
        for level in 0 ..< state.getIndentLevels():
          result.add str.initTok(OStDedent)

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
    result.add str.initTok(OStDedent)


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
          of MaybeLetters, {'-'}:
            discard

          of '\n':
            str.next()
            ended = true

          else:
            raise newUnexpectedCharError(str, parsing = "paragraph")

  result.add str.initTok(str.popSlice(tern(
    ended,
    tern(?str, -3, -2) #[ last trailing newline and pargraph separator newline ]#,
    -1)), OStText)

proc lexStructure*(): HsLexCallback[OrgToken] =
  ## Create lexer for toplevel structure of the org document
  proc aux(str: var PosStr): seq[OrgToken] =
    # Temporary token list for further processing
    case str[]:
      of '#':
        if str[+1, '+']:
          result = lexCommandBlock(str)

        else:
          raise newImplementError()

      of '\x00':
        result.add str.initEof(otEof)

      of '*':
        if str.column == 0:
          result.add lexSubtree(str)

        else:
          raise newImplementError()

      of '-':
        result = lexList(str)

      of '\n', ' ':
        str.skipWhile({' ', '\n'})
        result = str.aux()

      of MaybeLetters, {'~', '['}:
        result = lexParagraph(str)

      else:
        raise newUnexpectedCharError(str)

  return aux

proc lexGlobal*(): HsLexCallback[OrgToken] =
  ## Lex global structure of the org document
  var structure = lexStructure()
  proc aux(str: var PosStr): seq[OrgToken] =
    for token in structure(str):
      case token.kind:
        of OStText:
          # generic 'text' token was found somewhere in the main structure
          # of the document - list content, `#+caption` element etc. In
          # that context it only had defined boundaries but further lexing
          # was deferred until now, to avoid repeating the same construct
          # dozen times.
          result.add token.initFakeTok(OTxParagraphStart)

          var content = initPosStr(token)
          while ?content:
            result.add lexText(content)

          result.add token.initFakeTok(OTxParagraphEnd)

        else:
          result.add token

  return aux
