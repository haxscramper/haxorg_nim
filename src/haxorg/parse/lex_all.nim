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
    ostCommandPrefix
    ostIdent
    ostLineCommand
    ostCommandBegin ## `#+begin` part of the multiline command.
    ## `begin_<block-type>` is split into two tokens - `begin_` prefix and
    ## `ockBegin<block-type>` section.
    ostCommandEnd



    ostBigIdent
    ostColon
    ostText
    ostListDash
    ostListPlus
    ostListStar
    ostListItemEnd ## End of the list item
    ostCheckbox ## List or subtree checkbox

    ostSubtreeTodoState
    ostSubtreeImportance ## Subtree importance marker
    ostSubtreeCompletion ## Subtree completion marker
    ostSubtreeStars ## Subtree prefix
    ostSubtreeTag ## Subtree tag
    ostSubtreeTime
    ostAngleTime
    ostBracketTime

    ostComment ## line or inline comment
    ostListDoubleColon ## Double colon between description list tag and body
    ostCommandArguments ## List of command arguments
    ostCommandBracket ## `#+results[HASH...]`
    ostColonLiteral ## Literal block with `:`
    ostColonIdent ## Drawer or source code block wrappers with
    ## colon-wrapped identifiers. `:results:`, `:end:` etc.
    ostColonProperties
    ostColonEnd
    ostColonLogbook
    ostRawLogbook
    ostRawProperty

    ostLink ## Any kind of link
    ostHashTag ## Inline text hashtag

    ostCodeContent  ## Block of code inside `#+begin_src`
    ostTableContent ## Block of text inside `#+table`
    ostQuoteContent ## `#+quote` content

    ostBackendPass ## Backend-specific passthrough

    ostLogBook ## Logbook including content
    ostDrawer ## Drawer including content

    ostIndent ## Increase in indentation
    ostDedent ## Decrease in indentation
    ostSameIndent
    ostNoIndent

    # === structure tokens end
    #
    # === text tokens begin

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

    ottParagraphStart ## Fake token inserted by the lexer to delimit start
                      ## of the paragraph
    ottParagraphEnd

    ottWord
    ottNewline
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
    ottMetaOpen, ottMetaName, ottMetaBody, ottMetaClose

    ottSrcOpen, ottSrcName, ottSrcArgs, ottSrcBody, ottSrcClose

    ottCallOpen, ottCallName, ottCallInsideHeader,
    ottCallArgs, ottEndHeader, ottCallClose

    # === text tokens end
    #
    # === table tokens begin

    otaCmdArguments

    otaTableBegin
    otaTableEnd
    otaCellBody ## Unformatted table cell body
    otaRowSpec ## `#+row` command together with parameters
    otaCellSpec ## `#+cell` command with parameters

    otaContent

    otaPipeOpen
    otaPipeSeparator ## Vertical pipe (`|`) cell separator
    otaPipeClose
    otaPipeCellOpen

    otaDashSeparator ## Horizontal dash (`---`, `:---`, `---:` or `:---:`)
                      ## row separator
    otaCornerPlus ## Corner plus (`+`)

    # === table tokens end
    #
    # === command tokens begin

    octkCommand
    octkCommandArgs
    octkCommandBegin
    octkCommandPrefix
    octkCommandEnd
    octkBody
    octkLangName
    octkNewline
    octkNowebOpen ## `<<` - open for noweb placeholder
    octkNowebClose ## `>>` - close for noweb placeholder
    octkNowebName ## Name of the noweb placeholder
    octkNowebLpar ## Lpar of the noweb placeholder arguments
    octkNowebRpar ## RPar of the noweb placeholder arguments
    octkNowebComma ## Noweb argument separator
    octkNowebArg ## Noweb argument
    octkTextBlock ## Code before noweb placeholder. Requires separate token
                  ## to handle `##<<commented>>` - prefix comment should be
                  ## duplicated for each line of the placeholder expansion.

    octkCalloutOpen
    octkCalloutName
    octkCalloutClose

  OrgToken* = HsTok[OrgTokenKind]
  OrgLexer* = HsLexer[OrgToken]






const
  markupConfig = {
    '*': (ottBoldOpen,      ottBoldClose,      ottBoldInline),
    '/': (ottItalicOpen,    ottItalicClose,    ottItalicInline),
    '=': (ottVerbatimOpen,  ottVerbatimClose,  ottVerbatimInline),
    '`': (ottBacktickOpen,  ottBacktickClose,  ottBacktickInline),
    '~': (ottMonospaceOpen, ottMonospaceClose, ottMonospaceInline),
    '_': (ottUnderlineOpen, ottUnderlineClose, ottUnderlineInline),
    '+': (ottStrikeOpen,    ottStrikeClose,    ottStrikeInline),
    '"': (ottQuoteOpen,     ottQuoteClose,     otNone),
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
            str.asSlice str.skip({'#'}, {'+'}), octkCommandPrefix)

          result.add str.initTok(
            str.asSlice str.skipWhile(IdentChars + {'-', '_'}),
            octkCommandBegin)

          state.toFlag oblsInHeader

        of oblsInHeader:
          result.addInitTok(str, octkLangName):
            str.skipWhile(IdentChars)

          if ?str and str[' ']:
            str.space()
            result.addInitTok(str, octkCommandArgs):
              while ?str: str.next()


          else:
            result.add str.initFakeTok(octkCommandArgs)

          state.toFlag oblsInBody

        of oblsInBody:
          while ?str:
            case str[]:
              of '<':
                if str[+1, '<']:
                  result.add str.initAdvanceTok(2, octkNowebOpen)

                else:
                  # TODO merge with previous token if it has the same kind
                  result.add str.initAdvanceTok(1, octkTextBlock)

              of '\n':
                result.add str.initAdvanceTok(1, octkNewline)

              of '(':
                if str["(ref:"]:
                  result.add str.initAdvanceTok(5, octkCalloutOpen)
                  result.add str.scanTok(octkCalloutName, @')')
                  result.add str.initAdvanceTok(1, octkCalloutClose)

                else:
                  result.addOrJoin(str.initAdvanceTok(1, octkTextBlock))

              else:
                str.pushSlice()
                while ?str and not str[{'<', '\n', '('}]:
                  # NOTE can detect string literals and other constructs in
                  # the code and skip them. This can be configured? (tangle
                  # string literals or not)
                  str.next()

                result.addOrJoin(str.initTok(str.popSlice(), octkTextBlock))

          state.toFlag oblsEnded

        of oblsEnded:
          str.skip({'#'}, {'+'})
          let id = str.asSlice str.skipWhile(IdentChars + {'-', '_'})
          result.add initTok(id, octkCommandEnd)



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
              of ockBeginTable: result.add id.initTok(id.strVal(), otaTableBegin)
              of ockRow:        result.add id.initTok(id.strVal(), otaRowSpec)
              of ockCell:       result.add id.initTok(id.strVal(), otaCellSpec)
              of ockEndTable:   result.add id.initTok(id.strVal(), otaTableEnd)
              else: isTableCmd = false

            if isTableCmd:
              state.toFlag(oblsInHeader)
              str.space()
              if ?str:
                if result.last().kind != otaTableEnd:
                  result.addInitTok(str, otaCmdArguments):
                    str.skipUntil('\n', including = true)

                if ?str:
                  str.skip('\n')
                  state.toFlag(oblsInBody)

          else:
            isTableCmd = false

          if not isTableCmd:
            str.setPos(pos)
            result.addInitTok(str, otaContent):
              str.skipPastEol()

        of '|':
          let pos = str.getPos()
          str.skipBeforeEol()
          if str['|']:
            str.setPos(pos)

            var first = true
            dowhile ?str and str['|']:
              result.add str.initTok(
                tern(first, otaPipeOpen, otaPipeSeparator),
                str.asSlice str.skip('|'))

              first = false
              str.space()
              if ?str and not str['\n']:
                result.addInitTok(str, otaContent):
                  str.skipBefore({'|', '\n'})
                  if str[' ']:
                    while str[' ']: str.back()
                    if not str[' ']: str.next()

                  else:
                    str.next()
                str.space()

            discard result.pop()
            result.add str.initTok(otaPipeClose)

          else:
            str.setPos(pos)
            result.add str.initTok(otaPipeCellOpen, str.asSlice str.skip('|'))
            str.space()
            result.add str.initTok(otaContent, str.asSlice str.skipToEol())

          if ?str:
            str.skip('\n')

        of '\n':
          str.next()
          result.add lexTable(str)

        else:
          if state of oblsInHeader:
            result.addInitTok(str, otaCmdArguments):
              str.skipPastEol()

          else:
            result.addInitTok(str, otaContent):
              while ?str and not str[{'|', '#'}]:
                str.skipPastEol()

              if ?str:
                str.back()

            if ?str:
              str.next()


  return lexTable


proc lexText*(str: var PosStr): seq[OrgToken]

proc lexBracket*(str: var PosStr): seq[OrgToken] =
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

const TextChars = MaybeLetters + Digits + { '.', ',', '-'}
proc lexTextChars*(str: var PosStr): seq[OrgToken] =
  var isStructure: bool = false
  if str["src"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("src"), ottSrcOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf

      result.add str.initTok(str.asSlice str.skipWhile(IdentChars), ottSrcName)
      result.add str.initTok(
        str.asSlice(str.skipBalancedSlice({'{'}, {'}'}), 1, -2),
        ottSrcBody
      )

      result.add str.initTok(ottSrcClose)
      isStructure = true

    else:
      str.setPos(pos)


  elif str["call"]:
    let pos = str.getPos()
    var buf: seq[OrgToken]
    buf.add str.initTok(str.asSlice str.skip("call"), ottCallOpen)
    if str[{'_', '-'}]:
      str.next()

    if str[IdentStartChars]:
      result.add buf

      result.add str.initTok(
        str.asSlice str.skipWhile(IdentChars), ottCallName)

      if str['[']:
        result.add str.initTok(
          str.asSlice(str.skipBalancedSlice({'['}, {']'}), 1, -2),
          ottCallInsideHeader
        )

      result.add str.initTok(
        str.asSlice(str.skipBalancedSlice({'('}, {')'}), 1, -2),
        ottCallArgs
      )

      result.add str.initTok(ottCallClose)
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

    result.add str.initSliceTok(if allUp: ottBigIdent else: ottWord)


proc lexText*(str: var PosStr): seq[OrgToken] =
  if not ?str:
    result.add str.initEof(otEof)

  else:
    case str[]:
      of TextChars:
        result.add str.lexTextChars()

      of '\n':
        result.add str.initAdvanceTok(1, ottNewline)
      # of '\n':
      #   str.next()
      #   result.add lexText(str)



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

      of '@':
        var buf: seq[OrgToken]
        let slice = str.asSlice(str.skip('@'))
        if str[IdentChars]:
          buf.add str.initTok(slice, ottMetaOpen)
          buf.addInitTok(str, ottMetaName):
            str.skipWhile(IdentChars)

          if str['{']:
            while str['{']:
              buf.add str.initTok(
                str.asSlice(str.skipBalancedSlice({'{'}, {'}'}), 1, -2),
                ottMetaBody
              )

            result = buf
            result.add str.initTok(ottMetaClose)

          else:
            raise newImplementError("@name")

        else:
          result.add str.initTok(slice, ottMaybeWord)


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

      of '~', '`', '=':
        let start = str[]
        if str[+1, start]:
          result.add str.initTok(
            str.popPointSlice(advance = 2), markupTable[start][2])

        if str[-1, ' '] or str.atStart():
          result.add str.initTok(
            str.popPointSlice(),
            markupTable[start][0])

        result.addInitTok(str, ottRawText):
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
          result.add str.initAdvanceTok(3, ottRadioTargetOpen)

          # TODO More sophisicated lexer that checks for `>>` and `>``
          result.addInitTok(str, ottRawText):
            str.skipUntil({ '>' })

          result.addInitTok(str, ottRadioTargetClose):
            str.skip('>')
            str.skip('>')
            str.skip('>')


        elif str['<', '<']:
          result.add str.initAdvanceTok(2, ottTargetOpen)
          result.addInitTok(str, ottRawtext, str.skipUntil({ '>' }))
          result.addInitTok(str, ottTargetClose):
            str.skip('>')
            str.skip('>')

        else:
          result.add str.initAdvanceTok(1, ottPlaceholderOpen)
          result.add str.initTok(str.asSlice str.skipUntil({ '>' }), ottRawText)
          result.add str.initTok(str.asSlice str.skip('>'), ottPlaceholderClose)


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





proc lexSubtree(str: var PosStr): seq[OrgToken] =
  result.add str.initTok(str.asSlice str.skipWhile({'*'}), ostSubtreeStars)
  str.skipWhile({' '})
  block todo:
    var tmp = str
    tmp.startSlice()
    tmp.skipWhile(HighAsciiLetters)
    if tmp[' ']:
      result.add tmp.initTok(tmp.popSlice(), ostSubtreeTodoState)
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

    result.add str.initTok(str.popSlice(), ostSubtreeImportance)

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
      str.sliceBetween(start, finish), ostSubtreeTag)

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
      str.sliceBetween(start, finish), ostSubtreeCompletion)

    while body[' ']:
      body.back()

  block:
    let finish = body.getPos()
    body.goToSof()
    let start = body.getPos()

    headerTokens.add body.initTok(
      str.sliceBetween(start, finish), ostText)


  result.add headerTokens.reversed()
  discard str.trySkip('\n')

  var times = str
  times.space()
  if times[HighAsciiLetters]:
    let tag = times.asSlice times.skipWhile(HighAsciiLetters)
    if tag.strValNorm() in ["deadline", "closed"]:
      result.add initTok(tag, ostSubtreeTime)
      times.skip({':'})
      times.space()
      if times['<']:
        result.add initTok(ostAngleTime, times.scanSlice('<', @{'>', '\n'}, '>'))

      elif times['[']:
        result.add initTok(ostBracketTime, times.scanSlice('[', @{']', '\n'}, ']'))

      else:
        raise newUnexpectedCharError(times)

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
          result.add initTok(id, ostColonProperties)
          var hasEnd = false

          while ?drawer and not hasEnd:
            drawer.space()
            let id = drawer.scanSlice(':', *\DId, ':')
            if id.strValNorm() == ":end:":
              hasEnd = true
              result.add initTok(id, ostColonEnd)

            else:
              result.add initTok(id, ostColonIdent)
              if drawer[IdentStartChars]:
                result.addInitTok(drawer, ostIdent):
                  while ?drawer and drawer[DashIdentChars]:
                    drawer.next()

                drawer.skip(':')

              drawer.space()

              result.addInitTok(drawer, ostRawProperty):
                drawer.skipToEol()

              drawer.skip('\n')

        of ":logbook:":
          result.add initTok(id, ostColonLogbook)
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
                result.add initTok(drawer.popSlice(), ostRawLogbook)
                result.add initTok(id, ostColonEnd)
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
    of ockCaption: ostText
    else: ostCommandArguments


proc lexCommandBlock(str: var PosStr): seq[OrgToken] =
  # Store position of the command start - content be dedented or indented
  # arbitrarily, so `#+begin_src` starting at column 2 might have content
  # that starts on the column 0.
  let column = str.column
  result.add str.initAdvanceTok(2, ostCommandPrefix)
  let id = str.asSlice str.skipWhile(OCommandChars)

  if id.strValNorm().startsWith("begin"):
    result.add initTok(id, ostCommandBegin)
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

        result.add str.initTok(slice, ostCodeContent)
        result.add initTok(prefix, ostCommandPrefix)
        result.add initTok(id, ostCommandEnd)

  else:
    result.add initTok(id, ostLineCommand)
    result.add str.initAdvanceTok(1, ostColon, {':'})
    str.space()
    result.addInitTok(str, id.strVal().classifyCommand().whichArguments()):
      str.skipToEol()

    if ?str:
      str.skip('\n')

proc lexList(
    str: var PosStr,
    state: var HsLexerStateSimple): seq[OrgToken] =

  case str[]:
    of '-':
      let indent = str.column
      result.add str.initTok(
        str.asSlice str.skip({'-'}), ostListDash)

      str.space()

      if str['[']:
        result.add str.scanTok(ostCheckbox, '[', {'X', 'x', ' ', '-'}, ']')
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
        while str.trySkipEmptyLine():
          echov str

        # Decide based on the indentation what to do next
        # indentation decreased, end of the list item
        if str.getIndent() < indent:
          atEnd = true
          # echov "found end", str
          # echov indent
          # echov str.getIndent()

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
            nextList = true
            str = store


      result.add str.initTok(str.popSlice(-1), ostText)
      result.add str.initTok(ostListItemEnd)
      if nextList:
        result.add str.initTok(ostIndent)
        result.add str.lexList(state)
        result.add str.initTok(ostDedent)

    of '\n', '\x00':
      for level in 0 ..< state.getIndentLevels():
        echov "removing indent level"
        result.add str.initTok(ostDedent)

      if ?str:
        str.next()

      state.clearIndent()

    of ' ':
      let indent = state.skipIndent(str)
      case indent:
        of likIncIndent:
          result.add str.initTok(ostIndent)
          result.add str.lexList(state)

        of likDecIndent:  result.add str.initTok(ostDedent)
        of likSameIndent: result.add str.initTok(ostSameIndent)
        of likNoIndent:   result.add str.initTok(ostNoIndent)
        of likEmptyLine: raise newImplementError()


    else:
      raise newUnexpectedCharError(
        str,
        parsing = "ordered or unordered list")

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
    -1)), ostText)

proc lexStructure*(): HsLexCallback[OrgToken] =
  ## Create lexer for toplevel structure of the org document
  var state = newLexerState()
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
        result = lexList(str, state)

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
        of ostText:
          # generic 'text' token was found somewhere in the main structure
          # of the document - list content, `#+caption` element etc. In
          # that context it only had defined boundaries but further lexing
          # was deferred until now, to avoid repeating the same construct
          # dozen times.
          result.add token.initFakeTok(ottParagraphStart)

          var content = initPosStr(token)
          while ?content:
            result.add lexText(content)

          result.add token.initFakeTok(ottParagraphEnd)

        else:
          result.add token

  return aux
