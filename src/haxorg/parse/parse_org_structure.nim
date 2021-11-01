import
  std/[algorithm, sequtils]

import
  ../defs/[org_types, impl_org_node, impl_sem_org],
  ./parse_org_command,
  ./parse_org_text,
  ./parse_org_table,
  ./parse_org_code,
  ./parse_org_common

import
  hmisc/algo/[hparse_base, hlex_base, hstring_algo],
  hmisc/other/hpprint,
  hmisc/core/all,
  hmisc/types/colorstring

type
  OrgStructureTokenKind* = enum
    ostNone

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

    ostIndent
    ostDedent
    ostSameIndent
    ostNoIndent
    ostEof

  OrgStructureToken* = HsTok[OrgStructureTokenKind]
  OrgStructureLexer* = HsLexer[OrgStructureToken]


proc newTree*(kind: OrgNodeKind, tok: OrgStructureToken): OrgNode =
  newTree(kind, initPosStr(tok))

using
  lexer: var OrgStructureLexer
  parseConf: ParseConf

proc lexSubtree(str: var PosStr): seq[OrgStructureToken] =
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
    headerTokens: seq[OrgStructureToken]

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



proc lexCommandBlock(str: var PosStr): seq[OrgStructureToken] =
  result.add str.initAdvanceTok(2, ostCommandPrefix)
  let id = str.asSlice str.skipWhile(OCommandChars)

  if id.strValNorm().startsWith("begin"):
    result.add initTok(id, ostCommandBegin)
    let sectionName = id.strVal().normalize().dropPrefix("begin")

    str.space()
    result.add str.initTok(
      str.asSlice(str.skipPastEol(), -2), ostCommandArguments)
    let column = str.column
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
    result.addInitTok(str, ostCommandArguments):
      str.skipToEol()

    if ?str:
      str.skip('\n')

proc lexList(
    str: var PosStr,
    state: var HsLexerStateSimple): seq[OrgStructureToken] =

  case str[]:
    of '-':
      let indent = str.column
      result.add str.initTok(
        str.asSlice str.skip({'-'}), ostListDash)

      str.space()

      if str['[']:
        result.add str.scanTok(ostCheckbox, '[', {'X', 'x', ' ', '-'}, ']')
        str.space()

      str.startSlice()
      var atEnd = false
      var nextList = true
      while ?str and not atEnd:
        str.skipToEol()
        if str.getIndent() < indent:
          atEnd = true

        else:
          var store = str
          store.skipWhile({' '})
          if store["- "]:
            atEnd = true
            nextList = true
            str = store

          else:
            atEnd = true

      result.add str.initTok(str.popSlice(-2), ostText)
      if nextList:
        result.add str.lexList(state)

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




  # str.startSlice()
  # var inParagraph = true

  # echov str[0 .. 10]
  # echov str.column
  # echov column
  # while ?str and inParagraph:
  #   str.skipToEol()
  #   let ind = str.getIndent()
  #   echov ind
  #   echov str[0..10]
  #   if ind <= column:
  #     inParagraph = false

  # result.add str.initTok(str.popSlice(-2), ostText)


proc lexParagraph*(str: var PosStr): seq[OrgStructureToken] =
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

proc lexStructure*(): HsLexCallback[OrgStructureToken] =
  var state = newLexerState()
  proc aux(str: var PosStr): seq[OrgStructureToken] =
    # echov str
    case str[]:
      of '#':
        if str[+1, '+']:
          result = lexCommandBlock(str)

        else:
          raise newImplementError()

      of '\x00':
        result.add str.initEof(ostEof)

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

    # echov result

  return aux

proc parseStmt*(lexer, parseConf): OrgNode
proc parseCommandArgs*(lexer, parseConf): OrgNode =
  lexer.pop({ostCommandArguments}).initPosStr().parseCommandArgs(parseConf)

proc parseIdent*(lexer; subKind: OrgNodeSubKind = oskNone): OrgNode =
  newTree(orgIdent, lexer.pop({ostIdent}))

proc parseBigIdent*(
    lexer, parseConf; subkind: OrgNodeSubKind = oskNone): OrgNode =
  newTree(orgBigIdent, lexer.pop({ostBigIdent}))

# proc parseCmdArguments*(lexer, parseConf): OrgNode =
#   lexer.skip()
#   var flags: seq[OrgNode]
#   while lexer[] == '-':
#     flags.add orgCmdFlag.newTree(orgRawText.newTree(
#       lexer.getSkipUntil(OWhitespace).toSlice(lexer)))

#     lexer.skip()

#   var args: seq[OrgNode]
#   while lexer[] == ':':
#     lexer.advance()
#     args.add orgCmdValue.newTree(lexer.parseIdent())
#     lexer.skip()

#     var value = lexer.getSkipUntil({OEndOfFile, ':', '\n'}).toSlice(lexer)
#     while value[^1] in OWhitespace:
#       value.pop()

#     args[^1].add orgRawText.newTree(value)

#     # lexer.skip()

#   result = orgCmdArguments.newTree(
#     tern(flags.len > 0, newOStmtList(flags), newEmptyNode()),
#     tern(args.len > 0, newOStmtList(args), newEmptyNode())
#   )

  # lexer.nextLine()



func closingCommand*(cmd: OrgCommandKind): OrgCommandKind =
  const arr = toMapArray {
    ockBeginSrc: ockEndSrc,
    ockBeginExport: ockEndExport,
    ockBeginTable: ockEndTable,
    ockBeginDetails: ockEndDetails
  }

  return arr[cmd]
proc parseUnparsed*(node: var OrgNode, parseConf) =
  if node of orgUnparsed:
    var str = node.strVal().initPosStr((node.line, node.column))
    var lexer = initLexer(str, lexStructure(), some initTok(ostEof))
    node = parseStmt(lexer, parseConf)

  else:
    for item in mitems(node):
      parseUnparsed(item, parseConf)

    if node of orgParagraph and
       len(node) == 1 and
       node[0] of orgParagraph:
      node = node[0]


proc parseCommand*(lexer, parseConf): OrgNode =
  if lexer[+1, ostCommandBegin]:
    var tokens = @[lexer.popAsStr({ostCommandPrefix})]
    let
      id = lexer.popAsStr({ostCommandBegin})
      cmd = id.classifyCommand()

    tokens.last().add id

    var found = false
    while ?lexer and not (
      lexer[ostCommandEnd] and
      lexer[].initPosStr().classifyCommand() == closingCommand(cmd)
    ):
      tokens.add lexer.popAsStr()

    tokens.last().add lexer.popAsStr(ostCommandEnd)

    case cmd:
      of ockBeginSrc:
        result = parseSrcBlock(
          tokens.initLexer(initLexCode()).asVar(),
          parseConf)

      of ockBeginExport:
        result = newTree(
          orgExportCommand,
          newTree(orgIdent, tokens[1]),
          newTree(orgRawText, tokens[2]))

      of ockBeginTable:
        result = parseTable(
          tokens.initLexer(initLexTable()).asVar(), parseConf)

        parseUnparsed(result, parseConf)

      else:
        raise newImplementKindError(cmd)

  else:
    var tokens = @[lexer.popAsStr({ostCommandPrefix})]
    let
      id = lexer.popAsStr({ostLineCommand})
      cmd = id.classifyCommand()

    tokens.last().add id
    case cmd:
      of ockTitle:
        lexer.skip(ostColon)
        result = newTree(
          orgCommandTitle,
          lexer.popAsStr({ostCommandArguments}).parseText(parseConf))

      of ockInclude:
        lexer.skip(ostColon)
        var str = lexer.popAsStr({ostCommandArguments})
        result = newTree(orgCommandInclude)

        if str['"']:
          result.add newTree(
            orgFilePath, str.asSlice(str.skipStringLit(), +1, -2))

        else:
          result.add newTree(orgFilePath, str.asSlice str.skipTo(' '))

        str.space()
        if str["export"]:
          result.add newTree(orgIdent, str.asSlice str.skip("export"))
          str.space()
          result.add newTree(orgIdent, str.asSlice str.skipWhile(IdentChars))
          str.space()

        else:
          result.add newTree(orgIdent, str.asSlice str.skip("export"))
          str.space()
          result.add newTree(orgIdent, str.asSlice str.skipWhile(IdentChars))
          str.space()

        result.add parseCommandArgs(str, parseConf)

      of ockAttrImg, ockHeader:
        lexer.skip(ostColon)
        let resKind =
          case cmd:
            of ockAttrImg: orgAttrImg
            of ockHeader: orgCommandHeader
            else: raise newUnexpectedKindError(cmd)

        result = newTree(
          resKind,
          lexer.popAsStr({ostCommandArguments}).
            asVar().parseCommandArgs(parseConf))

      of ockName:
        lexer.skip(ostColon)
        result = newTree(
          orgCommandName,
          newTree(orgRawText, lexer.popAsStr({ostCommandArguments})))

      of ockCaption:
        lexer.skip(ostColon)
        result = newTree(
          orgCommandCaption,
          lexer.popAsStr({ostCommandArguments}).parseText(parseConf))

      of ockOptions:
        lexer.skip(ostColon)
        var params = lexer.popAsStr({ostCommandArguments})

        result = newCmdArguments()
        while ?params:
          result[orgfArgs].add newTree(
            orgCmdKey, params.asSlice params.skipTo(':'))

          params.skip(':')

          result[orgfArgs].add newTree(
            orgCmdValue, params.asSlice params.skipTo(' '))

          params.space()


        result = newTree(orgCommandOptions, result)

      else:
        raise newImplementKindError(cmd)




# proc `=~`(str: StrSlice, str2: string): bool =
#   normalize($str) == normalize(str2)

proc parseDrawer*(lexer, parseConf): OrgNode =
  result = newTree(orgDrawer)

  when false:
    result.add orgIdent.newTree(
      lexer.getInsideSimple(':', ':').toSlice(lexer))

    lexer.advance()
    var buf = lexer.initEmptyStrRanges()
    while not (lexer[":end:"] or lexer.atEnd()):
      buf.add lexer.pop()

    buf.pop()

    var propLexer = newSublexer(buf.toSlice(lexer))
    var proplist = newOStmtList()
    if result["name"].text =~ "properties":
      while propLexer[":"]:
        var prop = orgProperty.newTree()
        propLexer.skipExpected(":")
        prop.add propLexer.parseIdent()

        propLexer.skipExpected(":")
        if propLexer.allUntil(OIdentChars, {':'}):
          prop.add propLexer.parseIdent()
          propLexer.skipExpected(":")

        else:
          prop.add newEmptyNode()

        if prop["name"].text =~ "header-args":
          propLexer.skip()
          prop.add propLexer.parseCmdArguments(parseConf)
          propLexer.advance()

        else:
          propLexer.skip()
          prop.add orgRawText.newTree(
            propLexer.getSkipToEOL().toSlice(lexer))

        propList.add prop

      result.add propList

    elif result["name"].text =~ "logbook":
      result.add propLexer.parseStmtList(parseConf)

    else:
      result.add orgRawText.newTree(buf.toSlice(lexer))



    lexer.nextLine()




proc parseDrawers*(lexer, parseConf): OrgNode =
  ## Parse one or mode drawers starting on current line.
  when false:
    if lexer.lineStartsWith(":"):
      result = orgStmtList.newTree()

      while lexer[] == ':':
        result.add parseDrawer(lexer, parseConf)



    else:
      return newEmptyNode()





proc parseResultBlock*(lexer, parseConf): OrgNode =
  lexer.skip(ostCommandPrefix)
  lexer.skip(ostIdent, "results")

  result = newTree(orgResult)

  if lexer[ostCommandBracket]:
    result.add orgRawText.newTree(lexer.pop())

  else:
    result.add newEmptyNode()

  if lexer[ostColonIdent]:
    result.add lexer.parseDrawer(parseConf)

  elif lexer[ostColonLiteral]:
    result.add newTree(orgRawText, lexer.pop())

  # else:
  #   result.add orgRawText.newTree(lexer.getSkipToEOL().toSlice(lexer))

# proc searchResult*(lexer, parseConf): int =
#   var ahead = lexer
#   result = -1
#   while not ahead.atEnd():
#     if ahead[] notin {'#', '\n', ' '}:
#       return -1

#     else:
#       ahead.skip(Whitespace)

#       if not ahead["#+results"]:
#         ahead.nextLine()

#       else:
#         return ahead.d.bufpos



proc parseMultilineCommand*(
    lexer, parseConf;
    balanced: bool = true,
    parseInside: bool = false
  ): OrgNode =
  ## Parse multiline command starting from `#+begin` and ending with
  ## `#+end`.
  ##
  ## - @arg{balanced} :: body of the multiline command might contains
  ##   unundented pairs of `#+begin/#+end` blocks with the same name.
  ## - @arg{parseInline} :: parse body of multiline command as statement
  ##   list, or cut it verbatim into resulting AST in form of multiline
  ##   code block
  # TODO control kind of resulting cutout block.


  result = newTree(orgMultilineCommand)
  lexer.skip(ostCommandPrefix)
  lexer.skip(ostCommandBegin)
  let cmd = lexer.pop(ostIdent)


proc parseOrgCookie*(lexer, parseConf): OrgNode =
  raise newImplementError()

proc parseList*(lexer, parseConf): OrgNode =
  # This is a horrible nest of vaguely justified checks, paired with
  # multiple assumptions on input validty, but general outline of
  # implementation is as follows:
  #
  # - in loop,determine particular type of current list start, get
  #   bullet chars.
  # - Create sublexer for whole item
  # - Partiallt parse item - counter, checkbox
  # - Found extent of list 'header', positions of completion cookies if any
  # - Create separate header sublexer, determine ranges for tag (for property list),
  #   header text and completion ranges.
  # - Create `tag`, `header` and `completion` subnodes from ranges parsed in
  #   previous step.
  #
  # Such convoluted implementation is necessary because positions of `::`
  # and `[/]` (especially `::`) are not well-defined. Tag separator might
  # occur anywhere in the list item header and completion cookie is a last
  # element in list /if/ it is present. Approach like this (rough scan
  # forward, then go back to scanned range, and run more intricate scan but
  # without worrying about start/end correctness) should allow (in theory)
  # for more sophisticated
  result = newTree(orgList)
  const starts = {ostListDash, ostListStar, ostListPlus}
  lexer.expectKind(starts)
  while ?lexer and lexer[starts]:
    var item = newTree(orgListItem)

    block bullet:
      item.add newTree(orgBullet, lexer.popAsStr(starts))

    block counter:
      item.add newOrgEmptyNode()

    block checkbox:
      if lexer[ostCheckbox]:
        item.add newTree(orgCheckbox, lexer.popAsStr(ostCheckbox))

      else:
        item.add newOrgEmptyNode()

    block tag:
      item.add newOrgEmptyNode()

    block header:
      item.add parseText(lexer.popAsStr(ostText), parseConf)

    block completion:
      item.add newOrgEmptyNode()

    block body_block:
      var body = newTree(orgStmtList)
      if lexer[ostIndent]:
        lexer.next()
        while ?lexer and not lexer[ostDedent]:
          body.add parseStmt(lexer, parseConf)
          discard lexer.trySkip({ostSameIndent})

        if ?lexer:
          lexer.skip(ostDedent)

      item.add body

    result.add item


  when false:
    while lexer.listStartChar() != OEndOfFile:
      let start: char = lexer.listStartChar()
      let endset: set[char] = tern(start in ONumberedListChars, {'.', ')'}, {})
      let skipset: set[char] =
        case start:
          of '0': {'0' .. '9'}
          of 'a': {'a' .. 'z'}
          of 'A': {'A' .. 'Z'}
          of '-': {'-'}
          of '+': {'+'}
          of '*': {'*'}
          else: raiseAssert("#[ IMPLEMENT ]#")

      var bullet: StrRanges
      while lexer[] in skipset + endset:
        bullet.add lexer.pop()

      lexer.skip()
      var itemLexer = lexer.indentedSublexer(
        2,
        keepNewlines = true,
        fromInline = true,
        requireContinuation = false
      )


      let bulletSlice = $bullet.toSlice(lexer)
      let bulletClass =
        case bulletSlice:
          of "-": oskDashBullet
          of "+": oskPlusBullet
          of "*": oskStarBullet
          elif bulletSlice[0] in {'0' .. '9'}: oskNumBullet
          elif bulletSlice[0 .. ^2] in {'a' .. 'z', 'A' .. 'Z'}: oskLetterBullet
          elif bulletSlice[0 .. ^2] in {'I', 'M', 'x', 'V', 'v', 'i', 'm', 'x'}:
            oskRomanBullet

          else:
            oskNone



      var item = orgListItem.newTree(
        bulletClass,
        orgRawText.newTree(bullet.toSlice(lexer)))

      itemLexer.skip()
      # Parse counter-set and checkbox
      if itemLexer[] == '[':
        if itemLexer[+1] == '@':
          item.add orgCounter.newTree(
            itemLexer.getInsideSimple('[', ']').toSlice(lexer))

          itemLexer.skip()
          if itemLexer[] == '[':
            item.add orgCheckbox.newTree(
              itemLexer.getInsideSimple('[', ']').toSlice(lexer))

          else:
            item.add newEmptyNode()

        else:
          item.add newEmptyNode()

      else:
        item.add newEmptyNode()
        item.add newEmptyNode()

      # Extract header ranges for list element
      var headerRanges: StrRanges
      while true:
        # Crude heuristics, but it should work for now
        if itemLexer[] in OLineBreaks and itemLexer[+1] notin OWordChars:
          break

        headerRanges.add itemLexer.pop
        while itemLexer[] notin OLineBreaks:
          headerRanges.add itemLexer.pop

      # Create sublexer for header ranges and get subranges for tags and
      # completion cookies.
      var it = headerRanges.toSlice(lexer).newSublexer()
      let tagRanges = it.allRangesTo(
        "::",
        repeatIncluding = true,
        remaining = true # Get all indices in ranges
      )

      let cookieRanges = it.allRangesTo("[", remaining = true)

      var isValidCookie = true
      if cookieRanges.len > 1:
        # If completion cookie has multiple ranges and
        let sl = cookieRanges[^1].toSlice(lexer)
        for idx in rindices(sl):
          if sl.absAt(idx) notin {']', '[', '1' .. '9', '%', '/'}:
            isValidCookie = false
            break

      else:
        isValidCookie = false

      # There is no smart logic behind this wall of conditionals, it is just
      # a reasult of defensive coding against possible edge cases that I
      # managed to come up with.
      if tagRanges.len > 1:
        item.add tagRanges[0].toSlice(lexer).newSublexer().withResIt do:
          it.parseParagraph(parseConf, oskListTagText)

        let paragraph = overlapping(
          @[tagRanges[0], tagRanges[1]], tern(
            isValidCookie, # If completion cookie is present use it
            cookieRanges[^2],
            tagRanges[^1] # Otherwise get everything until the end of header
          )
        )

        if paragraph.len == 0:
          # Degenerate case with tag body being the only part in header. This
          # is a valid syntax, so no error here.
          item.add newEmptyNode()

        else:
          item.add paragraph.toSlice(lexer).newSublexer().withResIt do:
            it.parseParagraph(parseConf, oskListHeaderText)

      elif tagRanges.len == 1:
        item.add newEmptyNode()

        if isValidCookie:
          let overlap = overlapping(
            @[overlapping( # Get range for completion cookie
              @[cookieRanges[^2]], # Cookie will start at `[]` as usual
              cookieRanges[^1],
            )],
            tagRanges[0] # In case of missing `::` tag range will contain all
                         # header, need to exclude ranges for completion
                         # cookie.
          )

          item.add overlap.toSlice(lexer).newSublexer().withResIt do:
            it.parseParagraph(parseConf)

        else:
          item.add tagRanges[0].toSlice(lexer).newSublexer().withResIt do:
            it.parseParagraph(parseConf)


      if isValidCookie:
        item.add overlapping(
          @[cookieRanges[^2]], cookieRanges[^1]
        ).toSlice(lexer).newSublexer.withResIt do:
          orgCompletion.newTree(it.getInsideSimple('[', ']').toSlice(lexer))

      else:
        item.add newEmptyNode()

      item.add itemLexer.parseStmtList(parseConf)
      if item[^1].len == 0:
        item[^1] = newEmptyNode(oskListBodyText)

      else:
        item[^1].subKind = oskListBodyText

      result.add item


    var bullets: set[OrgNodeSubKind]
    var hasDescriptions: bool
    var allDescriptions = true
    for item in result:
      bullets.incl item.subKind
      if item["tag"].kind != orgEmptyNode:
        hasDescriptions = true

      else:
        allDescriptions = false

    if allDescriptions:
      result.subKind = oskFullDescList

    if hasDescriptions:
      result.subKind = oskPartialDescList

    elif bullets <= {oskDashBullet, oskPlusBullet, oskStarBullet}:
      result.subKind = oskUnorderedList

    elif bullets <= {oskNumBullet, oskLetterBullet, oskRomanBullet}:
      result.subKind = oskOrderedList

    else:
      result.subKind = oskMixedList


proc parseSubtree*(lexer, parseConf): OrgNode =
  ## Parse header node.
  ## - NOTE :: Only subtree header is parsed - @ret{["body"]} is
  ##   set to empty node and should be handled externally.
  # NOTE `@ret{["body"]}` should be rendered as `result["body"]`

  result = newTree(orgSubtree)

  let currentStars = lexer.popAsStr({ostSubtreeStars})

  block stars:
    result.add newTree(orgSubtreeStars, currentStars)

  block todo_status:
    if lexer[ostSubtreeTodoState]:
      result.add newTree(orgBigIdent, lexer.popAsStr())

    else:
      result.add newOrgEmpty()

  block importance:
    if lexer[ostSubtreeImportance]:
      result.add newTree(orgUrgencyStatus, lexer.popAsStr())

    else:
      result.add newOrgEmpty()

  block subtree_title:
    result.add parseText(lexer.popAsStr({ostText}), parseConf)

  block subtree_completion:
    if lexer[ostSubtreeCompletion]:
      result.add newTree(orgCompletion, lexer.popAsStr())

    else:
      result.add newOrgEmpty()

  block tree_tags:
    if lexer[ostSubtreeTag]:
      result.add newTree(orgOrgTag, lexer.popAsStr())

    else:
      result.add newOrgEmpty()

  block subtree_time:
    if lexer[ostSubtreeTime]:
      var times = newTree(orgSubtreeTimes)
      while lexer[ostSubtreeTime]:
        times.add newTree(orgInlineStmtList, @[
          newTree(orgIdent, lexer.popAsStr({ostSubtreeTime})),
          newTree(orgTimeStamp, lexer.popAsStr({ostAngleTime, ostBracketTime}))
        ])

      result.add times

    else:
      result.add newOrgEmpty()

  block tree_drawer:
    var drawer = newTree(orgDrawer)

    block drawer_properties:
      if lexer[ostColonProperties]:
        var props = newTree(orgPropertyList)
        lexer.skip(ostColonProperties)

        while lexer[ostColonIdent]:
          props.add newTree(
            orgProperty,
            newTree(orgIdent, lexer.popAsStr(ostColonIdent)),
            tern(
              lexer[ostIdent],
              newTree(orgIdent, lexer.popAsStr(ostIdent)),
              newTree(orgEmpty)
            ),
            parseCommandArgs(lexer.popAsStr(ostRawProperty), parseConf)
          )

        lexer.skip(ostColonEnd)
        drawer.add props

      else:
        drawer.add newOrgEmpty()

    block drawer_logbook:
      if lexer[ostColonLogbook]:
        var list = newTree(orgStmtList)
        lexer.skip(ostColonLogbook)
        lexer.skip(ostRawLogbook) # TODO Parse as new statement list
        lexer.skip(ostColonEnd)


        drawer.add newTree(orgLogbook, list)

      else:
        drawer.add newOrgEmpty()

    result.add drawer

  block subtree_body:
    var body = newTree(orgStmtList)
    var subtreeEnd = false
    while ?lexer and not subtreeEnd:
      case lexer[].kind:
        of ostSubtreeStars:
          let stars = lexer[]
          if currentStars.strVal().len < stars.strVal().len:
            body.add parseStmt(lexer, parseConf)

          else:
            subtreeEnd = true

        of ostEof:
          subtreeEnd = true

        else:
          body.add parseStmt(lexer, parseConf)

    result.add body

  # echov lexer[]
  # raise newImplementError()

  when false:
    result.add orgBareIdent.newTree(lexer.getSkipWhile({'*'}).toSlice(lexer))
    lexer.skip()

    if lexer.atBigIdent():
      result.add lexer.parseBigIdent(parseConf, oskTodoIdent)

    else:
      result.add newEmptyNode(oskTodoIdent)

    lexer.skip()

    result.add parseOrgCookie(lexer, parseConf)

    lexer.skip()

    var headerLexer = lexer.indentedSublexer(
      result["prefix"].charLen(),
      keepNewlines = false,
      requireContinuation = true,
      fromInline = true
    )


    var
      tagsElems: seq[int]
      tagsFound = false
      completionElems: seq[int]
      completionFound = false
      headerElems: seq[int]
      allIdx: seq[int] = toSeq(indices(headerLexer.d.buf))


    var pos = allIdx.high
    while pos >= 0:
      if not tagsFound:
        if lexer.absAt(allIdx[pos]) == ':':
          var tagEnded = false
          while not tagEnded:
            dec pos
            while lexer.absAt(allIdx[pos]) notin OWhitespace:
              tagsElems.add allIdx[pos]

              if lexer.absAt(allIdx[pos]) == ':':
                dec pos

              else:
                break

            if lexer.absAt(allIdx[pos]) in OWhitespace:
              tagEnded = true


        tagsFound = true

      elif not completionFound:
        if lexer.absAt(allIdx[pos]) in OWhitespace:
          dec pos

        elif lexer.absAt(allIdx[pos]) == ']':
          var subbuf: seq[int]
          dec pos
          while lexer.absAt(allIdx[pos]) in {'0' .. '9', '/', '%'}:
            subbuf.add allIdx[pos]
            dec pos

          if lexer.absAt(allIdx[pos]) == '[':
            dec pos
            completionElems = subbuf

          else:
            headerElems.add subbuf

          completionFound = true

        else:
          completionFound = true

      else:
        headerElems.add allIdx[pos]
        dec pos

    block: # subtree title
      var headerBuf: StrRanges
      for idx in reversed(headerElems):
        headerBuf.add idx

      result.add headerBuf.toSlice(lexer).newSublexer().withResIt do:
        parseParagraph(it, parseConf)

    block:
      if completionElems.len > 0:
        var completionSlice: StrRanges
        for idx in reversed(completionElems):
          completionSlice.add idx

        result.add orgRawText.newTree(completionSlice.toSlice(lexer))

      else:
        result.add newEmptyNode()


    block:
      if tagsElems.len > 0:
        var tagsBuf = @[lexer.initEmptyStrRanges()]
        for idx in reversed(tagsElems):
          if lexer.absAt(idx) == ':':
            tagsBuf.add lexer.initEmptyStrRanges()

          else:
            tagsBuf[^1].add idx

        result.add orgOrgTag.newTree()
        for buf in tagsBuf:
          if buf.len > 0:
            result[^1].add orgRawText.newTree(oskOrgTagIdent, buf.toSlice(lexer))

      else:
        result.add newEmptyNode()


    lexer.advance()

    var timesLexer = lexer
    timesLexer.skip()
    if timesLexer.atBigIdent():
      var times = newOStmtList()
      while timesLexer.atBigIdent():
        times.add orgSubtreeTimes.newTree()
        times[^1].add timesLexer.parseBigIdent(parseConf)
        timesLexer.skipExpected(":")
        timesLexer.skip()
        if timesLexer["<"]:
          times[^1].add orgTimeStamp.newTree(
            timesLexer.getInsideBalanced('<', '>'))

        elif timesLexer["["]:
          times[^1].add orgTimeStamp.newTree(
            timesLexer.getInsideBalanced('[', ']'))

        timesLexer.skip()

      result.add times

      lexer.d.bufpos = timesLexer.d.bufpos

    else:
      result.add newEmptyNode()

    lexer.gotoSOL()



    if lexer.lineStartsWith(":"):
      var drawerLexer = lexer.indentedSublexer(
        lexer.getIndent(),
        keepNewlines = true,
        requireContinuation = false,
        fromInline = false
      )

      result.add parseDrawers(drawerLexer, parseConf)

    else:
      result.add newEmptyNode()

    result.add newEmptyNode()

proc parseStmtList*(lexer, parseConf): OrgNode =
  result = newTree(orgStmtList)
  while ?lexer and not lexer[ostEof]:
    let stmt = parseStmt(lexer, parseConf)
    result.add stmt



proc parseStmt*(lexer, parseConf): OrgNode =
  case lexer[].kind:
    of ostListDash, ostListStar, ostListPLus:
      result = parseList(lexer, parseConf)

    of ostCommandPrefix:
      result = parseCommand(lexer, parseConf)

    of ostSubtreeStars:
      result = parseSubtree(lexer, parseConf)

    of ostText:
      # echov lexer[]
      # # echov lexer[+1]
      # echov lexer.pos
      result = parseText(lexer.popAsStr(), parseConf)
      # echov ?lexer
      # echov lexer.tokens
      # echov lexer.pos
      # echov lexer.str[]
      # echov lexer[]

    else:
      raise newUnexpectedTokenError(lexer)

  if result of orgParagraph and result.len == 1 and result[0] of orgLink:
    # Standalone links should be put on toplevel in order to handle
    # associative links correctly.
    result = result[0]


const defaultParseConf*: ParseConf = ParseConf(
  dropEmptyWords: true
)

proc parseOrg*(
    str: var PosStr, parseConf: ParseConf = defaultParseConf): OrgNode =

  var lexer = initLexer[OrgStructureToken](
    str, lexStructure(), some initTok(ostEof))

  result = parseStmtList(lexer, parseConf)
