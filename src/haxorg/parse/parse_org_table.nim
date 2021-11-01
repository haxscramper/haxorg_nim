import
  ../defs/[org_types, impl_org_node]

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all,
  ./parse_org_common,
  ./parse_org_command


type
  OrgTableTokenKind* = enum
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
    otaEof

  OrgTableToken* = HsTok[OrgTableTokenKind]
  OrgTableLexer* = HsLexer[OrgTableToken]


proc initLexTable*(): HsLexCallback[OrgTableToken] =
  var state = newLexerState(oblsNone)
  proc lexTable(str: var PosStr): seq[OrgTableToken] =
    if not ?str:
      result.add str.initEof(otaEof)

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


proc parseTable*(lexer: var OrgTableLexer, conf: ParseConf): OrgNode

proc parseCmdArguments*(lexer: var OrgTableLexer, conf: ParseConf): OrgNode =
  parseCommandArgs(lexer.popAsStr({otaCmdArguments}), conf)

proc parseCell*(lexer: var OrgTableLexer, conf: ParseConf): OrgNode =
  result = newTree(orgTableCell)
  case lexer[].kind:
    of otaCellSpec:
      lexer.skip(otaCellSpec)

      result.add parseCmdArguments(lexer, conf)

      var par = newTree(orgStmtList)
      while lexer[otaContent]:
        par.add newTree(orgUnparsed, lexer.popAsStr(otaContent))

      result.add par

    of otaContent:
      result.add newCmdArguments()
      result.add newTree(
        orgParagraph, newTree(orgUnparsed, lexer.popAsStr(otaContent)))

    else:
      raise newUnexpectedKindError(lexer[])


proc parseRow*(lexer: var OrgTableLexer, conf: ParseConf): OrgNode =
  case lexer[].kind:
    of otaPipeOpen:
      result = newTree(orgTableRow)
      result.add newTree(orgEmpty) # Command parameters
      result.add newTree(orgEmpty)
      while lexer[{otaPipeSeparator, otaPipeOpen}]:
        lexer.next()
        result.add parseCell(lexer, conf)

      lexer.skip(otaPipeClose)

    of otaRowSpec:
      result = newTree(orgTableRow)
      lexer.skip(otaRowSpec)

      # IMPLEMENT cmd parsing
      result.add newTree(orgEmpty)
      lexer.skip(otaCmdArguments)

      result.add newTree(orgEmpty)
      while ?lexer and lexer[otaCellSpec]:
        result.add parseCell(lexer, conf)

    else:
      raise newImplementKindError(lexer[])

proc parseTable*(lexer: var OrgTableLexer, conf: ParseCoNf): OrgNode =
  case lexer[].kind:
    of otaTableBegin:
      lexer.skip(otaTableBegin)
      result = newTree(orgTable)
      if lexer[otaCmdArguments]:
        result.add parseCmdArguments(lexer, conf)

      else:
        result.add newCmdArguments()

      while ?lexer and not lexer[otaTableEnd]:
        result.add parseRow(lexer, conf)

      lexer.skip(otaTableEnd)


    else:
      raise newImplementKindError(lexer[])
  # result = orgTable.newTree(parentRes[^1])
  # var sublexer = newSublexer(
  #   lexer.get,
  #   lexer.getBlockUntil("#+end-table")
  # )

  # type
  #   RowFormatting = enum
  #     rfCompact
  #     rfOneline
  #     rfStmtList

  # var rows = newOStmtList()
  # var rowArgs: OrgNode
  # while sublexer[] != OEndOfFile:
  #   rowArgs = sublexer.parseCommand(parseConf)[1]
  #   let body = sublexer.getBlockUntil("#+row")
  #   var cformat: RowFormatting

  #   block cellKind:
  #     var rowlexer = lexer.newSublexer(body)
  #     while true:
  #       if rowlexer[] in {'#', '\n'}:
  #         if rowlexer["#+cell:"]:
  #           cformat = rfStmtList
  #           break cellKind

  #         else:
  #           discard rowlexer.getSkipToEOL()
  #           rowlexer.advance()

  #       elif rowlexer[] in {' '}:
  #         discard rowlexer.skip()
  #         if rowlexer[] == '|':
  #           raise newImplementError()

  #         elif rowlexer[] in {'#', '\n'}:
  #           discard rowlexer.getSkipToEOL()
  #           rowlexer.advance()

  #         else:
  #           raise newImplementError(rowlexer.error("????").msg)

  #       elif rowlexer[] == '|':
  #         discard rowlexer.getSkipToEOL()
  #         if rowlexer[-1] == '|':
  #           cformat = rfCompact
  #           break cellKind

  #         else:
  #           cformat = rfOneline
  #           break cellKind

  #       else:
  #         raiseAssert("#[ IMPLEMENT ]#")


  #   var resrow = orgTableRow.newTree(rowArgs)
  #   block parseCell:
  #     var
  #       rowlexer = lexer.newSublexer(body)
  #       rowtext = newOStmtList()
  #       rowcells = newOStmtList()

  #     case cformat:
  #       of rfCompact:

  #         while rowlexer[] != OEndOfFile:
  #           if rowlexer[] == '|':
  #             rowlexer.advance()
  #             var cells = rowLexer.getSkipToEOL()
  #             lexer.advance()
  #             cells.pop()

  #             for elem in cells.toSlice(lexer).split('|'):
  #               rowcells.add orgTableCell.newTree(
  #                 newEmptyNode(), newWord(
  #                   elem.toSlice(lexer).strip().toSlice(lexer)))

  #           else:
  #             let slice = rowlexer.getSkipToEOL().toSlice(lexer)
  #             if slice.len > 0:
  #               rowtext.add newWord(slice)
  #             rowlexer.advance()

  #       of rfOneLine:
  #         while rowlexer[] != OEndOfFile:
  #           if rowlexer[] == '|':
  #             rowlexer.advance()
  #             rowlexer.skip()
  #             rowcells.add orgTableCell.newTree(
  #               newEmptyNode(),
  #               newWord(rowLexer.getSkipToEOL().toSlice(lexer))
  #             )

  #             rowlexer.advance()

  #           else:
  #             let slice = rowlexer.getSkipToEOL().toSlice(lexer)
  #             if slice.len > 0:
  #               rowtext.add newWord(slice)

  #             rowlexer.advance()

  #       of rfStmtList:
  #         let pos = rowlexer.getPosition()
  #         rowtext.add newWord(
  #           rowlexer.getBlockUntil("#+cell:").toSlice(lexer)
  #         )

  #         while rowlexer[] != OEndOfFile:
  #           assert rowlexer[0 .. 6] == "#+cell:"
  #           rowcells.add orgTableCell.newTree(
  #             rowlexer.parseCommand(parseConf),
  #             newWord(
  #               rowlexer.getBlockUntil("#+cell:").toSlice(lexer)
  #             )
  #           )

  #     resrow.add rowtext
  #     resrow.add rowcells



  #   result.add resrow

proc parseTable*(str: PosStr, conf: ParseConf): OrgNode =
  var lexer = initLexer(str, initLexTable())
  result = parseTable(lexer, conf)
