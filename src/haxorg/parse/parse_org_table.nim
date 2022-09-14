import
  ../defs/[org_types, impl_org_node]

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all,
  ./parse_org_common,
  ./parse_org_command





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
