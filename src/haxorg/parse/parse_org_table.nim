import
  ../defs/org_types

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all


type
  OrgTableTokenKind = enum
    ottkCellBody ## Unformatted table cell body
    ottkRowSpec ## `#+row` command together with parameters
    ottkColumnSpec ## `#+column` command with parameters
    ottkPipeSeparator ## Vertical pipe (`|`) cell separator
    ottkDashSeparator ## Horizontal dash (`---`, `:---`, `---:` or `:---:`)
                      ## row separator
    ottkCornerPlus ## Corner plus (`+`)

  OrgTableToken* = HsTok[OrgTableTokenKind]
  OrgTableLexer* = HsLexer[OrgTableToken]




proc lexCommand(str: var PosStr): seq[OrgCommandToken] =
  if not ?str:
    result.add str.initEof(octEof)

  else:
    case str[]:
      else:
        raise newUnexpectedCharError(str)

proc parseOrgTable*(
    lexer: var OrgTableLexer,
    parseConf: ParseConf,
    parentRes: OrgNode
  ): OrgNode =
  raise newImplementError()
  # result = orgTable.newTree(parentRes[^1])
  # var sublexer = newSublexer(
  #   lexer.getBuf(),
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
