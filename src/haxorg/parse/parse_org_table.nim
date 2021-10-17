import
  ../defs/org_types

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all


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




proc lexTable*(str: var PosStr): seq[OrgTableToken] =
  if not ?str:
    result.add str.initEof(otaEof)

  else:
    case str[]:
      of '#':
        if str["#+begin-table"]:
          result.add str.initTok(otaTableBegin, str.asSlice(str.skip("#+begin-table")))

        elif str["#+row"]:
          result.add str.initTok(otaRowSpec, str.asSlice(str.skip("#+row")))

        elif str["#+cell"]:
          result.add str.initTok(otaCellSpec, str.asSlice(str.skip("#+cell")))

        elif str["#+end-table"]:
          result.add str.initTok(otaTableEnd, str.asSlice(str.skip("#+end-table")))

        else:
          raise newImplementError()

        str.space()
        result.add str.initTok(otaCmdArguments, str.asSlice(
          str.skipUntil('\n', including = true)))

        if ?str:
          str.skip('\n')

      of '|':
        let pos = str.getPos()
        str.skipBeforeEol()
        if str['|']:
          str.setPos(pos)

          var first = true
          dowhile str['|']:
            result.add str.initTok(
              tern(first, otaPipeOpen, otaPipeSeparator),
              str.asSlice str.skip('|'))

            first = false

            str.space()
            let tok = str.initTok(otaContent):
              str.asSlice():
                str.skipBefore('|')
                if str[' ']:
                  while str[' ']: str.back()
                  if not str[' ']: str.next()

                else:
                  if not str['\n']:
                    str.next()

            result.add tok
            str.space()
          # var last: seq[OrgTableToken]
          # while str['|']:
          #   last.add str.initTok(otaPipeSeparator, str.asSlice str.skip('|'))
          #   str.space()
          #   last.add str.initTok(otaContent, str.asSlice str.skipTo('|'))

          # result.add last[0..^3]
          echov "---"
          discard result.pop()
          discard result.pop()
          result.add str.initTok(otaPipeClose)
          eachIt(result, echov it)

        else:
          str.setPos(pos)
          result.add str.initTok(otaPipeCellOpen, str.asSlice str.skip('|'))
          str.space()
          result.add str.initTok(otaContent, str.asSlice str.skipToEol())

        if ?str:
          str.skip('\n')

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
