## Perform initial pre-parsing of the code block - this is needed to
## differentiate between regular code body and noweb slices. The
## implementation is guaranteed to be shared across all code block
## implementations, which means it can be done directly during initial
## parsing stage.

import
  ../defs/[org_types, impl_org_node],
  ./parse_org_command,
  ./parse_org_common

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all,
  hmisc/other/hpprint


type
  OrgCodeTokenKind = enum
    octkNone

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

    octkEof

  OrgCodeToken* = HsTok[OrgCodeTokenKind]
  OrgCodeLexer* = HsLexer[OrgCodeToken]


proc initLexCode*(): HsLexCallback[OrgCodeToken] =
  var state = newLexerState(oblsNone)
  return proc(str: var PosStr): seq[OrgCodeToken] =
    if not ?str:
      result.add str.initEof(octkEof)

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


using
  lexer: var OrgCodeLexer
  parseConf: ParseConf

proc initCodeLexer*(str: PosStr): OrgCodeLexer =
  initLexer(str, initLexCode())

proc parseSrcInline*(lexer, parseConf): OrgNode =
  when false:
    assert lexer["src_"]
    lexer.advance(4)
    result = orgSrcCode.newTree()
    result.add lexer.parseIdent()
    case lexer[]:
      of '[':
        result.add orgRawText.newTree(lexer.getInsideBalanced('[', ']'))
        result.add orgRawText.newTree(lexer.getInsideBalanced('{', '}'))

      of '{':
        result.add newEmptyNode()
        result.add orgRawText.newTree(lexer.getInsideBalanced('{', '}'))

      else:
        raiseAssert("#[ IMPLEMENT ]#")


    lexer.skip()
    result.add parseOptMacro(lexer, parseConf)

proc parseCallInline*(lexer, parseConf): OrgNode =
  when false:
    assert lexer["call_"]
    lexer.advance(5)
    result = orgCallCode.newTree()
    result.add lexer.parseIdent()
    case lexer[]:
      of '[':
        result.add lexer.getInsideBalanced('[', ']').newSublexer().withResIt do:
          parseCmdArguments(it, parseConf)

        lexer.skip()
        result.add orgRawText.newTree(
          lexer.getInsideBalanced('(', ')'))

      of '(':
        result.add newEmptyNode()
        lexer.skip()
        result.add orgRawText.newTree(lexer.getInsideBalanced('(', ')'))

      else:
        raiseAssert("#[ IMPLEMENT ]#")

    lexer.skip()
    if lexer[] == '[':
      result.add lexer.getInsideBalanced('[', ']').newSublexer().withResIt do:
        parseCmdArguments(it, parseConf)

    else:
      result.add newEmptyNode()

    lexer.skip()
    result.add parseOptMacro(lexer, parseConf)


proc parseSrcBlock*(lexer; parseConf: ParseConf): OrgNode =
  lexer.skip(octkCommandPrefix)
  lexer.skip(octkCommandBegin)
  result = newTree(orgSrcCode)

  result.add newTree(orgIdent, lexer.popAsStr({octkLangName}))
  if lexer[].isFake:
    result.add newTree(
      orgCmdArguments,
      newTree(orgInlineStmtList),
      newTree(orgInlineStmtList))

    lexer.skip(octkCommandArgs)

  else:
    let tokens = lexer.popAsStr({octkCommandArgs})
    result.add parseCommandArgs(tokens, parseConf)

  var codeLines: seq[OrgNode] = @[newTree(orgCodeLine)]
  while ?lexer:
    case lexer[].kind:
      of octkTextBlock:
        codeLines.last().add newTree(orgCodeText, lexer.popAsStr())

      of octkCalloutOpen:
        lexer.skip({octkCalloutOpen})
        codeLines.last().add newTree(orgCodeCallout, lexer.popAsStr())
        lexer.skip({octkCalloutClose})

      of octkNewline:
        lexer.next()
        if codeLines.last().len == 0:
          codeLines.last().add newTree(orgEmpty)

        codeLines.add newTree(orgCodeLine)

      of octkCommandEnd:
        break

      else:
        raise newImplementKindError(lexer[])

  result.add newTree(orgStmtList, codeLines)
  lexer.skip(octkCommandEnd)

  result.add newOrgEmpty()

proc parseCallBlock*(lexer; parseConf: ParseConf): OrgNode =
  raise newImplementError()

proc parseCallLine*(lexer; parseConf: ParseConf): OrgNode =
  raise newImplementError()
