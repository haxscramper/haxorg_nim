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
