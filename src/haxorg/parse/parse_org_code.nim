## Perform initial pre-parsing of the code block - this is needed to
## differentiate between regular code body and noweb slices. The
## implementation is guaranteed to be shared across all code block
## implementations, which means it can be done directly during initial
## parsing stage.

import
  ../defs/org_types,
  ./parse_org_command

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all


type
  OrgCodeTokenKind = enum
    octkCommands
    octkBody
    octkNowebOpen ## `<<` - open for noweb placeholder
    octkNowebClose ## `>>` - close for noweb placeholder
    octkNowebName ## Name of the noweb placeholder
    octkNowebLpar ## Lpar of the noweb placeholder arguments
    octkNowebRpar ## RPar of the noweb placeholder arguments
    octkNowebComma ## Noweb argument separator
    octkNowebArg ## Noweb argument
    octkPreNoweb ## Code before noweb placeholder. Requires separate token
                 ## to handle `##<<commented>>` - prefix comment should be
                 ## duplicated for each line of the placeholder expansion.

    octkCallout

  OrgCodeToken* = HsTok[OrgCodeTokenKind]
  OrgCodeLexer* = HsLexer[OrgCodeToken]




proc lexCommand(str: var PosStr): seq[OrgCommandToken] =
  if not ?str:
    result.add str.initEof(octEof)

  else:
    case str[]:
      else:
        raise newUnexpectedCharError(str)

# proc parseNowebBlock*(lexer, parseConf): OrgNode =
#   result = orgNowebMultilineBlock.newTree()
#   while not lexer.atEnd():
#     var nowRange: StrRanges
#     while not (lexer["<<"] or lexer.atEnd()):
#       nowRange.add lexer.pop()

#     result.nowebBlock.slices.add NowebSlice(
#       slice: nowRange.toSlice(lexer)
#     )


#     if not lexer.atEnd():
#       lexer.advance()
#       var body = lexer.getInsideBalanced('<', '>')
#       result.nowebBlock.slices.add NowebSlice(
#         isPlaceholder: true,
#         slice: body
#       )
#       lexer.advance()


# proc parseSnippetBlock*(lexer, parseConf): OrgNode =
#   result = orgSnippetMultilineBlock.newTree()
#   while not lexer.atEnd():
#     var nowRange: StrRanges
#     while not ((lexer["$"] and lexer[+1] in {'0' .. '9', '{'}) or lexer.atEnd()):
#       nowRange.add lexer.pop()

#     result.snippetBlock.slices.add SnippetSlice(
#       slice: nowRange.toSlice(lexer)
#     )

#     if not lexer.atEnd():
#       lexer.advance()
#       if lexer[] in {'0' .. '9'}:
#         result.snippetBlock.slices.add SnippetSlice(
#           isPlaceholder: true,
#           slice: lexer.initStrRanges().toSlice(lexer)
#         )
#         lexer.advance()

#       else:
#         var body = lexer.getInsideBalanced('{', '}')
#         result.snippetBlock.slices.add SnippetSlice(
#           isPlaceholder: true,
#           hasBody: true,
#           slice: body
#         )

# proc parseOrgSource*(lexer, parseConf; parentRes: OrgNode): OrgNode =
#   result = orgSrcCode.newTree()

#   var argsLexer = newSublexer(parentRes[1].text)

#   argsLexer.skip()
#   result.add argsLexer.parseIdent()
#   argsLexer.skip()
#   result.add argsLexer.parseCmdArguments(parseConf)


#   result.add orgVerbatimMultilineBlock.newTree(
#     lexer.getBlockUntil("#+end").toSlice(lexer))

#   lexer.nextLine()

#   let idx = lexer.searchResult(parseConf)
#   if idx > 0:
#     var prefCmds: seq[OrgNode]
#     while not lexer["#+results"]:
#       while lexer[] in OLineBreaks + OWhitespace:
#         lexer.advance()

#       prefCmds.add lexer.parseCommand(parseConf)

#     result.add orgAssocStmtList.newTree(
#       orgStmtList.newTree(prefCmds),
#       lexer.parseResultBlock(parseConf)
#     )

#   else:
#     result.add newEmptyNode()

#   if result["header-args"]["args"].anyIt(
#     it["name"].text == "noweb" and
#     it["value"].text == "yes"
#   ):
#     result["body"] = result["body"].text.newSublexer().withResIt do:
#       parseNowebBlock(it, parseConf)

#   elif result["header-args"]["args"].anyIt(
#     it["name"].text == "snippet" and
#     it["value"].text == "yes"
#   ):
#     result["body"] = result["body"].text.newSublexer().withResIt do:
#       parseSnippetBlock(it, parseConf)

proc parseSrcInline*(lexer, parseConf): OrgNode =
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


proc parseCodeBlock*(str: PosStr, parseConf: ParseConf): OrgNode =
  raise newImplementError()

proc parseCodeInline*(str: PosStr, parseConf: ParseConf): OrgNode =
  raise newImplementError()


proc parseCallBlock*(str: PosStr, parseConf: ParseConf): OrgNode =
  raise newImplementError()

proc parseCallLine*(str: PosStr, parseConf: ParseConf): OrgNode =
  raise newImplementError()

proc parseCallInline*(str: PosStr, parseConf: ParseConf): OrgNode =
  raise newImplementError()
