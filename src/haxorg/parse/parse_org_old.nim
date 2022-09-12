{.experimental: "caseStmtMacros".}

import ./lex_all, ../defs/org_types, ../common
import ../defs/impl_org_node
import hmisc/core/[all, code_errors]
import hmisc/algo/[htemplates, halgorithm]
import std/[strutils, sequtils, strformat, streams, algorithm]

#[

* Some implementation notes

This parser assumes there is *no* hard runtime errors - whatewher user
wrote is *exactly* what they wanted to, and we can only try to figure out
their intentions as close as possible. This means multiple reparsing, or
long lookahead attempts are not out of ordinary, and meant to incorporate
most of the common edge cases, and provide generic fallback behavior in
other cases.

]#

proc classifyMarkKind*(ch: char): OrgNodeSubKind =
  case ch:
    of '+': oskStrike
    of '*': oskBold
    of '_': oskUnderline
    of '/': oskItalic
    of '~': oskMonospaced
    of '`': oskBacktick
    of '=': oskVerbatim
    else: oskNone

proc classifyWord*(word: string): OrgNodeSubKind =
  if allIt(word, it in OWhitespace):
    oskSpace

  elif allIt(word, it in OPunctChars):
    oskPunct

  else:
    oskText


proc parseStmtList*(lex: var Lexer, parseConf: ParseConf): OrgNode
proc parseParagraph*(
    lex: var Lexer,
    parseConf: ParseConf,
    subKind: OrgNodeSubKind = oskNone
  ): OrgNode

proc parseBareIdent*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  return newOrg(orgBareIdent, lex.pop())

proc parseCommandArgs*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  assert false

proc parseIdent*(lex: var Lexer, subKind: OrgNodeSubKind = oskNone): OrgNode =
  result = newOrg(orgIdent, lex.pop())
  result.subKind = subKind

proc parseBigIdent*(
    lex: var Lexer,
    parseConf: ParseConf,
    subkind: OrgNodeSubKind = oskNone
  ): OrgNode =

  result = newOrg(orgBigIdent, lex.pop())
  result.subKind = subKind

proc parseCmdArguments*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  assert false
  when false:
    lex.skip()
    var flags: seq[OrgNode]
    while lex[] == '-':
      flags.add onkCmdFlag.newTree(onkRawText.newTree(
        lex.getSkipUntil(OWhitespace).toSlice(lex)))

      lex.skip()

    var args: seq[OrgNode]
    while lex[] == ':':
      lex.advance()
      args.add onkCmdValue.newTree(lex.parseIdent())
      lex.skip()

      var value = lex.getSkipUntil({OEndOfFile, ':', '\n'}).toSlice(lex)
      while value[^1] in OWhitespace:
        value.pop()

      args[^1].add onkRawText.newTree(value)

      # lex.skip()

    result = onkCmdArguments.newTree(
      tern(flags.len > 0, newOStmtList(flags), newEmptyNode()),
      tern(args.len > 0, newOStmtList(args), newEmptyNode())
    )

    # lex.nextLine()



proc parseCommand*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  ## Parse single-line command. Command arguments will be cut verbatim into
  ## resulting ast for user-defined processing.
  assert false
  when false:
    result = onkCommand.newTree()
    lex.skipExpected("#+")
    result.add newOrgIdent(lex.getSkipWhileTo(OIdentChars, ':').toSlice(lex))
    lex.advance()
    if normalize($result["name"].text) in ["attrlatex"]:
      result.add parseCmdArguments(lex: var Lexer, parseConf: ParseConf)

    else:
      result.add parseCommandArgs(lex: var Lexer, parseConf: ParseConf)

    if lex[] == '\n':
      lex.advance()


proc parseDrawer*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = onkDrawer.newTree()

  result.add onkIdent.newTree(
    lex.getInsideSimple(':', ':').toSlice(lex))

  lex.advance()
  var buf = lex.initEmptyStrRanges()
  while not (lex[":end:"] or lex.atEnd()):
    buf.add lex.pop()

  buf.pop()

  var propLexer = newSublex(buf.toSlice(lex))
  var proplist = newOStmtList()
  if result["name"].text =~ "properties":
    while propLexer[":"]:
      var prop = onkProperty.newTree()
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
        prop.add propLexer.parseCmdArguments(parseConf: ParseConf)
        propLexer.advance()

      else:
        propLexer.skip()
        prop.add onkRawText.newTree(
          propLexer.getSkipToEOL().toSlice(lex))

      propList.add prop

    result.add propList

  elif result["name"].text =~ "logbook":
    result.add propLexer.parseStmtList(parseConf: ParseConf)

  else:
    result.add onkRawText.newTree(buf.toSlice(lex))



  lex.nextLine()




proc parseDrawers*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  ## Parse one or mode drawers starting on current line.
  if lex.lineStartsWith(":"):
    # var drawerLexer = lex.newSublex(
    #   lex.cutIndentedBlock(
    #     lex.indentTo(":"), fromInline = false)
    # )

    result = onkStmtList.newTree()

    while lex[] == ':':
      result.add parseDrawer(lex: var Lexer, parseConf: ParseConf)



  else:
    return newEmptyNode()




proc parseOrgTable*(lex: var Lexer, parseConf; parentRes: OrgNode): OrgNode =
  result = onkTable.newTree(parentRes[^1])
  var sublex = newSublex(
    lex.getBuf(),
    lex.getBlockUntil("#+end-table")
  )

  type
    RowFormatting = enum
      rfCompact
      rfOneline
      rfStmtList

  var rows = newOStmtList()
  var rowArgs: OrgNode
  while sublex[] != OEndOfFile:
    rowArgs = sublex.parseCommand(parseConf: ParseConf)[1]
    let body = sublex.getBlockUntil("#+row")
    var cformat: RowFormatting

    block cellKind:
      var rowlex = lex.newSublex(body)
      while true:
        if rowlex[] in {'#', '\n'}:
          if rowlex["#+cell:"]:
            cformat = rfStmtList
            break cellKind

          else:
            discard rowlex.getSkipToEOL()
            rowlex.advance()

        elif rowlex[] in {' '}:
          discard rowlex.skip()
          if rowlex[] == '|':
            raise newImplementError()

          elif rowlex[] in {'#', '\n'}:
            discard rowlex.getSkipToEOL()
            rowlex.advance()

          else:
            raise newImplementError(rowlex.error("????").msg)

        elif rowlex[] == '|':
          discard rowlex.getSkipToEOL()
          if rowlex[-1] == '|':
            cformat = rfCompact
            break cellKind

          else:
            cformat = rfOneline
            break cellKind

        else:
          raiseAssert("#[ IMPLEMENT ]#")


    var resrow = onkTableRow.newTree(rowArgs)
    block parseCell:
      var
        rowlex = lex.newSublex(body)
        rowtext = newOStmtList()
        rowcells = newOStmtList()

      case cformat:
        of rfCompact:

          while rowlex[] != OEndOfFile:
            if rowlex[] == '|':
              rowlex.advance()
              var cells = rowLexer.getSkipToEOL()
              lex.advance()
              cells.pop()

              for elem in cells.toSlice(lex).split('|'):
                rowcells.add onkTableCell.newTree(
                  newEmptyNode(), newWord(
                    elem.toSlice(lex).strip().toSlice(lex)))

            else:
              let slice = rowlex.getSkipToEOL().toSlice(lex)
              if slice.len > 0:
                rowtext.add newWord(slice)
              rowlex.advance()

        of rfOneLine:
          while rowlex[] != OEndOfFile:
            if rowlex[] == '|':
              rowlex.advance()
              rowlex.skip()
              rowcells.add onkTableCell.newTree(
                newEmptyNode(),
                newWord(rowLexer.getSkipToEOL().toSlice(lex))
              )

              rowlex.advance()

            else:
              let slice = rowlex.getSkipToEOL().toSlice(lex)
              if slice.len > 0:
                rowtext.add newWord(slice)

              rowlex.advance()

        of rfStmtList:
          let pos = rowlex.getPosition()
          rowtext.add newWord(
            rowlex.getBlockUntil("#+cell:").toSlice(lex)
          )

          while rowlex[] != OEndOfFile:
            assert rowlex[0 .. 6] == "#+cell:"
            rowcells.add onkTableCell.newTree(
              rowlex.parseCommand(parseConf: ParseConf),
              newWord(
                rowlex.getBlockUntil("#+cell:").toSlice(lex)
              )
            )

      resrow.add rowtext
      resrow.add rowcells



    result.add resrow

proc parseResultBlock*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  lex.skipExpected("#+results")
  result = onkResult.newTree()
  if lex["["]:
    result.add onkRawText.newTree(
      lex.getInsideSimple('[', ']').toSlice(lex))

  else:
    result.add newEmptyNode()

  lex.nextLine()

  if lex[":results:"]:
    result.add lex.parseDrawer(parseConf: ParseConf)

  else:
    result.add onkRawText.newTree(lex.getSkipToEOL().toSlice(lex))

proc searchResult*(lex: var Lexer, parseConf: ParseConf): int =
  var ahead = lex
  result = -1
  while not ahead.atEnd():
    if ahead[] notin {'#', '\n', ' '}:
      return -1

    else:
      ahead.skip(Whitespace)

      if not ahead["#+results"]:
        ahead.nextLine()

      else:
        return ahead.d.bufpos

proc parseNowebBlock*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = onkNowebMultilineBlock.newTree()
  while not lex.atEnd():
    var nowRange: StrRanges
    while not (lex["<<"] or lex.atEnd()):
      nowRange.add lex.pop()

    result.nowebBlock.slices.add NowebSlice(
      slice: nowRange.toSlice(lex)
    )


    if not lex.atEnd():
      lex.advance()
      var body = lex.getInsideBalanced('<', '>')
      result.nowebBlock.slices.add NowebSlice(
        isPlaceholder: true,
        slice: body
      )
      lex.advance()


proc parseSnippetBlock*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = onkSnippetMultilineBlock.newTree()
  while not lex.atEnd():
    var nowRange: StrRanges
    while not ((lex["$"] and lex[+1] in {'0' .. '9', '{'}) or lex.atEnd()):
      nowRange.add lex.pop()

    result.snippetBlock.slices.add SnippetSlice(
      slice: nowRange.toSlice(lex)
    )

    if not lex.atEnd():
      lex.advance()
      if lex[] in {'0' .. '9'}:
        result.snippetBlock.slices.add SnippetSlice(
          isPlaceholder: true,
          slice: lex.initStrRanges().toSlice(lex)
        )
        lex.advance()

      else:
        var body = lex.getInsideBalanced('{', '}')
        result.snippetBlock.slices.add SnippetSlice(
          isPlaceholder: true,
          hasBody: true,
          slice: body
        )

proc parseOrgSource*(lex: var Lexer, parseConf; parentRes: OrgNode): OrgNode =
  result = onkSrcCode.newTree()

  var argsLexer = newSublex(parentRes[1].text)

  argsLexer.skip()
  result.add argsLexer.parseIdent()
  argsLexer.skip()
  result.add argsLexer.parseCmdArguments(parseConf: ParseConf)


  result.add onkVerbatimMultilineBlock.newTree(
    lex.getBlockUntil("#+end").toSlice(lex))

  lex.nextLine()

  let idx = lex.searchResult(parseConf: ParseConf)
  if idx > 0:
    var prefCmds: seq[OrgNode]
    while not lex["#+results"]:
      while lex[] in OLineBreaks + OWhitespace:
        lex.advance()

      prefCmds.add lex.parseCommand(parseConf: ParseConf)

    result.add onkAssocStmtList.newTree(
      onkStmtList.newTree(prefCmds),
      lex.parseResultBlock(parseConf: ParseConf)
    )

  else:
    result.add newEmptyNode()

  if result["header-args"]["args"].anyIt(
    it["name"].text == "noweb" and
    it["value"].text == "yes"
  ):
    result["body"] = result["body"].text.newSublex().withResIt do:
      parseNowebBlock(it, parseConf: ParseConf)

  elif result["header-args"]["args"].anyIt(
    it["name"].text == "snippet" and
    it["value"].text == "yes"
  ):
    result["body"] = result["body"].text.newSublex().withResIt do:
      parseSnippetBlock(it, parseConf: ParseConf)


proc parseMultilineCommand*(
    lex: var Lexer, parseConf;
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


  result = OrgNode(kind: onkMultilineCommand)
  lex.skipExpected("#+BEGIN")
  discard lex.getSkipWhile({'-', '_'})

  result.add lex.parseIdent()

  result.add parseCommandArgs(lex: var Lexer, parseConf: ParseConf)
  lex.nextLine()

  if result["name"].text == "table":
    result = lex.parseOrgTable(parseConf, result)

  elif result["name"].text =~ "src":
    result = lex.parseOrgSource(parseConf, result)

  else:
    result.add onkVerbatimMultilineBlock.newTree(
      lex.getBlockUntil("#+end").toSlice(lex))

  lex.nextLine()


proc optGetWhile(lex; chars: set[char], resKind: OrgNodeKind): OrgNode =
  if lex[] in chars:
    result = newTree(resKind, lex.getSkipWhile(chars).toSlice(lex))

  else:
    result = newEmptyNode()

proc parseBracket*(lex: var Lexer, parseConf; buf: var StrSlice): OrgNode

proc parseAtEntry*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  ## Parse any entry starting with `@` sign - metatags, annotations, inline
  ## backend passes.
  if lex[0..1] == "@@":
    # Inline backend pass
    discard

  elif lex[0..1] == "@[":
    # Annotation start
    discard

  elif lex[] == '@' and lex[+1] in OIdentChars:
    # Metatag start OR random `@` in the text
    lex.advance()
    let id = lex.parseIdent(oskMetaTagIdent)
    if lex[] == '[':
      result = newTree(
        onkMetaTag,
        id,
        newTree(
          onkRawText,
          oskMetaTagArgs,
          lex.getInsideSimple('[', ']').toSlice(lex))
      )

    elif lex[] == '{':
      result = newTree(onkMetaTag, id)
      result.add newEmptyNode(oskMetatagArgs)

    else:
      raise newImplementError(lex.error("22").msg)

    if id.strVal() == "import":
      var sub = lex.getInsideBalanced('{', '}').newSublex()
      var buf: StrSlice
      result.add parseBracket(sub, parseConf, buf)

    else:
      result.add onkStmtList.newTree()
      while lex[] == '{':
        result[^1].add newTree(
          onkRawText, oskMetatagText, lex.getInsideBalanced('{', '}'))

  else:
    raise lex.error("Expected @-entry")


proc parseBracket*(lex: var Lexer, parseConf; buf: var StrSlice): OrgNode =
  ## Parse any square bracket entry starting at current lex position, and
  ## return it.

  if not lex.isBalancedToEOL():
    buf.add lex.pop()
    return

  var ahead = lex
  if lex[0..1] == "[[":
    # Link start
    result = onkLink.newTree()
    lex.advance()
    result.add onkRawText.newTree(lex.getInsideBalanced('[', ']'))
    if lex[] == '[':
      result.add lex.getInsideBalanced('[', ']').newSublex().withResIt do:
        parseParagraph(it, parseConf: ParseConf)

    else:
      lex.advance(2)
      result.add newEmptyNode()

    lex.advance()

  elif lex[] == '[':
    if lex[+1 .. +3] == "fn:":
      var notelex = lex.getInsideBalanced(
        '[', ']').newSublex()

      notelex.advance(3)
      result = onkFootnote.newTree()
      if notelex[] != ':':
        result.add notelex.parseIdent()

      else:
        result.add newEmptyNode()

      notelex.advance()

      result.add notelex.parseParagraph(parseConf: ParseConf)


    else:
      const start = {'!', '>', '<', '*', '#', '?', '@'} + {'A' .. 'Z'}

      if lex[+1] in start and
         lex[+2] in start:
        let body = lex.getInsideBalanced('[', ']').split('|')
        result = onkBracTag.newTree()
        for slice in body:
          result.add onkBareIdent.newTree(
            slice.toSlice(lex).strip().toSlice(lex))

      else:
        let body = lex.getInsideBalanced('[', ']')
        result = onkTimeStamp.newTree(body)




proc parseMacro*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  lex.skipExpected("{{")
  result = onkMacro.newTree(
    onkRawText.newTree(
      lex.getInsideBalanced('{', '}')))

  lex.skipExpected("}}")


proc parseOptMacro*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  case lex.nextSet({'{'}, OBareIdentChars - {'{'}):
    of 0:
      discard lex.getSkipUntil({'{'})
      if lex["{{{"]:
        return onkResult.newTree(lex.parseMacro(parseConf: ParseConf))

      else:
        return newEmptyNode()

    of 1:
      return newEmptyNode()


proc parseSrcInline*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  assert lex["src_"]
  lex.advance(4)
  result = onkSrcCode.newTree()
  result.add lex.parseIdent()
  case lex[]:
    of '[':
      result.add onkRawText.newTree(lex.getInsideBalanced('[', ']'))
      result.add onkRawText.newTree(lex.getInsideBalanced('{', '}'))

    of '{':
      result.add newEmptyNode()
      result.add onkRawText.newTree(lex.getInsideBalanced('{', '}'))

    else:
      raiseAssert("#[ IMPLEMENT ]#")


  lex.skip()
  result.add parseOptMacro(lex: var Lexer, parseConf: ParseConf)

proc parseCallInline*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  assert lex["call_"]
  lex.advance(5)
  result = onkCallCode.newTree()
  result.add lex.parseIdent()
  case lex[]:
    of '[':
      result.add lex.getInsideBalanced('[', ']').newSublex().withResIt do:
        parseCmdArguments(it, parseConf: ParseConf)

      lex.skip()
      result.add onkRawText.newTree(
        lex.getInsideBalanced('(', ')'))

    of '(':
      result.add newEmptyNode()
      lex.skip()
      result.add onkRawText.newTree(lex.getInsideBalanced('(', ')'))

    else:
      raiseAssert("#[ IMPLEMENT ]#")

  lex.skip()
  if lex[] == '[':
    result.add lex.getInsideBalanced('[', ']').newSublex().withResIt do:
      parseCmdArguments(it, parseConf: ParseConf)

  else:
    result.add newEmptyNode()

  lex.skip()
  result.add parseOptMacro(lex: var Lexer, parseConf: ParseConf)


proc parseHashTag*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  assert lex[] == '#'
  lex.advance()

  proc aux(lex: var Lexer, parseConf: ParseConf): OrgNode =
    result = onkHashTag.newTree()
    # `#tag`
    result.add lex.parseIdent()

    # `#tag##[sub1, sub2]`
    if lex[0 .. 2] == "##[":
      lex.advance(3)

      while lex[] != ']':
        # TODO on broken tags this would cause compilation errors and/or
        # whole text getting dragged into single tag body.
        result.add aux(lex: var Lexer, parseConf: ParseConf)
        lex.skip()
        if lex[] != ']':
          lex.skipExpected(",")
          lex.skip()

      lex.advance()

    # `#tag##sub`
    elif lex[0 .. 1] == "##":
      lex.advance(2)
      result.add aux(lex: var Lexer, parseConf: ParseConf)



  return aux(lex: var Lexer, parseConf: ParseConf)

proc parseInlineMath*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  ## Parse inline math expression, starting with any of `$`, `$$`, `\(`,
  ## and `\[`.

  assert lex[] == '$'
  lex.advance()
  result = onkMath.newTree(lex.getSkipUntil({'$'}).toSlice(lex))
  lex.advance()

proc splitTextbuf*(
  lex; buf: var StrSlice, dropEmpty: bool = true): seq[OrgNode] =

  func canAdd(slice: StrSlice): bool =
    not (dropEmpty and slice.allIt(it in OWhitespace))

  var text = lex.initEmptyStrRanges().toSlice(lex)
  var bigIdent = lex.initEmptyStrRanges().toSlice(lex)
  for i in indices(buf):
    let changeRegion =
      # Started whitespace region, flushing buffer
      (
        absAt(buf, i) in Whitespace and
        text.len > 0 and
        text.lastChar() notin Whitespace
      ) or
      # Finished whitespace region
      (
        absAt(buf, i) notin Whitespace and
        text.len > 0 and
        text.lastChar() in Whitespace
      )

    if changeRegion or (i == high(buf)):
      # Finished input buffer or found region change
      if i == high(buf) and not changeRegion:
        # Found last character.
        text.add i

      if (text[0] in OBigIdentChars or bigIdent.len > 0) and
        text.allOfIt(it in OBigIdentChars + OWhitespace):

        for idx in indices(text):
          bigIdent.add idx

        if i == high(buf):
          result.add onkBigIdent.newTree(bigIdent)

      else:
        if bigIdent.len > 0:
          var resIdx = toSeq(indices(bigIdent))
          var trailIdx: seq[int]

          for idx in rindices(bigIdent):
            if bigIdent.absAt(idx) in OWhitespace:
              trailIdx.add resIdx.pop

            else:
              break

          var res: StrRanges
          for idx in resIdx:
            res.add idx

          var trail: StrRanges
          for idx in reversed(trailIdx):
            trail.add idx

          if canAdd(res.toSlice(lex)):
            result.add onkBigIdent.newTree(oskBigWord, res.toSlice(lex))

          block:
            let trail = trail.toSlice(lex)
            if trailIdx.len > 0 and canAdd(trail):
              result.add newTree(onkWord, classifyWord($trail), trail)


        bigIdent.ranges = @[]

        # if dropEmpty and text.allIt(it in OWhitespace):
        #   discard

        if canAdd(text):
          result.add newTree(onkWord, classifyWord($text), text)

      text = initStrRanges(i, i).toSlice(lex)

      if i == high(buf) and changeRegion and canAdd(text):
        result.add newTree(onkWord, classifyWord($text), text)

    else:
      text.add i

proc getLastLevel(node: var OrgNode, level: int): var OrgNode =
  case level:
    of 0: return node
    of 1: return node[^1]
    of 2: return node[^1][^1]
    of 3: return node[^1][^1][^1]
    else: return getLastLevel(node, level - 4)

proc getLastLevel(node: OrgNode, level: int): OrgNode =
  case level:
    of 0: node
    of 1: node[^1]
    else: getLastLevel(node, level - 2)

proc parseSlashEntry*(lex: var Lexer, parseConf; buf: var StrSlice): OrgNode =
  assert lex[] == '\\'
  if lex[+1] in OIdentStartChars:
    var ahead = lex
    ahead.advance()
    if ahead.allUntil(OIdentChars, OWhitespace + {'{', OEndOfFile}):
      lex.advance()
      result = onkSymbol.newTree(
        lex.getSkipWhile(OIdentChars).toSlice(lex))

      if lex["{}"]:
        lex.advance(2)

    else:
      buf.add lex.pop()
      buf.add lex.pop()

  else:
    buf.add lex.pop()
    buf.add lex.pop()

    # lex.parseIdent()

proc parseAngleEntry*(lex: var Lexer, parseConf; buf: var StrSlice): OrgNode =
  if not lex.isBalancedToEOL():
    buf.add lex.pop()

  else:
    var lex = lex.getInsideBalanced('<', '>').newSublex()
    if lex[0] == '+' and lex[^1] == '+':
      lex.advance()
      var buf = lex.initEmptyStrRanges()
      while not lex.atEnd():
        buf.add lex.pop()

      buf.pop()

      var textlex = newSublex(buf.toSlice(lex))
      result = onkPlaceholder.newTree(
        textlex.parseParagraph(parseConf: ParseConf))

    elif lex[0] == '<' and lex[^1] == '>':
      lex.advance()
      var buf = lex.initStrRanges()
      while not lex.atEnd():
        buf.add lex.pop()

      buf.pop()

      result = onkRadioTarget.newTree(
        onkRawText.newTree(buf.toSlice(lex)))

    else:
      result = onkPlaceholder.newTree(lex.parseParagraph(parseConf: ParseConf))






proc parseText*(lex: var Lexer, parseConf: ParseConf): seq[OrgNode] =
  # Text parsing is implemented using non-recusive descent parser that
  # maintains stack explicitly (instead of constructing it via function
  # calls). This is made in order to provide support for stack
  # introspection at any given moment of parsing, and perform context-aware
  # decisions. Input lex is parsed *until the end* - e.g you need to
  # always pass sublex.

  # TODO implement support for additional formatting options, delimited
  # pairs, and punctuation. `<placeholder>`, `(structured-punctuation)`.

  # TODO parse big idents - note that things like `MUST NOT`, `SHALL NOT`
  # need to be parsed as single node.
  var stack: seq[seq[tuple[pending: bool,
                           node: OrgNode]]]


  template getLayerOpen(ch: string): int =
    var layerOpen = -1
    for idx, layer in pairs(stack):
      if layer.len > 0 and
         layer[^1].pending and
         layer[^1].node.getStr() == ch
        :
        layerOpen = idx + 1

    layerOpen


  template closeAllWith(inLayerOpen: int, ch: string): untyped =
    # Force close all layers of parse stack, by moving nodes from several
    # layers into subnodes. This is used for explicitly handling closing
    # delimtiers.
    let layerOpen: int = inLayerOpen
    let foldTimes: int = stack.len - layerOpen
    var nodes: seq[OrgNode]
    for _ in 0 ..< foldTimes:
      nodes.add reversed(stack.pop.mapIt(it.node))

    for node in reversed(nodes):
      if node.kind == onkMarkup and node.len == 0:
        # TODO convert markup node not `Word` and set correc positional
        # information.
        stack[^1][^1].node.add node

      else:
        stack[^1][^1].node.add node

    stack[^1][^1].pending = false


  template closeWith(ch: string): untyped =
    # Close last pending node in stack is there is any, otherwise move
    # current layer not lower one. Used for handling closing buffer nodes
    # /or/ delimiters. Cannot fold multiple layers of stack - only change
    # `pending` tag if needed.
    let layer = stack.pop
    if (stack.len > 0 and stack.last.len > 0 and stack.last2.pending):
      stack.last2.pending = false
      stack.last2.node.add layer.mapIt(it.node)

    elif stack.len == 0:
      stack.add @[layer]

    else:
      stack.last.add layer

  template pushWith(newPending: bool, node: OrgNode): untyped =
    # Add new node to parse stack. If last-last one (last layer, last node
    # in layer) is pending opening, add new layer, otherwise push to the
    # same layer. All pending nodes will be closed in `closeWith`.
    if (stack.last.len > 0 and stack.last2.pending):
      stack.add @[@[(newPending, node)]]

    else:
      stack.last.add (newPending, node)

  var inVerbatim = false
  # FIXME account for different kinds of verbatim formatting - current
  # implementation will trigger no-verbatim mode for closing `~` after
  # opening `=`

  template pushBuf(): untyped =
    # If buffer is non-empty push it as new word. Most of the logic in this
    # template is for dealing with whitespaces in buffers and separating
    # them into smaller things. For example `"buffer with space"` should be
    # handled as five different `Word`, instead of a single one.

    # Buffer is pushed before parsing each inline entry such as `$math$`,
    # `#tags` etc.
    if len(buf) > 0 and inVerbatim:
      pushWith(false, onkRawText.newTree(buf))
      buf = lex.initEmptyStrRanges().toSlice(lex)

    elif len(buf) > 0:
      for node in lex.splitTextBuf(buf, parseConf.dropEmptyWords):
        pushWith(false, node)


      buf = lex.initEmptyStrRanges().toSlice(lex)


  stack.add @[]

  var buf = lex.initEmptyStrRanges().toSlice(lex)

  while lex[] != OEndOfFile:
    # More sophisticated heuristics should be used to detect edge cases
    # like `~/me~`, `*sentence*.` and others. Since particular details are
    # not fully fleshed out I will leave it as it is now, and concentrate
    # on other parts of the document.

    const markChars = OMarkupChars + {'\'', '"'} +
      OPunctOpenChars + OPunctCloseChars - {'[', ']', '<', '>'}

    case lex[]:
      of markChars:
        var ch: string
        var hadPop = false
        if not inVerbatim and lex.isOpenAt(
          ch, markChars + OPunctOpenChars):
          # Start of the regular, constrained markup section.
          # Unconditinally push new layer.
          pushBuf()
          pushWith(true, newTree(onkMarkup, classifyMarkKind(ch[0]), $ch))

          if ch[0] in OVerbatimChars:
            inVerbatim = true

        elif lex.isCloseAt(ch, markChars + OPunctCloseChars):
          # End of regular constrained section, unconditionally close
          # current layer, possibly with warnings for things like
          # `*/not-fully-italic*`
          if not inVerbatim or (inVerbatim and ch[0] in OVerbatimChars):
            pushBuf()
            let layer = getLayerOpen($ch[0].matchingPair())
            if layer != -1:
              closeAllWith(layer, $ch[0].matchingPair())
              inVerbatim = false

            else:
              buf.add lex.pop()

          else:
            hadPop = true
            buf.add lex.pop()

        elif lex.isToggleAt(ch):
          # Detected unconstrained formatting block, will handle it
          # regardless.
          let layerOpen = getLayerOpen(ch)
          let isOpening = layerOpen == -1


          if ch[0] in OVerbatimChars:
            # Has matching unconstrained section open at one of the previous layers
            pushBuf()
            if isOpening:
              # Open new verbatim section
              inVerbatim = true
              pushWith(true, newTree(onkMarkup, classifyMarkKind(ch[0]), ch))

            else:
              inVerbatim = false
              closeAllWith(layerOpen, ch)

            lex.advance()

          elif inVerbatim:
            hadPop = true
            buf.add lex.pop()


          else:
            if isOpening:
              # Push new markup opening, no verbatim currently active
              pushWith(true, newTree(onkMarkup, classifyMarkKind(ch[0]), ch))
              lex.advance()

            else:
              # Push new markup opening, no verbatim currently active
              closeAllWith(layerOpen, ch)
              lex.advance()

        else:
          hadPop = true
          buf.add lex.pop()

        if not hadPop:
          lex.advance()


      of '$':
        if lex[-1] in OEmptyChars:
          pushBuf()
          pushWith(false, parseInlineMath(lex: var Lexer, parseConf: ParseConf))

        else:
          raiseAssert("#[ IMPLEMENT ]#")

      of '@':
        if lex[-1] in OEmptyChars:
          pushBuf()
          pushWith(false, parseAtEntry(lex: var Lexer, parseConf: ParseConf))

        else:
          buf.add lex.pop

      of '#':
        if lex[-1] in OEmptyChars and
           lex[+1] == '[' and
           not inVerbatim:

          pushBuf()

          lex.advance()
          pushWith(false, onkComment.newTree(
            lex.getInsideBalanced('[', ']'),
          ))

          lex.skipExpected("#")

        elif lex[-1] in OEmptyChars and
             lex[+1] in OWordChars and
             not inVerbatim:

          pushBuf()
          pushWith(false, parseHashTag(lex: var Lexer, parseConf: ParseConf))

        else:
          buf.add lex.pop

      of '[':
        if lex[-1] in OEmptyChars and
           not inVerbatim:
          pushBuf()
          let node = parseBracket(lex: var Lexer, parseConf, buf)
          if not node.isNil:
            pushWith(false, node)

        else:
          buf.add lex.pop

      of '\\':
        let node = lex.parseSlashEntry(parseConf, buf)
        if not node.isNil:
          pushWith(false, node)

      of '<':
        if inVerbatim:
          buf.add lex.pop()

        else:
          let node = lex.parseAngleEntry(parseConf, buf)
          if not node.isNil:
            pushWith(false, node)

      elif lex["src_"]:
        pushWith(false, lex.parseSrcInline(parseConf: ParseConf))

      elif lex["call_"]:
        pushWith(false, lex.parseCallInline(parseConf: ParseConf))

      else:
        buf.add lex.pop

  pushBuf()
  while stack.len > 1:
    closeWith("")

  return stack[0].mapIt(it.node)



proc parseParagraph*(
    lex: var Lexer, parseConf; subKind: OrgNodeSubKind = oskNone): OrgNode =
  result = onkParagraph.newTree(lex.parseText(parseConf: ParseConf))
  result.subKind = subKind


proc parseOrgCookie*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  if lex[] == '[':
    result = onkUrgencyStatus.newTree(
      getInsideSimple(lex: var Lexer, '[', ']').toSlice(lex))

  else:
    result = newEmptyNode()



proc parseList*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  # This is a horrible nest of vaguely justified checks, paired with
  # multiple assumptions on input validty, but general outline of
  # implementation is as follows:
  #
  # - in loop,determine particular type of current list start, get
  #   bullet chars.
  # - Create sublex for whole item
  # - Partiallt parse item - counter, checkbox
  # - Found extent of list 'header', positions of completion cookies if any
  # - Create separate header sublex: var Lexer, determine ranges for tag (for property list),
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
  result = onkList.newTree()


  while lex.listStartChar() != OEndOfFile:
    let start: char = lex.listStartChar()
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
    while lex[] in skipset + endset:
      bullet.add lex.pop()

    lex.skip()
    var itemLexer = lex.indentedSublex(
      2,
      keepNewlines = true,
      fromInline = true,
      requireContinuation = false
    )


    let bulletSlice = $bullet.toSlice(lex)
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



    var item = onkListItem.newTree(
      bulletClass,
      onkRawText.newTree(bullet.toSlice(lex)))

    itemLexer.skip()
    # Parse counter-set and checkbox
    if itemLexer[] == '[':
      if itemLexer[+1] == '@':
        item.add onkCounter.newTree(
          itemLexer.getInsideSimple('[', ']').toSlice(lex))

        itemLexer.skip()
        if itemLexer[] == '[':
          item.add onkCheckbox.newTree(
            itemLexer.getInsideSimple('[', ']').toSlice(lex))

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

    # Create sublex for header ranges and get subranges for tags and
    # completion cookies.
    var it = headerRanges.toSlice(lex).newSublex()
    let tagRanges = it.allRangesTo(
      "::",
      repeatIncluding = true,
      remaining = true # Get all indices in ranges
    )

    let cookieRanges = it.allRangesTo("[", remaining = true)

    var isValidCookie = true
    if cookieRanges.len > 1:
      # If completion cookie has multiple ranges and
      let sl = cookieRanges[^1].toSlice(lex)
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
      item.add tagRanges[0].toSlice(lex).newSublex().withResIt do:
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
        item.add paragraph.toSlice(lex).newSublex().withResIt do:
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

        item.add overlap.toSlice(lex).newSublex().withResIt do:
          it.parseParagraph(parseConf: ParseConf)

      else:
        item.add tagRanges[0].toSlice(lex).newSublex().withResIt do:
          it.parseParagraph(parseConf: ParseConf)


    if isValidCookie:
      item.add overlapping(
        @[cookieRanges[^2]], cookieRanges[^1]
      ).toSlice(lex).newSublex.withResIt do:
        onkCompletion.newTree(it.getInsideSimple('[', ']').toSlice(lex))

    else:
      item.add newEmptyNode()

    item.add itemLexer.parseStmtList(parseConf: ParseConf)
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
    if item["tag"].kind != onkEmptyNode:
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


proc parseSubtree*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  ## Parse header node.
  ## - NOTE :: Only subtree header is parsed - @ret{["body"]} is
  ##   set to empty node and should be handled externally.
  # NOTE `@ret{["body"]}` should be rendered as `result["body"]`

  result = OrgNode(kind: onkSubtree)

  result.add onkBareIdent.newTree(lex.getSkipWhile({'*'}).toSlice(lex))
  lex.skip()

  if lex.atBigIdent():
    result.add lex.parseBigIdent(parseConf, oskTodoIdent)

  else:
    result.add newEmptyNode(oskTodoIdent)

  lex.skip()

  result.add parseOrgCookie(lex: var Lexer, parseConf: ParseConf)

  lex.skip()

  var headerLexer = lex.indentedSublex(
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
      if lex.absAt(allIdx[pos]) == ':':
        var tagEnded = false
        while not tagEnded:
          dec pos
          while lex.absAt(allIdx[pos]) notin OWhitespace:
            tagsElems.add allIdx[pos]

            if lex.absAt(allIdx[pos]) == ':':
              dec pos

            else:
              break

          if lex.absAt(allIdx[pos]) in OWhitespace:
            tagEnded = true


      tagsFound = true

    elif not completionFound:
      if lex.absAt(allIdx[pos]) in OWhitespace:
        dec pos

      elif lex.absAt(allIdx[pos]) == ']':
        var subbuf: seq[int]
        dec pos
        while lex.absAt(allIdx[pos]) in {'0' .. '9', '/', '%'}:
          subbuf.add allIdx[pos]
          dec pos

        if lex.absAt(allIdx[pos]) == '[':
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

    result.add headerBuf.toSlice(lex).newSublex().withResIt do:
      parseParagraph(it, parseConf: ParseConf)

  block:
    if completionElems.len > 0:
      var completionSlice: StrRanges
      for idx in reversed(completionElems):
        completionSlice.add idx

      result.add onkRawText.newTree(completionSlice.toSlice(lex))

    else:
      result.add newEmptyNode()


  block:
    if tagsElems.len > 0:
      var tagsBuf = @[lex.initEmptyStrRanges()]
      for idx in reversed(tagsElems):
        if lex.absAt(idx) == ':':
          tagsBuf.add lex.initEmptyStrRanges()

        else:
          tagsBuf[^1].add idx

      result.add onkOrgTag.newTree()
      for buf in tagsBuf:
        if buf.len > 0:
          result[^1].add onkRawText.newTree(oskOrgTagIdent, buf.toSlice(lex))

    else:
      result.add newEmptyNode()


  lex.advance()

  var timesLexer = lex
  timesLexer.skip()
  if timesLexer.atBigIdent():
    var times = newOStmtList()
    while timesLexer.atBigIdent():
      times.add onkSubtreeTimes.newTree()
      times[^1].add timesLexer.parseBigIdent(parseConf: ParseConf)
      timesLexer.skipExpected(":")
      timesLexer.skip()
      if timesLexer["<"]:
        times[^1].add onkTimeStamp.newTree(
          timesLexer.getInsideBalanced('<', '>'))

      elif timesLexer["["]:
        times[^1].add onkTimeStamp.newTree(
          timesLexer.getInsideBalanced('[', ']'))

      timesLexer.skip()

    result.add times

    lex.d.bufpos = timesLexer.d.bufpos

  else:
    result.add newEmptyNode()

  lex.gotoSOL()



  if lex.lineStartsWith(":"):
    var drawerLexer = lex.indentedSublex(
      lex.getIndent(),
      keepNewlines = true,
      requireContinuation = false,
      fromInline = false
    )

    result.add parseDrawers(drawerLexer: var Lexer, parseConf: ParseConf)

  else:
    result.add newEmptyNode()

  result.add newEmptyNode()

type
  OrgStart* = enum
    otkNone

    otkCommand
    otkBeginCommand
    otkIdent
    otkSubtreeStart
    otkListStart
    otkParagraph
    otkLineComment
    otkDrawer
    otkEof
    otkList


proc detectStart(lex): OrgStart =
  let ch = lex[]
  case ch:
    elif ch == '#':
      if lex["#+begin"]:
        result = otkBeginCommand

      elif lex["#+"]:
        result = otkCommand

      elif lex["#["]:
        # Inline comment start
        result = otkParagraph

      elif lex["# "]:
        # Comment until end of line
        result = otkLineComment

      else:
        # Text startsing with tag
        result = otkParagraph

    elif ch == '*':
      if lex.column == 0:
        var idx = 0
        while lex[idx] in {'*'}:
          inc idx

        if lex[idx] in {' '}:
          # `***** Heading`
          result = otkSubtreeStart

        else:
          # `*First bold*`
          result = otkParagraph

      else:
        result = otkListStart

    elif ch in OListChars:
      if lex.listStartChar() == OEndOfFile:
        result = otkParagraph

      else:
        result = otkList

    elif ch in OWordChars + {'['}:
      result = otkParagraph

    elif ch == '\n':
      lex.skip({'\n'})
      return detectStart(lex)

    elif ch == OEndOfFile:
      result = otkEOF

    elif ch == ':':
      var idx = 0
      while lex[idx] in OIdentChars:
        inc idx

      if lex[idx] == ':':
        result = otkDrawer

      else:
        result = otkParagraph

    elif lex[] in OMarkupChars + {'\\'}:
      result = otkParagraph

    else:
      result = otkParagraph



proc parseStmtList*(lex: var Lexer, parseConf: ParseConf): OrgNode =
  result = OrgNode(kind: onkStmtList)
  var assocListBuf: seq[OrgNode]

  var headerLevel = 0

  var treeStack: seq[seq[OrgNode]]

  template pushTree(tree) =
    if treeStack.len == 0:
      result.add tree

    else:
      if treeStack[^1][^1]["body"].kind == onkEmptyNode:
        treeStack[^1][^1]["body"] = newOStmtList()

      treeStack[^1][^1]["body"].add tree

  template pushAssoc =
    case assocListBuf.len:
      of 0: discard
      of 1:
        pushTree assocListBuf.pop()

      else:
        pushTree onkAssocStmtList.newTree(assocListBuf)
        assocListBuf = @[]

  while lex[] != OEndOfFile:
    let kind = lex.detectStart()
    case kind:
      of otkBeginCommand:
        let cmd = parseMultilineCommand(lex: var Lexer, parseConf: ParseConf)
        if assocListBuf.len > 0:
          pushTree onkAssocStmtList.newTree(
            onkStmtList.newTree(assocListBuf),
            cmd
          )

          assocListBuf = @[]

        else:
          pushTree cmd

      of otkCommand:
        assocListBuf.add parseCommand(lex: var Lexer, parseConf: ParseConf)

      of otkSubtreeStart:
        let tree = parseSubtree(lex: var Lexer, parseConf: ParseConf)
        let newLevel = tree["prefix"].charlen
        if newLevel > headerLevel:
          headerLevel = newLevel
          treeStack.add @[tree]

        elif newLevel == headerLevel:
          treeStack[^1].add tree

        else:
          let level: seq[OrgNode] = treeStack.pop
          treeStack[^1][^1]["body"].add level
          treeStack.add @[tree]


      of otkParagraph:
        var paragraphLexer = lex.indentedSublex(
          0,
          keepNewlines = false,
          requireContinuation = false,
          fromInline = false,
          atEnd = (
            proc(lex: var Lexer): bool =
              # Correct for adjacent regular text block with list right after
              result = lex[0..1] in ["- ", "+ ", "* "]
          )

        )

        pushTree newTree(
          onkParagraph, oskStandaloneText, paragraphLexer.parseText(parseConf: ParseConf))

      of otkEOF:
        break

      of otkDrawer:
        pushTree parseDrawer(lex: var Lexer, parseConf: ParseConf)

      of otkList:
        pushTree parseList(lex: var Lexer, parseConf: ParseConf)

      else:
        raiseAssert(&"#[ IMPLEMENT for kind {kind} {instantiationInfo()} ]#")

    while lex[] in Newlines:
      if lex[] in {'\n'}:
        pushAssoc()

      lex.advance()

  pushAssoc()

  while treeStack.len > 1:
    let level = treeStack.pop()
    treeStack[^1][^1]["body"].add level

  if treeStack.len > 0:
    result.add treeStack.pop()


const defaultParseConf*: ParseConf = ParseConf(
  dropEmptyWords: true
)

proc parseOrg*(str: string, parseConf: ParseConf = defaultParseConf: ParseConf): OrgNode =
  startHax()
  var lex = newLexer(newStrBufSlice(str))

  try:
    result = parseStmtList(lex: var Lexer, parseConf: ParseConf)

  except CodeError as err:
    pprintErr()
