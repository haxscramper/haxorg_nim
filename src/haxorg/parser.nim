{.experimental: "caseStmtMacros".}

import lexer, ast, common, buf
import hmisc/helpers
import std/[strutils, sequtils, strformat, streams]

import fusion/matching


using lexer: var Lexer


proc parseStmtList(lexer): OrgNode

proc parseBareIdent*(lexer): OrgNode =
  lexer.skip()
  result = newBareIdent(getSkipWhile(lexer, OBareIdentChars).toSlice(lexer))

proc parseCommandArgs*(lexer): OrgNode =
  result = onkRawText.newTree(lexer.getSkipToEOL(true).toSlice(lexer))

proc parseIdent*(lexer): OrgNode =
  var buf = initStrRanges(lexer)
  lexer.expect(OIdentStartChars)
  buf.add lexer.pop
  while lexer[] in OIdentChars:
    buf.add lexer.pop


  return onkIdent.newTree(buf.toSlice(lexer))

proc parseCommand*(lexer): OrgNode =
  ## Parse single-line command. Command arguments will be cut verbatim into
  ## resulting ast for user-defined processing.
  result = onkCommand.newTree()
  lexer.skipExpected("#+")
  result.add newOrgIdent(lexer.getSkipWhileTo(OIdentChars, ':').toSlice(lexer))
  lexer.advance()
  result.add parseCommandArgs(lexer)
  lexer.nextLine()


proc parseCmdArguments*(lexer): OrgNode =
  var flags: seq[OrgNode]
  while lexer[] == '-':
    flags.add onkCmdFlag.newTree(onkRawText.newTree(
      lexer.getSkipUntil(OWhitespace).toSlice(lexer)))

    lexer.skip()

  var args: seq[OrgNode]
  while lexer[] == ':':
    lexer.advance()
    args.add onkCmdValue.newTree(lexer.parseIdent())
    lexer.skip()

    args[^1].add onkRawText.newTree(
      lexer.getSkipUntil({OEndOfFile, ':'}).toSlice(lexer))

    lexer.skip()

  result = onkCmdArguments.newTree(
    newOStmtList(flags),
    newOStmtList(args)
  )

proc parseDrawer*(lexer): OrgNode =
  result = onkDrawer.newTree()

  result.add onkIdent.newTree(
    lexer.getInsideSimple(':', ':').toSlice(lexer))

  while true:
    if lexer[0 .. 4] == ":end:":
      discard lexer.getSkipToEOL()
      lexer.advance()
      return

    elif lexer[] == ':':
      result.add onkProperty.newTree(
        onkIdent.newTree(lexer.getInsideSimple(':', ':').toSlice(lexer)),
        onkRawText.newTree(lexer.getSkipToEOL().toSlice(lexer)))

      lexer.advance()

    else:
      var buf = lexer.initStrRanges().toSlice(lexer)
      while true:
        if lexer[] in {':', OEndOfFile} and lexer[-1] in {'\n'}:
          discard lexer.getSkipToEOL()
          lexer.advance()
          buf.pop()
          # NOTE I can launch sublexer on this part if needed. I know
          # format for at least some default drawers, but generally
          # speaking it is a free-form input, so it can contain completely
          # unparseable data.
          result.add onkRawText.newTree(buf)
          return

        else:
          buf.add lexer.pop()




proc parseDrawers*(lexer): OrgNode =
  ## Parse one or mode drawers starting on current line.
  echov lexer.lineStartsWith(":")
  if lexer.lineStartsWith(":"):
    var drawerLexer = lexer.newSublexer(
      lexer.cutIndentedBlock(
        lexer.indentTo(":"), fromInline = false)
    )

    result = onkStmtList.newTree()

    while drawerLexer[] == ':':
      result.add parseDrawer(drawerLexer)


  else:
    return newEmptyNode()




proc parseOrgTable*(lexer; parentRes: OrgNode): OrgNode =
  result = onkTable.newTree(parentRes[^1])
  # echov lexer @? -5 .. 5
  # echov lexer.d.buf.ranges
  var sublexer = newSublexer(
    lexer.getBuf(),
    lexer.getBlockUntil("#+end-table")
  )

  type
    RowFormatting = enum
      rfCompact
      rfOneline
      rfStmtList

  var rows = newOStmtList()
  var rowArgs: OrgNode
  while sublexer[] != OEndOfFile:
    rowArgs = sublexer.parseCommand()[1]
    let body = sublexer.getBlockUntil("#+row")
    var cformat: RowFormatting

    block cellKind:
      var rowlexer = lexer.newSublexer(body)
      while true:
        if rowlexer[] in {'#', '\n'}:
          if rowlexer["#+cell:"]:
            cformat = rfStmtList
            break cellKind

          else:
            discard rowlexer.getSkipToEOL()
            rowlexer.advance()

        elif rowlexer[] in {' '}:
          discard rowlexer.skip()
          if rowlexer[] == '|':
            echo lexer.error("Fuck '   |'").msg

          elif rowlexer[] in {'#', '\n'}:
            discard rowlexer.getSkipToEOL()
            rowlexer.advance()

          else:
            echo rowlexer.error("????").msg

        elif rowlexer[] == '|':
          discard rowlexer.getSkipToEOL()
          if rowlexer[-1] == '|':
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
        rowlexer = lexer.newSublexer(body)
        rowtext = newOStmtList()
        rowcells = newOStmtList()

      case cformat:
        of rfCompact:

          while rowlexer[] != OEndOfFile:
            if rowlexer[] == '|':
              rowlexer.advance()
              var cells = rowLexer.getSkipToEOL()
              lexer.advance()
              cells.pop()

              for elem in cells.toSlice(lexer).split('|'):
                rowcells.add onkTableCell.newTree(
                  newEmptyNode(), newWord(
                    elem.toSlice(lexer).strip().toSlice(lexer)))

            else:
              let slice = rowlexer.getSkipToEOL().toSlice(lexer)
              if slice.len > 0:
                # echov toSeq(items(slice))
                # echov slice
                # echov slice.ranges
                rowtext.add newWord(slice)
              rowlexer.advance()

        of rfOneLine:
          while rowlexer[] != OEndOfFile:
            if rowlexer[] == '|':
              rowlexer.advance()
              rowlexer.skip()
              rowcells.add onkTableCell.newTree(
                newEmptyNode(),
                newWord(rowLexer.getSkipToEOL().toSlice(lexer))
              )

              rowlexer.advance()

            else:
              let slice = rowlexer.getSkipToEOL().toSlice(lexer)
              if slice.len > 0:
                rowtext.add newWord(slice)

              rowlexer.advance()

        of rfStmtList:
          let pos = rowlexer.getPosition()
          rowtext.add newWord(
            rowlexer.getBlockUntil("#+cell:").toSlice(lexer)
          )

          while rowlexer[] != OEndOfFile:
            assert rowlexer[0 .. 6] == "#+cell:"
            rowcells.add onkTableCell.newTree(
              rowlexer.parseCommand(),
              newWord(
                rowlexer.getBlockUntil("#+cell:").toSlice(lexer)
              )
            )

      resrow.add rowtext
      resrow.add rowcells



    result.add resrow

proc parseResultBlock*(lexer): OrgNode =
  result = onkResult.newTree(
    lexer.parseCommand()
  )

  if lexer[":results:"]:
    result.add lexer.parseDrawer()

  else:
    raiseAssert("#[ IMPLEMENT ]#")

proc searchResult*(lexer): int =
  while lexer[result] != OEndOfFile:
    if lexer[result] notin {'#', '\n', ' '}:
      return -1

    else:
      while lexer[result] in OLineBreaks + OWhitespace:
        inc result

      if lexer[result .. result + "#+results:".high] != "#+results:":
        inc result, "#+results:".len
        while lexer[result] notin OLineBreaks:
          inc result


      else:
        return

proc parseNowebBlock*(lexer): OrgNode =
  result = onkNowebMultilineBlock.newTree()
  while not lexer.atEnd():
    var nowRange: StrRanges
    while not (lexer["<<"] or lexer.atEnd()):
      nowRange.add lexer.pop()

    result.nowebBlock.slices.add NowebSlice(
      slice: nowRange.toSlice(lexer)
    )


    if not lexer.atEnd():
      lexer.advance()
      var body = lexer.getInsideBalanced('<', '>')
      result.nowebBlock.slices.add NowebSlice(
        isPlaceholder: true,
        slice: body
      )
      lexer.advance()


proc parseSnippetBlock*(lexer): OrgNode =
  result = onkSnippetMultilineBlock.newTree()
  while not lexer.atEnd():
    var nowRange: StrRanges
    while not ((lexer["$"] and lexer[+1] in {'0' .. '9', '{'}) or lexer.atEnd()):
      nowRange.add lexer.pop()

    result.snippetBlock.slices.add SnippetSlice(
      slice: nowRange.toSlice(lexer)
    )

    if not lexer.atEnd():
      lexer.advance()
      if lexer[] in {'0' .. '9'}:
        result.snippetBlock.slices.add SnippetSlice(
          isPlaceholder: true,
          slice: lexer.initStrRanges().toSlice(lexer)
        )
        lexer.advance()

      else:
        var body = lexer.getInsideBalanced('{', '}')
        result.snippetBlock.slices.add SnippetSlice(
          isPlaceholder: true,
          hasBody: true,
          slice: body
        )

proc parseOrgSource*(lexer; parentRes: OrgNode): OrgNode =
  result = onkSrcCode.newTree()

  var argsLexer = newSublexer(parentRes[1].text)

  argsLexer.skip()
  result.add argsLexer.parseIdent()
  argsLexer.skip()
  result.add argsLexer.parseCmdArguments()


  result.add onkVerbatimMultilineBlock.newTree(
    lexer.getBlockUntil("#+end").toSlice(lexer))

  lexer.nextLine()

  let idx = lexer.searchResult()
  if idx > 0:
    var prefCmds: seq[OrgNode]
    while not lexer["#+results:"]:
      while lexer[] in OLineBreaks + OWhitespace:
        lexer.advance()

      prefCmds.add lexer.parseCommand()

    result.add onkAssocStmtList.newTree(
      onkStmtList.newTree(prefCmds),
      lexer.parseResultBlock()
    )

  else:
    result.add newEmptyNode()

  if result["header-args"]["args"].anyIt(
    it["name"].text == "noweb" and
    it["value"].text == "yes"
  ):
    result["body"] = result["body"].text.newSublexer().withResIt do:
      parseNowebBlock(it)

  elif result["header-args"]["args"].anyIt(
    it["name"].text == "snippet" and
    it["value"].text == "yes"
  ):
    result["body"] = result["body"].text.newSublexer().withResIt do:
      parseSnippetBlock(it)


proc parseMultilineCommand*(
    lexer;
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

  echov "Multiline command"

  result = OrgNode(kind: onkMultilineCommand)
  lexer.skipExpected("#+begin")
  discard lexer.lexScanp(*{'-', '_'})

  result.add lexer.parseIdent()

  result.add parseCommandArgs(lexer)
  lexer.nextLine()

  if result["name"].text == "table":
    result = lexer.parseOrgTable(result)

  elif result["name"].text == "src":
    result = lexer.parseOrgSource(result)

  else:
    result.add onkVerbatimMultilineBlock.newTree(
      lexer.getBlockUntil("#+end").toSlice(lexer))

  lexer.nextLine()


proc optGetWhile(lexer; chars: set[char], resKind: OrgNodeKind): OrgNode =
  if lexer[] in chars:
    result = newTree(resKind, lexer.getSkipWhile(chars).toSlice(lexer))

  else:
    result = newEmptyNode()


proc parseAtEntry*(lexer): OrgNode =
  ## Parse any entry starting with `@` sign - metatags, annotations, inline
  ## backend passes.
  if lexer[0..1] == "@@":
    # Inline backend pass
    discard

  elif lexer[0..1] == "@[":
    # Annotation start
    discard

  elif lexer[] == '@' and lexer[+1] in OIdentChars:
    # Metatag start OR random `@` in the text
    lexer.advance()
    let id = lexer.parseIdent()
    if lexer[] == '[':
      result = onkMetaTag.newTree(
        id,
        onkRawText.newTree(lexer.getInsideSimple('[', ']').toSlice(lexer)))

    elif lexer[] == '{':
      result = onkMetaTag.newTree(id)
      result.add onkRawText.newTree()

    else:
      echo lexer.error("22").msg
      raiseAssert("#[ IMPLEMENT ]#")


    result.add onkStmtList.newTree()
    while lexer[] == '{':
      result[^1].add onkRawText.newTree(lexer.getInsideBalanced('{', '}'))

  else:
    raise lexer.error("Expected @-entry")

proc parseBracket*(lexer): OrgNode =
  ## Parse any square bracket entry starting at current lexer position, and
  ## return it.
  if lexer[0..1] == "[[":
    # Link start
    discard

  elif lexer[] == '[':
    # Inactive timestamp, or `BracTag`
    echov lexer @? 0 .. 10

    const start = {'!', '>', '<', '*', '#', '?', '@'} + {'A' .. 'Z'}

    if lexer[+1] in start and
       lexer[+2] in start:
      let body = lexer.getInsideSimple('[', ']').toSlice(lexer).split('|')
      result = onkBracTag.newTree()
      for slice in body:
        result.add onkBareIdent.newTree(
          slice.toSlice(lexer).strip().toSlice(lexer))

      # quit 0
    else:
      let body = lexer.getInsideSimple('[', ']').toSlice(lexer)
      result = onkTimeStamp.newTree(body)




proc parseMacro*(lexer): OrgNode =
  lexer.skipExpected("{{")
  result = onkMacro.newTree(
    onkRawText.newTree(
      lexer.getInsideBalanced('{', '}')))

  lexer.skipExpected("}}")


proc parseOptMacro*(lexer): OrgNode =
  case lexer.nextSet({'{'}, OBareIdentChars - {'{'}):
    of 0:
      discard lexer.getSkipUntil({'{'})
      if lexer["{{{"]:
        return onkResult.newTree(lexer.parseMacro())

      else:
        return newEmptyNode()

    of 1:
      return newEmptyNode()


proc parseSrcInline*(lexer): OrgNode =
  assert lexer["src_"]
  lexer.advance(4)
  result = onkSrcCode.newTree()
  result.add lexer.parseIdent()
  case lexer[]:
    of '[':
      result.add onkRawText.newTree(lexer.getInsideBalanced('[', ']'))
      result.add onkRawText.newTree(lexer.getInsideBalanced('{', '}'))

    of '{':
      result.add newEmptyNode()
      result.add onkRawText.newTree(lexer.getInsideBalanced('{', '}'))

    else:
      raiseAssert("#[ IMPLEMENT ]#")


  lexer.skip()
  result.add parseOptMacro(lexer)

proc parseCallInline*(lexer): OrgNode =
  assert lexer["call_"]
  lexer.advance(5)
  result = onkCallCode.newTree()
  result.add lexer.parseIdent()
  case lexer[]:
    of '[':
      result.add lexer.getInsideBalanced('[', ']').newSublexer().withResIt do:
        parseCmdArguments(it)

      lexer.skip()
      result.add onkRawText.newTree(
        lexer.getInsideBalanced('(', ')'))

    of '(':
      result.add newEmptyNode()
      lexer.skip()
      result.add onkRawText.newTree(lexer.getInsideBalanced('(', ')'))

    else:
      raiseAssert("#[ IMPLEMENT ]#")

  lexer.skip()
  if lexer[] == '[':
    result.add lexer.getInsideBalanced('[', ']').newSublexer().withResIt do:
      parseCmdArguments(it)

  else:
    result.add newEmptyNode()

  lexer.skip()
  result.add parseOptMacro(lexer)


proc parseHashTag*(lexer): OrgNode =
  assert lexer[] == '#'
  lexer.advance()

  proc aux(lexer): OrgNode =
    result = onkHashTag.newTree()
    # `#tag`
    result.add lexer.parseIdent()

    # `#tag##[sub1, sub2]`
    if lexer[0 .. 2] == "##[":
      lexer.advance(3)

      while lexer[] != ']':
        # TODO on broken tags this would cause compilation errors and/or
        # whole text getting dragged into single tag body.
        result.add aux(lexer)
        lexer.skip()
        if lexer[] != ']':
          lexer.skipExpected(",")
          lexer.skip()

      lexer.advance()

    # `#tag##sub`
    elif lexer[0 .. 1] == "##":
      lexer.advance(2)
      result.add aux(lexer)



  return aux(lexer)

proc parseInlineMath*(lexer): OrgNode =
  ## Parse inline math expression, starting with any of `$`, `$$`, `\(`,
  ## and `\[`.

  assert lexer[] == '$'
  lexer.advance()
  result = onkMath.newTree(lexer.getSkipUntil({'$'}).toSlice(lexer))
  lexer.advance()


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




proc parseText*(lexer): seq[OrgNode] =
  # Text parsing is implemented using non-recusive descent parser that
  # maintains stack explicitly (instead of constructing it via function
  # calls). This is made in order to provide support for stack
  # introspection at any given moment of parsing, and perform context-aware
  # decisions. Input lexer is parsed *until the end* - e.g you need to
  # always pass sublexer.

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

  template pushBuf(): untyped =
    # If buffer is non-empty push it as new word. Most of the logic in this
    # template is for dealing with whitespaces in buffers and separating
    # them into smaller things. For example `"buffer with space"` should be
    # handled as five different `Word`, instead of a single one.

    # Buffer is pushed before parsing each inline entry such as `$math$`,
    # `#tags` etc.
    if len(buf) > 0:
      var text = lexer.initEmptyStrRanges().toSlice(lexer)
      var bigIdent = lexer.initEmptyStrRanges().toSlice(lexer)
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

              pushWith(false, onkBigIdent.newTree(res.toSlice(lexer)))
              if trailIdx.len > 0:
                pushWith(false, onkWord.newTree(trail.toSlice(lexer)))


            bigIdent.ranges = @[]

            pushWith(false, onkWord.newTree(text))

          text = initStrRanges(i, i).toSlice(lexer)

          if i == high(buf) and changeRegion:
            pushWith(false, onkWord.newTree(text))


        else:
          text.add i


      buf = lexer.initEmptyStrRanges().toSlice(lexer)


  stack.add @[]

  var buf = lexer.initEmptyStrRanges().toSlice(lexer)

  while lexer[] != OEndOfFile:
    # More sophisticated heuristics should be used to detect edge cases
    # like `~/me~`, `*sentence*.` and others. Since particular details are
    # not fully fleshed out I will leave it as it is now, and concentrate
    # on other parts of the document.
    # echov lexer @? 0 .. 5
    case lexer[]:
      of {'*', '_', '/', '~', '`', '+', '='} + {'\'', '"'}:
        var ch: string
        if lexer.isOpenAt(ch):
          # Start of the regular, constrained markup section.
          # Unconditinally push new layer.
          pushBuf()
          pushWith(true, onkMarkup.newTree($ch))

        elif lexer.isCloseAt(ch):
          # End of regular constrained section, unconditionally close
          # current layer, possibly with warnings for things like
          # `*/not-fully-italic*`
          pushBuf()
          closeAllWith(getLayerOpen(ch), ch)

        elif lexer.isToggleAt(ch):
          # Detected unconstrained formatting block, will handle it
          # regardless.
          let layerOpen = getLayerOpen(ch)
          if layerOpen != -1:
            closeAllWith(layerOpen, ch)

          else:
            pushWith(true, onkMarkup.newTree(ch))

          lexer.advance()

        else:
          buf.add lexer.pop()

        if ch.len != 0:
          lexer.advance()

      of '$':
        if lexer[-1] in OEmptyChars:
          pushBuf()
          pushWith(false, parseInlineMath(lexer))

        else:
          raiseAssert("#[ IMPLEMENT ]#")

      of '@':
        if lexer[-1] in OEmptyChars:
          pushBuf()
          pushWith(false, parseAtEntry(lexer))

        else:
          buf.add lexer.pop

      of '#':
        if lexer[-1] in OEmptyChars and
           lexer[+1] == '[':
          pushBuf()

          lexer.advance()
          pushWith(false, onkComment.newTree(
            lexer.getInsideBalanced('[', ']'),
          ))

          lexer.skipExpected("#")

        elif lexer[-1] in OEmptyChars and
             lexer[+1] in OWordChars:
          pushBuf()
          pushWith(false, parseHashTag(lexer))

        else:
          buf.add lexer.pop

      of '[':
        if lexer[-1] in OEmptyChars:
          pushBuf()
          pushWith(false, parseBracket(lexer))

        else:
          buf.add lexer.pop

      elif lexer["src_"]:
        pushWith(false, lexer.parseSrcInline())

      elif lexer["call_"]:
        pushWith(false, lexer.parseCallInline())

      else:
        buf.add lexer.pop

  pushBuf()
  while stack.len > 1:
    closeWith("")

  return stack[0].mapIt(it.node)



proc parseParagraph*(lexer): OrgNode =
  result = onkParagraph.newTree(lexer.parseText())


proc parseOrgCookie*(lexer): OrgNode =
  if lexer[] == '[':
    result = onkUrgencyStatus.newTree(
      getInsideSimple(lexer, '[', ']').toSlice(lexer))

  else:
    result = newEmptyNode()



proc parseList*(lexer): OrgNode =
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
  result = onkList.newTree()


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

    var item = onkListItem.newTree(
      onkRawText.newTree(bullet.toSlice(lexer))
    )

    itemLexer.skip()
    # Parse counter-set and checkbox
    if itemLexer[] == '[':
      if itemLexer[+1] == '@':
        item.add onkCounter.newTree(
          itemLexer.getInsideSimple('[', ']').toSlice(lexer))

        itemLexer.skip()
        if itemLexer[] == '[':
          item.add onkCheckbox.newTree(
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

    # There is no smart logic behind this wall of conditionals, it is just
    # a reasult of defensive coding against possible edge cases that I
    # managed to come up with.
    if tagRanges.len > 1:
      item.add tagRanges[0].toSlice(lexer).newSublexer().withResIt do:
        it.parseParagraph()

      let paragraph = overlapping(
        @[tagRanges[0], tagRanges[1]], tern(
          cookieRanges.len > 1, # If completion cookie is present use it
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
          it.parseParagraph()

    elif tagRanges.len == 1:
      item.add newEmptyNode()

      if cookieRanges.len > 1:
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
          it.parseParagraph()

      else:
        item.add tagRanges[0].toSlice(lexer).newSublexer().withResIt do:
          it.parseParagraph()


    if cookieRanges.len > 1:
      item.add overlapping(
        @[cookieRanges[^2]], cookieRanges[^1]
      ).toSlice(lexer).newSublexer.withResIt do:
        onkCompletion.newTree(it.getInsideSimple('[', ']').toSlice(lexer))

    else:
      item.add newEmptyNode()

    item.add itemLexer.parseStmtList()
    result.add item

proc parseSubtree(lexer): OrgNode =
  result = OrgNode(kind: onkSubtree)

  result.add onkBareIdent.newTree(lexer.getSkipWhile({'*'}).toSlice(lexer))
  lexer.skip()

  result.add lexer.optGetWhile(OBigIdentChars, onkBigIdent)
  lexer.skip()

  result.add parseOrgCookie(lexer)

  lexer.skip()

  var headerLexer = lexer.indentedSublexer(
    result["prefix"].charLen(),
    keepNewlines = false,
    requireContinuation = true,
    fromInline = true
  )

  result.add parseParagraph(headerLexer)

  # IMPLEMENT instead of cutting whole header string into sublexer, first
  # check for subtree completion status and tags, and then parse things.
  result.add newEmptyNode()
  result.add newEmptyNode()

  result.add parseDrawers(lexer)

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


proc detectStart(lexer): OrgStart =
  let ch = lexer[]
  case ch:
    elif ch == '#':
      if lexer["#+begin"]:
        result = otkBeginCommand

      elif lexer["#+"]:
        result = otkCommand

      elif lexer["#["]:
        # Inline comment start
        result = otkParagraph

      elif lexer["# "]:
        # Comment until end of line
        result = otkLineComment

      else:
        # Text startsing with tag
        result = otkParagraph

    elif ch == '*':
      if lexer.column == 0:
        var idx = 0
        while lexer[idx] in {'*'}:
          inc idx

        if lexer[idx] in {' '}:
          # `***** Heading`
          result = otkSubtreeStart

        else:
          # `*First bold*`
          result = otkParagraph

      else:
        result = otkListStart

    elif ch in OListChars:
      if lexer.listStartChar() == OEndOfFile:
        result = otkParagraph

      else:
        result = otkList

    elif ch in OWordChars + {'['}:
      result = otkParagraph

    elif ch == '\n':
      lexer.skip({'\n'})
      return detectStart(lexer)

    elif ch == OEndOfFile:
      result = otkEOF

    elif ch == ':':
      var idx = 0
      while lexer[idx] in OIdentChars:
        inc idx

      if lexer[idx] == ':':
        result = otkDrawer

      else:
        result = otkParagraph

    elif lexer[] in OMarkupChars:
      result = otkParagraph

    else:
      raiseAssert(&"#[ IMPLEMENT on char {[lexer[]]} ]#")



proc parseStmtList(lexer): OrgNode =
  result = OrgNode(kind: onkStmtList)
  while lexer[] != OEndOfFile:
    let kind = lexer.detectStart()
    case kind:
      of otkBeginCommand:
        result.add parseMultilineCommand(lexer)

      of otkCommand:
        result.add parseCommand(lexer)

      of otkSubtreeStart:
        result.add parseSubtree(lexer)

      of otkParagraph:
        var paragraphLexer = lexer.indentedSublexer(
          0,
          keepNewlines = false,
          requireContinuation = false,
          fromInline = false
        )

        result.add onkParagraph.newTree(paragraphLexer.parseText())

      of otkEOF:
        break

      of otkDrawer:
        result.add parseDrawer(lexer)

      of otkList:
        result.add parseList(lexer)

      else:
        raiseAssert(&"#[ IMPLEMENT for kind {kind} {instantiationInfo()} ]#")

    discard lexer.skip(Newlines + Whitespace)




proc parseOrg*(str: string): OrgNode =
  startHax()
  var lexer = newLexer(newStrBufSlice(str))

  result = parseStmtList(lexer)
  echo result.treeRepr()
