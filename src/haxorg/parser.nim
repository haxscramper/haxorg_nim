import lexer, ast, common, buf
import hmisc/helpers
import std/[strutils, sequtils, strformat, streams]

using lexer: var Lexer

proc parseBareIdent*(lexer): OrgNode =
  lexer.skip()
  result = newBareIdent(getSkipWhile(lexer, OBareIdentChars).toSlice(lexer))

proc parseCommandArgs*(lexer): OrgNode =
  result = onkRawText.newTree(lexer.getSkipToEOL().toSlice(lexer))

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
  echov lexer @? 0 .. 10
  result = onkCommand.newTree()
  lexer.skipExpected("#+")
  result.add newOrgIdent(lexer.getSkipWhileTo(OIdentChars, ':').toSlice(lexer))
  lexer.advance()
  result.add parseCommandArgs(lexer)
  lexer.nextLine()


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

  result.add newOrgIdent(lexer.getSkipWhileTo(OIdentChars, ':').toSlice(lexer))

  result.add parseCommandArgs(lexer)
  lexer.nextLine()

  if result["name"].text == "table":
    result = onkTable.newTree(result[^1])
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

                for elem in cells.toSlice(lexer).split("|"):
                  rowcells.add onkTableCell.newTree(
                    newEmptyNode(),
                    newWord(elem.toSlice(lexer).strip().toSlice(lexer))
                  )

              else:
                rowtext.add newWord(rowlexer.getSkipToEOL().toSlice(lexer))
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
                rowtext.add newWord(rowlexer.getSkipToEOL().toSlice(lexer))
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
                newWord(rowlexer.getBlockUntil("#+cell:").toSlice(lexer))
              )

        resrow.add rowtext
        resrow.add rowcells



      result.add resrow

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
        onkRawText.newTree(lexer.getInsideSimple(('[', ']')).toSlice(lexer)))

    elif lexer[] == '{':
      result = onkMetaTag.newTree(id)
      result.add onkRawText.newTree()

    else:
      echo lexer.error("22").msg
      raiseAssert("#[ IMPLEMENT ]#")


    result.add onkStmtList.newTree()
    while lexer[] == '{':
      result[^1].add onkRawText.newTree(lexer.getInsideBalanced(('{', '}')))

  else:
    raise lexer.error("Expected @-entry")

proc parseBracket*(lexer): OrgNode =
  ## Parse any square bracket entry starting at current lexer position, and
  ## return it.
  if lexer[0..1] == "[[":
    # Link start
    discard

  elif lexer[] == '[':
    # Inactive timestamp
    discard

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
  var stack: seq[seq[tuple[pending: bool,
                           node: OrgNode]]]

  template closeWith(ch: string): untyped =
    # Close last pending node in stack is there is any, otherwise move
    # current layer not lower one.

    # IMPLEMENT handling of missing node pairs should happen here - close
    # request should be performed unconditionally (e.g. at the end of text
    # parsing all elements are closed), but some blocks might be missing
    # nodes. In this case markup delimiter should be pasted as regular word
    # (and warning should be emitted).
    let layer = stack.pop
    if (stack.last.len > 0 and stack.last2.pending):
      stack.last2.pending = false
      stack.last2.node.add layer.mapIt(it.node)

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
      var text = initStrRanges(buf.ranges).toSlice(lexer)
      for i in indices(buf):
        let changeRegion =
          # Started whitespace region, flushing buffer
          (
            buf[i] in Whitespace and
            text.len > 0 and
            text.lastChar() notin Whitespace
          ) or
          # Finished whitespace region
          (
            buf[i] notin Whitespace and
            text.len > 0 and
            text.lastChar() in Whitespace
          )

        if changeRegion or (i == high(buf)):
          # Finished input buffer or found region change

          if i == high(buf) and not changeRegion:
            # Found last character.
            text.add i

          pushWith(false, onkWord.newTree(text))

          text = initStrRanges(i, i).toSlice(lexer)

          if i == high(buf) and changeRegion:
            pushWith(false, onkWord.newTree(text))


        else:
          text.add i


      buf = lexer.initStrRanges().toSlice(lexer)


  stack.add @[]

  var buf = lexer.initStrRanges().toSlice(lexer)

  while lexer[] != OEndOfFile:
    # More sophisticated heuristics should be used to detect edge cases
    # like `~/me~`, `*sentence*.` and others. Since particular details are
    # not fully fleshed out I will leave it as it is now, and concentrate
    # on other parts of the document.
    case lexer[]:
      of {'*', '_', '/', '~', '`', '+', '='} + {'\'', '"'}:
        let ch = lexer[]
        if lexer[-1] notin OWordChars and lexer[+1] != ch:
          # Start of the regular, constrained markup section.
          # Unconditinally push new layer.
          pushBuf()
          pushWith(true, onkMarkup.newTree($ch))

        elif lexer[+1] notin OWordChars:
          # End of regular constrained section, unconditionally close
          # current layer, possibly with warnings for things like
          # `*/not-fully-italic*`
          pushBuf()
          closeWith($ch)


        elif lexer[+1] == ch:
          # Detected unconstrained formatting block, will handle it
          # regardless.
          let ch = $ch & $ch
          pushBuf()

          # If it matches underlying element in text, close it, otherwise
          # push new layers. Pushing new layers indefinitely is safe,
          # because everything will be closed on main function return
          # (possibly with warnings though)
          if stack.len > 1 and
             stack[^2][^1].pending and
             stack[^2][^1].node.getStr() == ch:
            # Close unconstrainted block
            closeWith(ch)

          else:
            # Open unconstrained block
            pushWith(true, onkMarkup.newTree(ch))

          lexer.advance()

        else:
          buf.add lexer.pop



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
          raiseAssert("#[ IMPLEMENT ]#")

      of '#':
        if lexer[-1] in OEmptyChars and
           lexer[+1] == '[':
          pushBuf()

          lexer.advance()
          pushWith(false, onkComment.newTree(
            lexer.getInsideSimple(('[', ']')).toSlice(lexer),
          ))

          lexer.skipExpected("#")

        elif lexer[-1] in OEmptyChars and
             lexer[+1] in OWordChars:
          pushBuf()
          pushWith(false, parseHashTag(lexer))

        else:
          buf.add lexer.pop


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
      getInsideSimple(lexer, ('[', ']')).toSlice(lexer))

  else:
    result = newEmptyNode()



proc parseDrawer*(lexer): OrgNode =
  result = onkDrawer.newTree()

  result.add onkIdent.newTree(
    lexer.getInsideSimple((':', ':')).toSlice(lexer))

  while true:
    if lexer[0 .. 4] == ":end:":
      discard lexer.getSkipToEOL()
      lexer.advance()
      return

    elif lexer[] == ':':
      result.add onkProperty.newTree(
        onkIdent.newTree(lexer.getInsideSimple((':', ':')).toSlice(lexer)),
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
    otkEof


proc detectStart(lexer): OrgStart =
  case lexer[]:
    of '#':
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

    of '*':
      if lexer.column == 0:
        result = otkSubtreeStart

      else:
        result = otkListStart

    of OWordChars:
      result = otkParagraph

    of '\n':
      lexer.skip({'\n'})
      return detectStart(lexer)

    of OEndOfFile:
      result = otkEOF

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

      else:
        raiseAssert(&"#[ IMPLEMENT for kind {kind} {instantiationInfo()} ]#")

    discard lexer.skip(Newlines + Whitespace)


  echo result.treeRepr()


proc parseOrg*(str: string): OrgNode =
  startHax()
  var lexer = newLexer(newStrBufSlice(str))

  parseStmtList(lexer)
