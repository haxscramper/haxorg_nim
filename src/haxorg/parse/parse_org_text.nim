import
  hmisc/algo/[hparse_base, hlex_base]

import
  ../defs/org_types

proc lexText(str: var PosStr): seq[OrgTextToken] =
  if not ?str:
    result.add str.initEof(ottEof)

  else:
    case str[]:
      else:
        raise newUnexpectedCharError(str)

using
  lexer: var OrgTextLexer
  parseConf: ParseConf

proc parseHashTag*(lexer, parseConf): OrgNode =
  when false:
    assert lexer[] == '#'
    lexer.advance()

    proc aux(lexer, parseConf): OrgNode =
      result = onkHashTag.newTree()
      # `#tag`
      result.add lexer.parseIdent()

      # `#tag##[sub1, sub2]`
      if lexer[0 .. 2] == "##[":
        lexer.advance(3)

        while lexer[] != ']':
          # TODO on broken tags this would cause compilation errors and/or
          # whole text getting dragged into single tag body.
          result.add aux(lexer, parseConf)
          lexer.skip()
          if lexer[] != ']':
            lexer.skipExpected(",")
            lexer.skip()

        lexer.advance()

      # `#tag##sub`
      elif lexer[0 .. 1] == "##":
        lexer.advance(2)
        result.add aux(lexer, parseConf)



    return aux(lexer, parseConf)


proc parseText*(lexer, parseConf): seq[OrgNode] =
  when false:
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
        buf = lexer.initEmptyStrRanges().toSlice(lexer)

      elif len(buf) > 0:
        for node in lexer.splitTextBuf(buf, parseConf.dropEmptyWords):
          pushWith(false, node)


        buf = lexer.initEmptyStrRanges().toSlice(lexer)


    stack.add @[]

    var buf = lexer.initEmptyStrRanges().toSlice(lexer)

    while lexer[] != OEndOfFile:
      # More sophisticated heuristics should be used to detect edge cases
      # like `~/me~`, `*sentence*.` and others. Since particular details are
      # not fully fleshed out I will leave it as it is now, and concentrate
      # on other parts of the document.

      const markChars = OMarkupChars + {'\'', '"'} +
        OPunctOpenChars + OPunctCloseChars - {'[', ']', '<', '>'}

      case lexer[]:
        of markChars:
          var ch: string
          var hadPop = false
          if not inVerbatim and lexer.isOpenAt(
            ch, markChars + OPunctOpenChars):
            # Start of the regular, constrained markup section.
            # Unconditinally push new layer.
            pushBuf()
            pushWith(true, newTree(onkMarkup, classifyMarkKind(ch[0]), $ch))

            if ch[0] in OVerbatimChars:
              inVerbatim = true

          elif lexer.isCloseAt(ch, markChars + OPunctCloseChars):
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
                buf.add lexer.pop()

            else:
              hadPop = true
              buf.add lexer.pop()

          elif lexer.isToggleAt(ch):
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

              lexer.advance()

            elif inVerbatim:
              hadPop = true
              buf.add lexer.pop()


            else:
              if isOpening:
                # Push new markup opening, no verbatim currently active
                pushWith(true, newTree(onkMarkup, classifyMarkKind(ch[0]), ch))
                lexer.advance()

              else:
                # Push new markup opening, no verbatim currently active
                closeAllWith(layerOpen, ch)
                lexer.advance()

          else:
            hadPop = true
            buf.add lexer.pop()

          if not hadPop:
            lexer.advance()


        of '$':
          if lexer[-1] in OEmptyChars:
            pushBuf()
            pushWith(false, parseInlineMath(lexer, parseConf))

          else:
            raiseAssert("#[ IMPLEMENT ]#")

        of '@':
          if lexer[-1] in OEmptyChars:
            pushBuf()
            pushWith(false, parseAtEntry(lexer, parseConf))

          else:
            buf.add lexer.pop

        of '#':
          if lexer[-1] in OEmptyChars and
             lexer[+1] == '[' and
             not inVerbatim:

            pushBuf()

            lexer.advance()
            pushWith(false, onkComment.newTree(
              lexer.getInsideBalanced('[', ']'),
            ))

            lexer.skipExpected("#")

          elif lexer[-1] in OEmptyChars and
               lexer[+1] in OWordChars and
               not inVerbatim:

            pushBuf()
            pushWith(false, parseHashTag(lexer, parseConf))

          else:
            buf.add lexer.pop

        of '[':
          if lexer[-1] in OEmptyChars and
             not inVerbatim:
            pushBuf()
            let node = parseBracket(lexer, parseConf, buf)
            if not node.isNil:
              pushWith(false, node)

          else:
            buf.add lexer.pop

        of '\\':
          let node = lexer.parseSlashEntry(parseConf, buf)
          if not node.isNil:
            pushWith(false, node)

        of '<':
          if inVerbatim:
            buf.add lexer.pop()

          else:
            let node = lexer.parseAngleEntry(parseConf, buf)
            if not node.isNil:
              pushWith(false, node)

        elif lexer["src_"]:
          pushWith(false, lexer.parseSrcInline(parseConf))

        elif lexer["call_"]:
          pushWith(false, lexer.parseCallInline(parseConf))

        else:
          buf.add lexer.pop

    pushBuf()
    while stack.len > 1:
      closeWith("")

    return stack[0].mapIt(it.node)
