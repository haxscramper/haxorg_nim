import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/core/all

import
  std/[sequtils, algorithm, parseutils]

import
  ../defs/[org_types, impl_org_node],
  ./parse_org_code


proc newTree*(kind: OrgNodeKind, tok: OrgTextToken): OrgNode =
  newTree(kind, initPosStr(tok))

using
  lexer: var OrgTextLexer
  parseConf: ParseConf

proc initTextLexer*(str: var PosStr): OrgTextLexer =
  initLexer(str, lexText)


proc parseAtEntry*(lexer, parseConf): OrgNode =
  ## Parse any entry starting with `@` sign - metatags, annotations, inline
  ## backend passes.
  when false:
    if lexer["@@"]:
      # Inline backend pass
      discard

    elif lexer["@["]:
      # Annotation start
      discard

    elif lexer[] == '@' and lexer[+1] in OIdentChars:
      # Metatag start OR random `@` in the text
      lexer.advance()
      let id = lexer.parseIdent(oskMetaTagIdent)
      if lexer[] == '[':
        result = newTree(
          orgMetaTag,
          id,
          newTree(
            orgRawText,
            oskMetaTagArgs,
            lexer.getInsideSimple('[', ']').toSlice(lexer))
        )

      elif lexer[] == '{':
        result = newTree(orgMetaTag, id)
        result.add newEmptyNode(oskMetatagArgs)

      else:
        raise newImplementError(lexer.error("22").msg)

      if id.strVal() == "import":
        var sub = lexer.getInsideBalanced('{', '}').newSublexer()
        var buf: PosStr
        result.add parseBracket(sub, parseConf, buf)

      else:
        result.add orgStmtList.newTree()
        while lexer[] == '{':
          result[^1].add newTree(
            orgRawText, oskMetatagText, lexer.getInsideBalanced('{', '}'))

    else:
      raise lexer.error("Expected @-entry")

proc parseSlashEntry*(lexer, parseConf): OrgNode =
  when false:
    assert lexer[] == '\\'
    if lexer[+1] in OIdentStartChars:
      var ahead = lexer
      ahead.advance()
      if ahead.allUntil(OIdentChars, OWhitespace + {'{', OEndOfFile}):
        lexer.advance()
        result = orgSymbol.newTree(
          lexer.getSkipWhile(OIdentChars).toSlice(lexer))

        if lexer["{}"]:
          lexer.advance(2)

      else:
        buf.add lexer.pop()
        buf.add lexer.pop()

    else:
      buf.add lexer.pop()
      buf.add lexer.pop()



proc parseMacro*(lexer, parseConf): OrgNode =
  when false:
    lexer.skipExpected("{{")
    result = orgMacro.newTree(
      orgRawText.newTree(
        lexer.getInsideBalanced('{', '}')))

    lexer.skipExpected("}}")


proc parseOptMacro*(lexer, parseConf): OrgNode =
  when false:
    case lexer.nextSet({'{'}, OBareIdentChars - {'{'}):
      of 0:
        discard lexer.getSkipUntil({'{'})
        if lexer["{{{"]:
          return orgResult.newTree(lexer.parseMacro(parseConf))

        else:
          return newEmptyNode()

      of 1:
        return newEmptyNode()


proc parseHashTag*(lexer, parseConf): OrgNode =
  newTree(orgIdent, lexer.popAsStr())

proc parseInlineMath*(lexer, parseConf): OrgNode =
  ## Parse inline math expression, starting with any of `$`, `$$`, `\(`,
  ## and `\[`.

  let close =
    case lexer[].kind:
      of ottDollarOpen: ottDollarClose
      of ottDoubleDollarOpen: ottDoubleDollarClose
      of ottLatexParOpen: ottLatexParClose
      of ottLatexBraceOpen: ottLatexBraceClose
      else: raise newUnexpectedTokenError(lexer)

  lexer.next()
  result = newTree(orgMath, lexer.popAsStr({ottLatexInlineRaw}))
  lexer.skip({close})




proc parseText*(lexer, parseConf): seq[OrgNode]
proc parseLink*(lexer, parseConf): OrgNode =
  var link = newTree(orgLink)

  lexer.skip(ottLinkOpen)
  lexer.skip(ottLinkTargetOpen)

  var target = newTree(orgLinkTarget, lexer.popAsStr({ottRawText}))
  let str = target.strVal()
  link.add target

  if str[0] == '(' and str[^1] == ')':
    link.subKind = oskLinkCallout

  elif str[0] == '*':
    link.subKind = oskLinkSubtree

  elif str[0] == '#':
    link.subKind = oskLinkId

  elif str[0] in {'.', '/'}:
    link.subKind = oskLinkFile

  else:
    let colon = str.skipWhile({'a' .. 'z'})
    if colon < str.len and str[colon] == ':':
      let protocol = str[0 .. colon]
      case protocol:
        of "file": link.subKind = oskLinkFile
        else: raise newImplementKindError(protocol)

    else:
      link.subKind = oskLinkImplicit

  lexer.skip(ottLinkTargetClose)


  if lexer[ottLinkDescriptionOpen]:
    lexer.skip(ottLinkDescriptionOpen)
    var desc: seq[OrgTextToken]
    while ?lexer and not lexer[ottLinkDescriptionClose]:
      desc.add lexer.pop()

    var descLexer = initLexer(desc)

    link.add newTree(orgParagraph, parseText(descLexer, parseConf))
    lexer.skip(ottLinkDescriptionClose)

  else:
    link.add newOrgEmpty()

  lexer.skip(ottLinkClose)

  return link



proc getLastLevel(node: var OrgNode, level: int): var OrgNode =
  when false:
    case level:
      of 0: return node
      of 1: return node[^1]
      of 2: return node[^1][^1]
      of 3: return node[^1][^1][^1]
      else: return getLastLevel(node, level - 4)

proc getLastLevel(node: OrgNode, level: int): OrgNode =
  when false:
    case level:
      of 0: node
      of 1: node[^1]
      else: getLastLevel(node, level - 2)


    # lexer.parseIdent()

proc parseAngleEntry*(lexer, parseConf; buf: var PosStr): OrgNode =
  when false:
    if not lexer.isBalancedToEOL():
      buf.add lexer.pop()

    else:
      var lexer = lexer.getInsideBalanced('<', '>').newSublexer()
      if lexer[0] == '+' and lexer[^1] == '+':
        lexer.next()
        var buf = lexer.initEmptyStrRanges()
        while not lexer.atEnd():
          buf.add lexer.pop()

        buf.pop()

        var textlexer = newSublexer(buf.toSlice(lexer))
        result = orgPlaceholder.newTree(
          textlexer.parseParagraph(parseConf))

      elif lexer[0] == '<' and lexer[^1] == '>':
        lexer.next()
        var buf = lexer.initStrRanges()
        while not lexer.atEnd():
          buf.add lexer.pop()

        buf.pop()

        result = orgRadioTarget.newTree(
          orgRawText.newTree(buf.toSlice(lexer)))

      else:
        result = orgPlaceholder.newTree(lexer.parseParagraph(parseConf))

proc parseText*(str: PosStr, conf: ParseConf): OrgNode =
  var lexer = initLexer(str, lexText)
  result = newTree(orgParagraph, parseText(lexer, conf))
