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

proc parseHashTag*(lexer, parseConf): OrgNode =
  newTree(orgIdent, lexer.popAsStr())

proc parseText*(lexer, parseConf): seq[OrgNode]

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
