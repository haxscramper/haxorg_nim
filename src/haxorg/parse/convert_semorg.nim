import fusion/matching


proc toSemOrg*(
    node: OrgNode,
    config: RunConf = defaultRunConf,
    scope: seq[TreeScope] = @[]
   ): SemOrg


proc convertOrgLink*(
    link: OrgNode, config: RunConf, scope: seq[TreeScope]
  ): OrgLink

proc convertMetaTag*(
    tag: OrgNode, config: RunConf, scope: seq[TreeScope]
  ): SemMetaTag =

  let tagKind = strutils.parseEnum[SemMetaTagKind](
    $tag["name"].text, smtUnresolved)

  result = SemMetaTag(kind: tagKind)
  if tagKind == smtImport:
    result.importLink = convertOrgLink(
      tag["body"], config, scope)


proc convertOrgLink*(
    link: OrgNode, config: RunConf, scope: seq[TreeScope]
  ): OrgLink =

  assertKind(link, {onkLink})

  var str = initPosStr(link["link"].strVal())

  # echov link["link"].strVal()

  let format = str.popUntil({':'})
  # if format.len == 0:
  #   raise newArgumentError(
  #     "Cannot convert link with empty format")

  str.advance()
  case format.normalize():
    of "code":
      return config.linkResolver(format, str)

    else:
      if isNil(config.linkResolver):
        raise newArgumentError(
          "Cannot resolve link with format", format,
          ". Current running config `linkResolver` is `nil`.",
          "Link string value:", link[0].strVal()
        )

      else:
        # if format.len == 0:
        #   echov link.treeRepr()

        return config.linkResolver(format, str)



proc toSemOrg*(
    node: OrgNode,
    config: RunConf = defaultRunConf,
    scope: seq[TreeScope] = @[]
   ): SemOrg =


  template writeSubnodes(): untyped =
    if result.kind in orgSubnodeKinds:
      for subnode in node.subnodes:
        result.add toSemOrg(subnode, config, tern(
          node.kind == onkSubtree,
          scope & @[TreeScope(tree: result)],
          scope
        ))


  case node:
    of SrcCode({ "lang" : @lang }):
      result = newSemOrg(node)
      result.codeBlock = config.newCodeBlock($lang.text)

      parseFrom(result.codeBlock, result, scope)

    of BigIdent(text: @text):
      let ident = $text
      result = newSemOrg(node)
      result.bigIdentKind = strutils.parseEnum[OrgBigIdentKind]($text, obiOther)

    of ListItem[
      @bullet, @counter, @checkbox, @tag, @header, @completion, @body
    ]:
      result = newSemOrg(node)

      result.itemBullet = $bullet.text

      case tag:
        of Paragraph[BigIdent(text: @idText)]:
          result.itemTag = some(SemItemTag(
            kind: sitBigIdent,
            idText: $idText,
            idKind: strutils.parseEnum($idText, obiOther),
          ))

        of Paragraph[@tag is MetaTag()]:
          result.itemTag = some(SemItemTag(
            kind: sitMeta,
            meta: convertMetaTag(tag, config, scope)
          ))

      writeSubnodes()

    of Subtree[
      @prefix, @todo, @urgency, @title, @completion,
      @tags, @times, @drawers, @body
    ]:

      result = newSemOrg(node)

      result.subtLevel = len($prefix.text)
      result.subtTags = split($tags.text, ":")

      writeSubnodes()

    of MetaTag():
      result = newSemOrg(node)
      result.metaTag = convertMetaTag(node, config, scope)
      writeSubnodes()

    of Link():
      result = newSemOrg(node)
      result.linkTarget = convertOrgLink(node, config, scope)

      if node.len > 1 and node[^1].kind != onkEmptyNode:
        result.linkDescription = some toSemOrg(node[^1])

    else:
      result = newSemOrg(node)
      writeSubnodes()


proc toSemOrgDocument*(
    node: OrgNode,
    config: RunConf = defaultRunConf
  ): SemOrg =

  result = SemOrg(kind: onkDocument, isGenerated: true)
  result.add toSemOrg(node, config)
