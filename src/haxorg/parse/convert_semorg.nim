import
  fusion/matching,
  std/[strutils, macros, tables, strformat]

import
  ../defs/[org_types, impl_sem_org, impl_org_node]

import
  hmisc/core/all,
  hmisc/types/colorstring


macro unpackNode(node: OrgNode, subnodes: untyped{nkBracket}): untyped =
  result = newStmtList()
  for idx, name in subnodes:
    result.add newVarStmt(name, nnkBracketExpr.newtree(node, newLit(idx)))


proc toSem*(
    node: OrgNode, config: RunConf, scope: seq[TreeScope]): SemOrg


proc convertOrgLink*(
    link: OrgNode, config: RunConf, scope: seq[TreeScope]): OrgLink

proc convertMetaTag*(
    tag: OrgNode, config: RunConf, scope: seq[TreeScope]): MetaTag =

  when false:
    let tagKind = strutils.parseEnum[MetaTagKind](
      $tag["name"].text, smtUnresolved)

    result = MetaTag(kind: tagKind)
    if tagKind == smtImport:
      result.importLink = convertOrgLink(
        tag["body"], config, scope)


proc convertOrgLink*(
    link: OrgNode, config: RunConf, scope: seq[TreeScope]): OrgLink =

  when false:
    assertKind(link, {orgLink})

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


proc convertSubtree*(node: OrgNode, config: RunConf, scope: seq[TreeScope]): Subtree =
  node.unpackNode([prefix, todo, urgency, title, completion, tags, times, drawers, body])
  result = Subtree()

proc toSem*(
    node: OrgNode, config: RunConf, scope: seq[TreeScope]): SemOrg =

  case node.kind:
    of orgStmtList, orgParagraph:
      result = newSem(node)
      for sub in items(node):
        result.add toSem(sub, config, scope)

    of orgSubtree:
      let tree = convertSubtree(node, config, scope)
      # TODO add new subtree to scope

      result = newSem(
        node,
        toSem(node["title"], config, scope),
        toSem(node["body"], config, scope))
      result.subtree = tree

    of orgWord:
      result = newSem(node)


    of orgSrcCode:
      let lang: string = node["lang"].strVal()
      if lang notin config.codeCreateCallbacks:
        raise newArgumentError(
          &"Cannot create code block for language '{lang}' - " &
            "construction callback is missing from `config.codeCreateCallbacks`")

      else:
        result = newSem(node)
        result.codeBlock = config.codeCreateCallbacks[lang]()
        result.codeBlock.parseFrom(node, scope)

    else:
      raise newImplementKindError(node, $node.treeRepr())

  when false:

    template writeSubnodes(): untyped =
      if result.kind in orgSubnodeKinds:
        for subnode in node.subnodes:
          result.add toSemOrg(subnode, config, tern(
            node.kind == orgSubtree,
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

        if node.len > 1 and node[^1].kind != orgEmptyNode:
          result.linkDescription = some toSemOrg(node[^1])

      else:
        result = newSemOrg(node)
        writeSubnodes()


proc toSem*(node: OrgNode, conf: RunConf): SemOrg =
  toSem(node, conf, @[])

proc toSemDocument*(node: OrgNode, config: RunConf): SemOrg =
  result = newSem(orgDocument)
  result.add toSem(node, config, @[])
