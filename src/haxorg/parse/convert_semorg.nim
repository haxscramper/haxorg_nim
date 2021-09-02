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
    node: OrgNode, config: RunConf, scope: var SemConvertCtx): SemOrg

proc convertOrgLink*(
    link: OrgNode, config: RunConf, scope: var SemConvertCtx): OrgLink

proc convertMetaTag*(
    tag: OrgNode, config: RunConf, scope: var SemConvertCtx): MetaTag =

  when false:
    let tagKind = strutils.parseEnum[MetaTagKind](
      $tag["name"].text, smtUnresolved)

    result = MetaTag(kind: tagKind)
    if tagKind == smtImport:
      result.importLink = convertOrgLink(
        tag["body"], config, scope)


proc convertOrgLink*(
    link: OrgNode, config: RunConf, scope: var SemConvertCtx): OrgLink =

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


proc convertSubtree*(node: OrgNode, config: RunConf, scope: var SemConvertCtx): Subtree =
  node.unpackNode(
    [prefix, todo, urgency, title, completion, tags, times, drawers, body])
  result = Subtree()

  result.level = prefix.strVal().count('*')

proc convertList*(node: OrgNode, config: RunConf, scope: var SemConvertCtx): SemOrg =
  assertKind(node, {orgList})
  result = newSem(node)
  var listKind = oskNone
  for item in items(node):
    item.unpackNode([
      bullet, counter, checkbox, tag, header, completion, body])

    var outItem = newSem(item)
    case bullet.strVal()[0]:
      of '-', '+', '*':
        listKind = tern(listKind == oskNone, oskUnorderedList, oskMixedList)

      of '0' .. '9':
        listKind = tern(listKind == oskNone, oskOrderedList, oskMixedList)

      of 'a' .. 'z', 'A' .. 'Z':
        listKind = tern(listKind == oskNone, oskOrderedList, oskMixedList)

      else:
        raise newImplementKindError(bullet[0].strVal())

    outItem.add toSem(tag, config, scope)
    outItem.add toSem(header, config, scope)
    outItem.add toSem(body, config, scope)

    result.add outItem

  result.subKind = listKind


proc toSem*(
    node: OrgNode,
    config: RunConf,
    scope: var SemConvertCtx,
  ): SemOrg =

  case node.kind:
    of orgParagraph,
       orgMarkupKinds,
       orgCommandCaption,
       orgLink,
       orgLinkTarget:
      result = newSem(node)
      for sub in items(node):
        result.add toSem(sub, config, scope)

    of orgList:
      result = convertList(node, config, scope)

    of orgCommandName:
      result = newSem(node, newSem(orgIdent, node))

    of orgEmpty, orgRawText:
      result = newSem(node)

    of orgStmtList:
      var commands: seq[OrgNode]

      result = newSem(node)
      for sub in items(node):
        if sub of orgLineCommandKinds:
          if commands.len == 0 or
             (commands.len > 0 and commands.last().line + 1 == sub.line):
            commands.add sub

          else:
            for cmd in commands:
              result.add toSem(cmd, config, scope)

            commands.clear()


        else:
          var tmp = toSem(sub, config, scope)

          if commands.len > 0:
            if sub of orgAssociatedKinds and
               commands.last().line + 1 == sub.line:

              var list = newSem(orgAssocStmtList)
              for cmd in commands:
                list.add toSem(cmd, config, scope)

              tmp.assocList = some list
              commands.clear()

            else:
              for cmd in commands:
                result.add toSem(cmd, config, scope)

              commands.clear()

          result.add tmp

      for cmd in commands:
        result.add toSem(cmd, config, scope)

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

    of orgRadioTarget:
      echov node[0].strVal()
      result = newSem(orgWord, node[0])

    of orgTarget:
      result = newSem(orgWord, node[0])

    of orgSrcCode:
      let lang: string = node["lang"].strVal()
      if lang notin config.codeCreateCallbacks:
        raise newArgumentError(
          &"Cannot create code block for language '{lang}' - " &
            "construction callback is missing from `config.codeCreateCallbacks`")

      else:
        result = newSem(node, newSem(orgEmpty), newSem(orgEmpty))
        result.codeBlock = config.codeCreateCallbacks[lang]()
        result.codeBlock.parseFrom(node, scope)

    else:
      raise newImplementKindError(node, $node.treeRepr())


proc toSem*(node: OrgNode, conf: RunConf): SemOrg =
  toSem(node, conf, asVar initSemConvertCtx())

proc toSemDocument*(node: OrgNode, config: RunConf): SemOrg =
  result = newSem(orgDocument)
  result.add toSem(node, config, asVar initSemConvertCtx())
