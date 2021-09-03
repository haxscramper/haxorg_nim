import
  fusion/matching,
  std/[strutils, macros, tables, strformat, parseutils]

import
  ../defs/[org_types, impl_sem_org, impl_org_node, impl_context]

import
  hmisc/core/all,
  hmisc/types/colorstring


macro unpackNode(node: OrgNode, subnodes: untyped{nkBracket}): untyped =
  result = newStmtList()
  for idx, name in subnodes:
    result.add newVarStmt(name, nnkBracketExpr.newtree(node, newLit(idx)))

proc toSem*(
    node: OrgNode, config: RunConf, scope: var SemOrgCtx): SemOrg

proc convertSubtree*(node: OrgNode, config: RunConf, scope: var SemOrgCtx): Subtree =
  node.unpackNode(
    [prefix, todo, urgency, title, completion, tags, times, drawers, body])
  result = Subtree()

  result.level = prefix.strVal().count('*')

proc convertList*(
    node: OrgNode, config: RunConf, scope: var SemOrgCtx): SemOrg =
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


proc convertLink*(
    node: OrgNode, config: RunConf, scope: var SemOrgCtx): SemOrg =

  result = newSem(node)

  var link: OrgLink
  let target: string = node[0].strVal()

  case node.subKind:
    of oskLinkCallout:
      link = OrgLink(kind: olkCallout, targetName: target[1 .. ^2])

    of oskLinkSubtree:
      link = OrgLink(kind: olkSubtree, targetName:
        target[target.skipWhile({'*', ' '}) .. ^1])

    of oskLinkImplicit:
      link = OrgLink(kind: olkImplicit, targetName: target)

    else:
      raise newImplementKindError(node.subKind, $node.treeRepr())


  # Link target is resolved if possible
  result.link = link


  # Link description is converted as-is
  result.add toSem(node[1], config, scope)
  result.link.anchor = scope.getTarget(result)


proc toSem*(
    node: OrgNode,
    config: RunConf,
    scope: var SemOrgCtx,
  ): SemOrg =

  case node.kind:
    of orgParagraph,
       orgMarkupKinds,
       orgCommandCaption,
       orgLinkTarget:
      result = newSem(node)
      for sub in items(node):
        result.add toSem(sub, config, scope)

    of orgLink:
      result = convertLink(node, config, scope)

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


proc toSem*(node: OrgNode, conf: RunConf): (SemOrg, SemOrgCtx) =
  result[1] = initSemOrgCtx()
  result[0] = toSem(node, conf, result[1])

proc toSemDocument*(node: OrgNode, conf: RunConf): (SemOrg, SemOrgCtx) =
  let (main, ctx) = toSem(node, conf)
  result[0] = newSem(orgDocument)
  result[0].add main
  result[1] = ctx
