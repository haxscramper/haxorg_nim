import
  fusion/matching,
  std/[strutils, macros, tables, strformat, parseutils]

import
  ../defs/[org_types, impl_sem_org, impl_org_node, impl_context]

import
  hmisc/core/all,
  hmisc/types/colorstring


proc joinFlatText*(semOrg: SemOrg): string =
  if semOrg of orgTokenKinds:
    result = semOrg.strVal()

  else:
    for node in items(semOrg):
      result.add joinFlatText(node)

proc getName*(semOrg: SemOrg): string =
  case semOrg.kind:
    of orgSubtree:
      semOrg["header"].joinFlatText()

    of orgParagraph, orgCommandName, orgTokenKinds:
      joinFlatText(semOrg)

    else:
      raise newImplementKindError(semOrg, $semOrg.treeRepr())

proc toSem*(
  node: OrgNode, config: RunConf, ctx: var SemOrgCtx): SemOrg

proc clearAssociative*(ctx: var SemOrgCtx) =
  ctx.associative.clear()

proc flushAssociative*(ctx: var SemOrgCtx, config: RunConf): seq[SemOrg] =
  for cmd in ctx.associative:
    result.add toSem(cmd, config, ctx)

  ctx.clearAssociative()


proc popAssociativeList*(ctx: var SemOrgCtx, config: RunConf): Option[SemOrg] =
  if ctx.associative.len > 0:
    var list = newSem(orgAssocStmtList)
    for cmd in ctx.associative:
      list.add toSem(cmd, config, ctx)

    ctx.associative.clear()
    return some list

macro unpackNode(node: OrgNode, subnodes: untyped{nkBracket}): untyped =
  result = newStmtList()
  for idx, name in subnodes:
    result.add newVarStmt(name, nnkBracketExpr.newtree(node, newLit(idx)))

proc convertSubtree*(
    node: OrgNode, config: RunConf, ctx: var SemOrgCtx): SemOrg =

  node.unpackNode(
    [prefix, todo, urgency, title, completion, tags, times, drawers, body])
  var tree = Subtree()


  tree.level = prefix.strVal().count('*')

  let semTitle = toSem(node["title"], config, ctx)
  result = newSem(node, semTitle, toSem(node["body"], config, ctx))
  ctx[symSubtree, semTitle.getName()] = result
  result.subtree = tree


proc convertList*(
    node: OrgNode, config: RunConf, ctx: var SemOrgCtx): SemOrg =
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

    outItem.add toSem(tag, config, ctx)
    outItem.add toSem(header, config, ctx)
    outItem.add toSem(body, config, ctx)

    result.add outItem

  result.subKind = listKind


proc convertLink*(
    node: OrgNode, config: RunConf, ctx: var SemOrgCtx): SemOrg =

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
  result.add toSem(node[1], config, ctx)
  result.link.anchor = ctx.getTarget(result)


proc convertSrcCode*(
    node: OrgNode, config: RunConf, ctx: var SemOrgCtx): SemOrg =

  let lang: string = node["lang"].strVal()
  if lang notin config.codeCreateCallbacks:
    raise newArgumentError(
      &"Cannot create code block for language '{lang}' - " &
        "construction callback is missing from `config.codeCreateCallbacks`")

  else:
    result = newSem(node)
    result.assocList = ctx.popAssociativeList(config)

    if result.hasAssoc({orgCommandName}):
      ctx[symNamed, result.getAssoc({orgCommandName})[
        0 #[ QUESTION how to handle multiple #+name annotations? ]#].getName()] = result

    var body = newSem(node["body"])

    for line in node["body"]:
      var newLine = newSem(line)
      for item in line:
        let newItem = newSem(item)
        if item.kind == orgCodeCallout:
          ctx[symCalloutTarget, newItem.getName()] = newItem

        newLine.add newItem

      body.add newLine

    result.add body
    result.add newSem(orgEmpty)

    result.codeBlock = config.codeCreateCallbacks[lang]()
    result.codeBlock.parseFrom(node, ctx)


proc toSem*(
    node: OrgNode,
    config: RunConf,
    ctx: var SemOrgCtx,
  ): SemOrg =

  case node.kind:
    of orgParagraph,
       orgMarkupKinds,
       orgCommandCaption,
       orgLinkTarget:
      result = newSem(node)
      for sub in items(node):
        result.add toSem(sub, config, ctx)

    of orgLink:
      result = convertLink(node, config, ctx)

    of orgList:
      result = convertList(node, config, ctx)

    of orgCommandName:
      result = newSem(node, newSem(orgIdent, node[0]))

    of orgEmpty, orgRawText:
      result = newSem(node)

    of orgStmtList:
      result = newSem(node)
      for sub in items(node):
        if sub of orgLineCommandKinds:
          if ctx.associative.len == 0 or
             (ctx.associative.len > 0 and
              ctx.associative.last().line + 1 == sub.line):
            ctx.associative.add sub

          else:
            result.add ctx.flushAssociative(config)

        else:
          if sub of orgAssociatedKinds and
             ctx.associative.last().line + 1 == sub.line:

            result.add toSem(sub, config, ctx)
            assert ctx.associative.len == 0,
              &"Conversion of the {sub.kind} did not clear associative list"

          else:
            result.add ctx.flushAssociative(config)
            result.add toSem(sub, config, ctx)

      for cmd in ctx.associative:
        result.add toSem(cmd, config, ctx)

    of orgSubtree:
      result = convertSubtree(node, config, ctx)

    of orgWord, orgBigIdent:
      let str = node.strVal()
      var word = newSem(node)
      if node of orgBigIdent:
        word.bigIdentKind = parseEnum[OrgBigIdentKind](str, obiNone)

      if str in ctx[symRadioTarget]:
        result = newSem(orgLink)
        result.link = OrgLink(
          anchor: some initAnchor(result),
          kind: olkRadioLink,
          targetName: str)

        result.add word

      else:
        result = word


    of orgRadioTarget:
      result = newSem(orgWord, node[0])
      ctx[symRadioTarget, result.getName()] = result

    of orgTarget:
      result = newSem(orgWord, node[0])
      ctx[symRegularTarget, result.getName()] = result

    of orgSrcCode:
      result = convertSrcCode(node, config, ctx)

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
