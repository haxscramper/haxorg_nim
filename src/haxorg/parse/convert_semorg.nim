import
  fusion/matching,
  std/[strutils, macros, tables, strformat, parseutils, strtabs]

import
  ../defs/[
    org_types, impl_sem_org, impl_org_types,
    impl_org_node, impl_context, impl_org_params]

import
  hmisc/core/all,
  hmisc/types/colorstring,
  hmisc/other/[oswrap, hpprint],
  hmisc/algo/hlex_base


proc toSem*(
  node: OrgNode, config: OrgConf, ctx: var SemOrgCtx): SemOrg

proc clearAssociative*(ctx: var SemOrgCtx) =
  ctx.associative.clear()

proc flushAssociative*(ctx: var SemOrgCtx, config: OrgConf): seq[SemOrg] =
  for cmd in ctx.associative:
    result.add toSem(cmd, config, ctx)

  ctx.clearAssociative()


proc popAssociativeList*(ctx: var SemOrgCtx, config: OrgConf): Option[SemOrg] =
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
    node: OrgNode, config: OrgConf, ctx: var SemOrgCtx): SemOrg =

  node.unpackNode(
    [prefix, todo, urgency, title, completion, tags, times, drawers, body])
  var tree = Subtree()

  tree.level = prefix.strVal().count('*')

  if not(drawers["properties"] of orgEmpty):
    for prop in drawers["properties"]:
      prop.unpackNode([name, subname, values])
      let nameStr = name.strVal().strip(chars = {':'})
      let subnameStr = subname.strVal().strip(chars = {':'})
      tree.properties[(nameStr, subnameStr)] = values


  let semTitle = toSem(node["title"], config, ctx)
  result = newSem(node, semTitle)
  result.subtree = tree

  ctx.scope.add TreeScope(tree: result)
  let semBody = toSem(node["body"], config, ctx)
  result.add semBody
  discard ctx.scope.pop

  ctx[symSubtree, semTitle.getName()] = result




proc convertList*(
    node: OrgNode, config: OrgConf, ctx: var SemOrgCtx): SemOrg =
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


proc newArgParser*(blockName: string): CliApp =
  newCliApp(blockName, (0,0,0), "", "", noDefault = cliNoDefaultOpts)


proc convertAttrProperty*(node: OrgNode, confifg: OrgConf, ctx: SemOrgCtx): OrgProperty =
  case node.kind:
    of orgAttrImg:
      var spec = OrgImageSpec()
      var app = newArgParser("attr_img")
      app.add opt(":w", "Image width", check = cliCheckFor(string))
      app.add opt(":h", "Image height", check = cliCheckFor(string))


      if app.parseArgs(node[0][orgfArgs][0..^1]):
        for name, opt in app.getRootCmd().options:
          case name:
            of ":w": spec.width = some parseOrgDimensions(opt.strVal)
            of ":h": spec.height = some parseOrgDimensions(opt.strVal)
            else: raise newUnexpectedKindError(name)

      result = OrgProperty(kind: opkAttrImg, image: spec)

    else:
      raise newUnexpectedKindError(node)



proc convertLink*(
    node: OrgNode, config: OrgConf, ctx: var SemOrgCtx): SemOrg =

  result = newSem(node)

  if ?ctx.fixSize.width or ?ctx.fixSize.height:
    result.properties.add OrgProperty(
      kind: opkColumnSpec,
      cellSpec:(
        width: ctx.fixSize.width,
        height: ctx.fixSize.height))

  if ctx.isInToplevel(node):
    result.properties.add OrgProperty(kind: opkToplevel)

  var link: OrgLink
  let target: string = node[orgfLink].strVal()
  case node.subKind:
    of oskLinkCallout:
      link = OrgLink(kind: olkCallout, targetName: target[1 .. ^2])

    of oskLinkSubtree:
      link = OrgLink(kind: olkSubtree, targetName:
        target[target.skipWhile({'*', ' '}) .. ^1])

    of oskLinkImplicit:
      link = OrgLink(kind: olkImplicit, targetName: target)

    of oskLinkFile:
      var res: OrgFile =
        if target[0] == '.':
          OrgFile(
            isRelative: true,
            relTo: ctx.fileStack.top(),
            relFile: RelFile(target[1..^1]))

        else:
          OrgFile(
            isRelative: false,
            absFile: AbsFile(target))

      res.category =
        case RelFile(target).ext():
          of "png", "jpg", "jpeg", "bmp":
            ofcBitmapImage

          else:
            ofcText

      while ?ctx.associative:
        let p = ctx.associative.pop().convertAttrProperty(config, ctx)
        case p.kind:
          of opkAttrImg:
            assertKind(res.category, {ofcBitmapImage})

          else:
            raise newImplementKindError(p)

        result.properties.add p

      link = OrgLink(kind: olkFile, linkFile: res)

    else:
      raise newImplementKindError(node.subKind, $node.treeRepr())


  # Link target is resolved if possible
  result.link = link


  # Link description is converted as-is
  result.add toSem(node[orgfDesc], config, ctx)
  result.link.anchor = ctx.getTarget(result)


proc convertSrcCode*(
    node: OrgNode, config: OrgConf, ctx: var SemOrgCtx): SemOrg =

  let lang: string = node["lang"].strVal()
  if lang notin config.codeCreateCallbacks:
    raise newArgumentError(
      &"Cannot create code block for language '{lang}' - " &
        "construction callback is missing from `config.codeCreateCallbacks`")

  else:
    result = newSem(node)
    result.assocList = ctx.popAssociativeList(config)

    if result.hasAssoc({orgCommandName}):
      ctx[symNamed, result.getAssoc({orgCommandName})[0].getName()] = result

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

proc mergeHeaderArgs(sem: OrgNode, ctx: SemOrgCtx): seq[OrgNode] =
  for arg in sem[orgfArgs][orgfArgs]:
    result.add arg

  for header in ctx.associative:
    if header of orgCommandHeader:
      for arg in header[orgfArgs][orgfArgs]:
        result.add arg

proc parseTableDimensions*(str: string):
  tuple[
    separators: seq[OrgCellSeparator],
    aligns: seq[
      tuple[
        vdir: OrgVerticalDirection,
        hdir: OrgHorizontalDirection,
        dim: Option[OrgDimensions]
      ]]] =
  # IMPLEMENT multiple dimensions specified
  # IMPLEMENT interrow (intercolumn) separator specs for `||`, `|`, `,` etc.
  # IMPLEMENT intercol placeholder to allow for `_|_|_|_`

  var str = initPosStr(str)
  if str["||"]:  result.separators.add ocsDoubleLine; str.next(2)
  elif str["|"]: result.separators.add ocsSingleLine; str.next(1)
  else:          result.separators.add ocsNone

  while ?str:
    var vdir = ovdNone
    var hdir = ohdNone
    var dim: Option[OrgDimensions]
    var isSep: bool = false
    case str[]:
      of 'l': hdir = ohdLeft; str.next()
      of 'r': hdir = ohdRight; str.next()
      of 'c': hdir = ohdCenter; str.next()
      of 'm': vdir = ovdCenter; str.next()
      of 't': vdir = ovdTop; str.next()
      of 'b': vdir = ovdBottom; str.next()
      of '0' .. '9':
        let size = str.asSlice():
          str.skipWhile({'0' .. '9'})
          str.skipWhile(IdentChars)

        dim = some parseOrgDimensions(size.strVal())

      of ',':
        result.separators.add ocsNone
        str.next()
        isSep = true

      of '|':
        if str["||"]:
          result.separators.add ocsDoubleLine
          str.next(2)

        else:
          result.separators.add ocsSingleLine
          str.next(1)

        isSep = true

      of '_':
        str.next()

      else:
        raise newUnexpectedCharError(str)

    if not isSep:
      result.aligns.add((vdir, hdir, dim))




proc convertTable*(
    node: OrgNode, config: OrgConf, ctx: var SemOrgCtx): SemOrg =

  result = newSem(node)
  result.table = OrgTable()

  var vdirs: seq[OrgVerticalDirection]
  var hdirs: seq[OrgHorizontalDirection]

  block table_parameters:
    var app = newArgParser("begin_table")
    app.add opt(
      ":format", "Column alignment and placement formatting",
      check = cliCheckFor(string))

    if app.parseArgs(mergeHeaderArgs(node, ctx)):
      for name, opt in app.getOptions():
        case name:
          of ":format":
            let (separators, dims) = parseTableDimensions(opt as string)
            result.table.intercols = separators

            for (vdir, hdir, dim) in dims:
              result.table.widths.add dim
              vdirs.add vdir
              hdirs.add hdir


          else:
            raise newUnexpectedKindError(name)

  block row_parsing:
    var colcount = 0
    for rowidx, row in node[1 .. ^1]:
      var semrow = OrgRow()

      for idx, col in row[2 .. ^1]:
        var semcol = OrgCell(
          vertical: vdirs.getOr(idx),
          horizontal: hdirs.getOr(idx),
          width: result.table.widths.getOr(idx),
        )

        semcol.borders.top = tern(
          rowidx == 0,
          result.table.toprow,
          result.table.rows[rowidx - 1].afterrow)

        semcol.borders.bottom = semrow.afterrow
        semcol.borders.left = result.table.intercols.getOr(idx)
        semcol.borders.right = result.table.intercols.getOr(idx + 1)



        block:
          ctx.fixSize.width = semcol.width

          semcol.body = toSem(col[orgfText], config, ctx)

          ctx.fixSize.width.clear()

        semrow.cells.add semcol

      result.table.rows.add semrow
      colcount = max(len(semrow.cells), colcount)

    result.table.columnCount = colcount




proc convertAttr*(node: OrgNode, config: OrgConf, ctx: SemOrgCtx): SemOrg =
  result = newSem(node)
  result.property = convertAttrProperty(node, config, ctx)


import hmisc/algo/clformat

proc parseOrgValue(item: string): tuple[name: string, value: OrgValue] =
  var str = initPosStr(item)
  let pos = str.getPos()
  let name = str.asSlice str.skipWhile(IdentChars)
  if str.trySkip('='):
    result.name = strVal(name)
    if str.trySkip('('):
      let id = str.popIdent()
      case id:
        of "org":
          str.skip(' ')
          str.space()

          let val =
            if str['"']:
              str.asSlice(str.skipStringLit(), +1, -2)

            else:
              str.asSlice(str.skipTo(')'))


          result.value = OrgValue(kind: ovkString, strVal: strVal(val))
          str.skip(')')

        else:
          raise newUnexpectedKindError(id)

    else:
      result.value = OrgValue(
        kind: ovkString,
        strVal: strVal(str.asSlice(str.skipToEol()))
      )

  else:
    str.setPos(pos)
    result.name = strVal(name)
    result.value = OrgValue(kind: ovkString, strVal: strVal(name))

proc interpolatePath*(conf: OrgConf, path: string, ctx: SemOrgCtx): OrgFile =
  let env = newStringTable({
    "template": $conf.templateDir
  })

  let path = path % env

  if path[0] == '.':
    result = OrgFile(
      isRelative: true,
      relTo: ctx.fileStack.top(),
      relFile: RelFile(path)
    )

  else:
    result = OrgFile(isRelative: false, absFile: AbsFile(path))

  echov result


proc convertInclude*(
    node: OrgNode,
    conf: OrgConf,
    ctx: var SemOrgCtx,
  ): SemOrg =

  result = newSem(node)

  var spec =
    if node[orgfKind] of orgEmpty:
      OrgInclude(kind: oikOrgInclude)

    else:
      case node[orgfKind].strVal():
        of "export": OrgInclude(kind: oikExportInclude)
        of "src": OrgInclude(kind: oikSrcInclude)
        else: raise newUnexpectedKindError(node[orgfKind].strVal())

  spec.file = conf.interpolatePath(node[orgfFile].strVal(), ctx)

  case spec.kind:
    of oikExportInclude:
      spec.backend = node[orgfLang].strVal().split(",")

    of oikSrcInclude:
      spec.langname = node[orgfLang].strVal()

    else:
      discard

  var app = newArgParser("include")
  app.add opt(":var", "Parametrized include",
              check = cliCheckFor(seq[string]))

  app.add opt(":page", "Add new page after include",
              check = cliCheckFor(string) #[ HACK use `checkFor(bool)` ]#)

  if app.parseArgs(mergeHeaderArgs(node, ctx)):
    for name, opt in app.getOptions():
      case name:
        of ":var":
          for item in opt as seq[string]:
            let (varname, value) = parseOrgValue(item)
            spec.vars[varname] = value

        of ":page":
          case opt as string: # REFACTOR use `as bool` & lexcast
            of "t":
              spec.newpage = true

            of "nil":
              spec.newpage = false

            else:
              raise newUnexpectedKindError(opt as string)

        else:
          raise newUnexpectedKindError(name, opt.treeRepr())


  result.includeSpec = spec

  while ?ctx.associative:
    let prop = ctx.associative.pop()
    case prop.kind:
      of orgCommandHeader: discard
      else: raise newUnexpectedKindError(prop)

proc toSem*(
    node: OrgNode,
    config: OrgConf,
    ctx: var SemOrgCtx,
  ): SemOrg =
  ctx.kindStack.add node.kind
  case node.kind:
    of orgDoclevelKinds:
      result = newSem(orgEmptyNode, node)

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

    of orgCommandInclude:
      result = convertInclude(node, config, ctx)

    of orgStmtList:
      result = newSem(node)
      for sub in items(node):
        if (sub of orgAssociatedKinds and ?ctx.associative) and
           ctx.associative.last().line + 1 == sub.line:

          # Org-mode node node can have associated comments. Expecting
          # recursive conversion to semorg to correctly clear required
          # context lists.
          result.add toSem(sub, config, ctx)
          if ?ctx.associative:
            raise newLogicError(
              &"Conversion of the {sub.kind} did not clear associative list")

        elif sub of orgLineCommandKinds:
          if ctx.associative.len == 0 or
             (ctx.associative.len > 0 and
              ctx.associative.last().line + 1 == sub.line):
            ctx.associative.add sub

          else:
            result.add ctx.flushAssociative(config)

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

    of orgNewline:
      result = newSem(node)

    of orgRadioTarget:
      result = newSem(orgWord, node[0])
      ctx[symRadioTarget, result.getName()] = result

    of orgTarget:
      result = newSem(orgWord, node[0])
      ctx[symRegularTarget, result.getName()] = result

    of orgSrcCode:
      result = convertSrcCode(node, config, ctx)

    of orgTable:
      result = convertTable(node, config, ctx)

    of orgAttrImg:
      result = convertAttr(node, config, ctx)

    else:
      raise newImplementKindError(
        node, $node.treeRepr(hdisplay(flags += dfWithRanges)))

  assert ctx.kindStack.pop() == node.kind


proc toSem*(node: OrgNode, file: AbsFile, conf: OrgConf): (SemOrg, SemOrgCtx) =
  result[1] = initSemOrgCtx(file)
  result[0] = toSem(node, conf, result[1])

proc semDocument*(node: OrgNode, conf: OrgConf): OrgDocument =
  proc aux(node: OrgNode, doc: var OrgDocument) =
    case node.kind:
      of orgCommandOptions:
        var idx = 0
        let args = node[orgfArgs][orgfArgs]
        while idx < len(args):
          let (key, val) = (args[idx].strVal(), args[idx + 1].strVal())
          case key:
            of "toc":
              if val == "t":
                raise newImplementError()

              else:
                doc.tocMax = some parseInt(val)

            of "pagenum":
              if val == "t":
                doc.pagenum = some 1

              else:
                doc.pagenum = some parseInt(val)


            else:
              raise newImplementKindError(key)

          inc idx, 2

      of orgSubnodeKinds - orgDoclevelKinds:
        for sub in items(node):
          aux(sub, doc)

      else:
        discard



  aux(node, result)

proc toSemDocument*(node: OrgNode, file: AbsFile, conf: OrgConf): (SemOrg, SemOrgCtx) =
  let (main, ctx) = toSem(node, file, conf)
  result[0] = newSem(orgDocument, main)
  result[0].document = semDocument(node, conf)
  result[1] = ctx
