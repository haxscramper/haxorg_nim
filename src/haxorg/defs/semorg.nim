{.experimental: "caseStmtMacros".}

import ast, buf
import std/[options, tables, strutils, strformat, uri,
            hashes, enumerate, sequtils]

import hpprint, hpprint/hpprint_repr
import hmisc/other/hshell
import hmisc/other/oswrap
import hmisc/types/colorstring
import hmisc/algo/[hlex_base, hparse_base, htree_mapping, clformat]
import hmisc/core/all
import nimtraits


import fusion/matching






var defaultRunConf*: RunConf

proc subKind*(semorg: SemOrg): OrgNodeSubKind = semorg.node.subkind

proc register*(lang: string, codeBuilder: CodeBuilder) =
  defaultRunConf.codeCreateCallbacks[lang] = codeBuilder


proc parseBaseBlockArgs(cb: CodeBlock, cmdArguments: OrgNode) =
  assertKind(cmdArguments, {onkEmptyNode, onkCmdArguments})
  if cmdArguments.kind == onkEmptyNode:
    return

  for arg in cmdArguments["args"]:
    let value: StrSlice = arg["value"].text
    case $arg["name"].text:
      of "session":
        cb.evalSession = some($value)

      of "exports":
        for entry in slices(split(value, ' '), value):
          case $entry:
            of "both":     cb.resExports = creBoth
            of "code":     cb.resExports = creCode
            of "results":  cb.resExports = creResults
            of "none":     cb.resExports = creNone

            of "drawer":   cb.resFormat = crtDrawer
            of "html":     cb.resFormat = crtHtml
            of "latex":    cb.resFormat = crtLatex
            of "link":     cb.resFormat = crtLink
            of "graphics": cb.resFormat = crtGraphics
            of "org":      cb.resFormat = crtOrg
            of "pp":       cb.resFormat = crtPP
            of "raw":      cb.resFormat = crtRaw

            of "output":   cb.resCollection = crcOutput
            of "value":    cb.resCollection = crcValue
            of "value-type":
              cb.resCollection = crcValueType


            of "replace":  cb.resHandling = crtReplace
            of "silent":   cb.resHandling = crtSilent
            of "append":   cb.resHandling = crtAppend
            of "prepend":  cb.resHandling = crtPrepend

            else:
              raise newUnexpectedString(
                entry,
                "Unexpected export specification",
                [
                  "both", "code", "results", "none",

                  "drawer", "html", "latex", "link", "graphics", "org",
                  "pp", "raw",

                  "output", "value",

                  "replace", "silent", "append", "prepend",
                ]
              )

      of "eval":
        case $value:
          of "never":
            cb.evalWhen = cewNever

          of "noexport", "never-export", "no-export":
            cb.evalWhen = cewNeverExport

          of "query":
            cb.evalWhen = cewQuery

          of "query-export":
            cb.evalWhen = cewQuery

          else:
            raise newUnexpectedString(
              value,
              "Unexpected export specification",
              ["never", "noexport", "never-export", "no-export", "query",
               "query-export"
              ]
            )




proc parseBaseBlock*(cb: CodeBlock, semorg: SemOrg, scope: seq[TreeScope]) =
  for entry in scope:
    for drawer in entry.tree.node["drawers"]:
      if drawer["name"].text == "properties":
        for prop in drawer["body"]:
          if prop["name"].text == "header-args" and
             prop["subname"].text == cb.langName:

            parseBaseBlockArgs(cb, prop["values"])

  parseBaseBlockArgs(cb, semorg.node["header-args"])

  cb.code = $semorg.node["body"].text

proc updateContext*(codeBlock: CodeBlock, context: var CodeRunContext) =
  if codeBlock.evalSession.isSome():
    context.prevBlocks.mgetOrPut(
      codeBlock.evalSession.get(), newSeq[CodeBlock]()).add codeBlock

proc getSameSession*(
    codeBlock: CodeBlock, context: CodeRunContext): seq[CodeBlock] =

  if codeBlock.evalSession.isSome():
    context.prevBlocks.getOrDefault(
      codeBlock.evalSession.get(), @[codeBlock])

  else:
    @[codeBlock]


method runCode*(
    codeBlock: CodeBlock,
    context: var CodeRunContext
  ) {.base.} =

  raiseAssert("#[ IMPLEMENT ]#")

method parseFrom*(
  codeBlock: CodeBlock, semorg: SemOrg, scope: seq[TreeScope]) {.base.} =
  ## Parse code block body from semorg node. This method is called from
  ## top-level convert dispatcher loop using
  ## `parseFrom(semorg.codeBloc,semorg)` to trigger runtime dispatch.
  ## Overrides for this method can set only `codeBlock` argument, or modify
  ## `semorg` too, it doesn't really matter.
  raiseAssert("#[ IMPLEMENT ]#")



method parseFrom*(
  codeBlock: DefaultCodeBlock, semorg: SemOrg, scope: seq[TreeScope]) =
  parseBaseBlock(CodeBlock(codeBlock), semorg, scope)


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


proc `not`*[K](s: set[K]): set[K] = ({ low(K) .. high(K) } - s)

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

proc runCodeBlocks*(
    tree: var SemOrg,
    config: RunConf = defaultRunConf
  ) =
  ## Execute all code blocks in `SemOrg`.
  proc aux(tree: var SemOrg, context: var CodeRunContext) =
    case tree.kind:
      of orgTokenKinds:
        discard

      of orgSubnodeKinds:
        if tree.kind == onkSrcCode:
          runCode(tree.codeBlock, context)

        else:
          for subnode in mitems(tree.subnodes):
            aux(subnode, context)

      else:
        # Noweb and snippet nodes should have been already untangled and
        # replaced with regular semorg entries.
        raiseAssert("#[ IMPLEMENT ]#")

  var context: CodeRunContext
  aux(tree, context)


proc objTreeRepr*(tag: SemMetaTag, colored: bool = true): ObjTree =
  result = pptObj($typeof(tag))
  for name, field in fieldPairs(tag[]):
    result.fldPairs.add((name, objTreeRepr(field)))

proc objTreeRepr*(
  node: SemOrg, colored: bool = true, name: string = "<<fail>>"): ObjTree =
  let name = tern(
    name != "<<fail>>", &"({toGreen(name, colored)}) ", "")

  if node.isNil:
    return pptConst($(name & toBlue("<nil>", colored)))


  var subname = ""
  if not node.isGenerated:
    if node.node.subKind != oskNone:
      subname = &"[{toMagenta($node.node.subKind, colored)}]"

  case node.kind:
    of onkIdent:
      return pptConst(
        &"{name}{toItalic($node.kind, colored)} " &
        &"{subname} {toCyan($node.node.text, colored)}")

    of orgTokenKinds - {onkIdent, onkMarkup}:
      let txt = if node.isGenerated: node.str else: $node.node.text
      if '\n' in txt:
        result = pptObj(
          &"{name}{toItalic($node.kind, colored)}{subname}" &
            &"\n\"\"\"\n{toYellow(txt, colored)}\n\"\"\"")

      else:
        result = pptObj(
          &"{name}{toItalic($node.kind, colored)} " &
          &"{subname} \"{toYellow(txt, colored)}\"")

    of onkNowebMultilineBlock:
      raiseImplementError("")

    of onkSnippetMultilineBlock:
      raiseImplementError("")

    of onkSrcCode:
      result = pptConst($node.node.str)

    else:
      let mark = tern(
        not node.isGenerated and node.node.str.len > 0,
        &" <{toBlue(node.node.str, colored)}>",
        ""
      )


      var subnodes: seq[ObjTree]

      for name, value in fieldPairs(node[]):
        when name notin [
          "node", "subnodes", "symTable", "isGenerated", "kind",
          "properties"
        ]:

          when value is Option:
            if value.isSome():
              subnodes.add pptObj(
                $name.toRed(colored).wrap("()"),
                objTreeRepr(value.get(), colored = colored)
              )

          else:
            subnodes.add pptObj(
              $name.toRed(colored).wrap("()"),
              objTreeRepr(value, colored = colored)
            )

      for idx, subnode in enumerate(items(node)):
        subnodes.add objTreeRepr(
          subnode, colored, getSubnodeName(node.kind, idx))

      result = pptObj(
        &"{name}{toItalic($node.kind, colored)} {subname}{mark}",
        initStyle(), subnodes)


proc treeRepr*(tree: SemOrg, colored: bool = true): string =
  objTreeRepr(tree, colored = colored).treeRepr()
