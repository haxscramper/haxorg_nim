
import hmisc/core/all

importx:
  ./org_types
  ./impl_org_node

  std/[sequtils, tables]

  hmisc/[
    macros/ast_spec,
    other/hpprint,
    types/colorstring,
    algo/[clformat, hstring_algo, tree/tree_selector]
  ]

export clformat

const
  semOrgSpec* = astSpec(SemOrg, OrgNodeKind):
    orgSubtree:
      0 as "title": orgParagraph
      1 as "body": orgStmtList or orgEmpty

    orgSrcCode:
      0 as "compile-result": orgEmpty
      1 as "eval-result": orgEmpty

    orgListItem:
      0 as "tag"
      1 as "header"
      2 as "body"

    orgTable:
      0 .. ^1 as "rows":
        orgTableRow

    orgTableRow:
      0 as "text": orgParagraph or orgEmpty
      1 as "body":
        0 .. ^1:
          orgTableCell

    orgTableCell:
      0 as "text":
        orgParagraph or orgStmtList or orgEmpty


generateFieldEnum(semOrgSpec, "semf")

proc add*(tree: var SemOrg, subtree: SemOrg | seq[SemOrg]) =
  assert tree.kind in orgSubnodeKinds
  tree.subnodes.add subtree

iterator items*(tree: SemOrg): SemOrg =
  for subnode in tree.subnodes:
    yield subnode

iterator pairs*(tree: SemOrg): (int, SemOrg) =
  for idx, subnode in pairs(tree.subnodes):
    yield (idx, subnode)


iterator mitems*(tree: var SemOrg): var SemOrg =
  for subnode in mitems(tree.subnodes):
    yield subnode

proc len*(tree: SemOrg): int = tree.subnodes.len

func isEmptyNode*(tree: SemOrg): bool =
  isNil(tree) or tree.kind == orgEmptyNode

func `?`*(tree: SemOrg): bool = not isEmptyNode(tree)

func hasAssoc*(tree: SemOrg, kinds: set[OrgNodeKind]): bool =
  tree.assocList.isSome() and anyIt(tree.assocList.get(), it of kinds)

func `[]`*(tree: SemOrg, idx: int): SemOrg =
  tree.subnodes[idx]

func `[]`*(tree: SemOrg, name: string): SemOrg =
  getSingleSubnode(semOrgSpec, tree, name)

func `[]`*(tree: SemOrg, field: SemOrgField): SemOrg =
  tree[getSingleSubnodeIdx(semOrgSpec, tree, toName(field))]

func `[]=`*(tree: SemOrg, name: string, other: SemOrg) =
  tree.subnodes[getSingleSubnodeIdx(semOrgSpec, tree, name)] = other

proc newSem*(kind: OrgNodeKind, str: string): SemOrg =
  SemOrg(kind: kind, isGenerated: true, str: str)

proc newSem*(node: OrgNode, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(
    kind: node.kind,
    isGenerated: false,
    node: node,
    subkind: node.subkind,
    subnodes: @subnodes)

proc newSem*(kind: OrgNodeKind, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(kind: kind, isGenerated: true, subnodes: @subnodes)

proc newSem*(kind: OrgNodeKind, base: OrgNode): SemOrg =
  SemOrg(kind: kind, isGenerated: false, node: base)

func getAssoc*(tree: SemOrg, kinds: set[OrgNodeKind]): SemOrg =
  result = newSem(orgStmtList)
  if tree.assocList.isSome():
    for item in tree.assocList.get():
      if item of kinds:
        result.add item

func strVal*(node: SemOrg): string =
  assertKind node, orgTokenKinds
  if node.isGenerated:
    result = node.str

  else:
    result = node.node.strVal()

let semTreeSelector* = initQueryCtx(
  proc(node: SemOrg): SemOrg = newSem(orgStmtList, node),
  proc(node: SemOrg, elem: set[OrgNodeKind]): bool = node.kind in elem,
  proc(node1, node2: SemOrg): bool = node1.kind == node2.kind)



proc treeRepr*(
    node: SemOrg,
    conf: HDisplayOpts = defaultHDisplay,
    rawBlockParams: bool = false
  ): ColoredText =

  coloredResult()

  var pprintConf = pconf(
    ignorePaths =
      matchField("blockArgs") &
        matchType("CliDesc") &
        matchType("SemOrg") &
        matchTypeField("ShellError", "msg") &
        matchTypeField("ShellError", "errstr") &
        matchTypeField("ShellResult", "cmd") &
        matchTypeField("Subtree", "properties")
    ,
    confId = 228
  )

  proc aux(n: SemOrg, level: int, name: Option[string]) =
    addIndent level
    if isNil(n):
      add "<nil>" + fgRed
      return

    let kind = hshow(n.kind)
    add kind

    case n.kind:
      of orgEmpty:
        discard

      of orgTokenKinds - orgEmpty:
        # echov n.kind
        # echov orgTokenKinds - orgEmpty
        # echov n.node.treeRepr()
        # pprint n
        if n.isGenerated or notNil(n.node.text.baseStr):
          add " "
          add hshow(n.strVal().indentBody(kind.len + level * 2))

        else:
          add " "
          add "[empty-base-str-for-non-generated]" + fgRed

      of orgSrcCode:
        add "\n"
        // "Source code block tree repr"
        if isNil(n.codeBlock):
          addIndent level + 1
          add "empty code block" + fgRed

        else:
          add blockPPtree(n.codeBlock, pprintConf).objectTreeRepr(
            pprintConf, level * 2 + 2)
          # add "\n"
          # add pptree(n.codeBlock.execResult, pprintConf).objectTreeRepr(
          #   pprintConf, level * 2 + 2)

          if rawBlockParams:
            add "\n"
            add pptree(n.codeBlock.blockArgs.value, pprintConf).objectTreeRepr(
              pprintConf, level * 2 + 2)


        for idx, sub in pairs(n):
          add "\n"
          aux(sub, level + 1, semOrgSpec.fieldName(n, idx))

      of orgSubtree:
        add "\n"
        add pptree(n.subtree, pprintConf).objectTreeRepr(
          pprintConf, level * 2 + 2)

        add "\n"
        addIndent level + 2
        add "properties:"
        for key, value in pairs(n.subtree.properties):
          add "\n"
          addIndent(level + 3)
          add key.name
          add "("
          add key.subname
          add ") = \n"
          add treeRepr(value).indent((level + 4) * 2)

        for idx, sub in (n):
          add "\n"
          aux(sub, level + 1, semOrgSpec.fieldName(n, idx))



      of orgLink:
        add "\n"
        add pptree(n.link, pprintConf).objectTreeRepr(
          pprintConf, level * 2 + 2)

        for idx, sub in pairs(n):
          add "\n"
          aux(sub, level + 1, semOrgSpec.fieldName(n, idx))

      of orgAttrImg:
        add "\n"
        add pptree(n.property.image, pprintConf).objectTreeRepr(
          pprintConf, level * 2 + 2)

      of orgParagraph:
        proc isMarkupWords(org: SemOrg): bool =
          org.kind in {orgWord} or
          (
            org.kind in {orgBold, orgItalic, orgWord, orgParagraph} and
            allIt(org, isMarkupWords(it))
          )

        proc formatMarkup(n: SemOrg): ColoredText =
          case n.kind:
            of orgWord:
              result.add "W["
              result.add hshow(n.strVal(), hdisplay(flags -= dfUseQuotes))
              result.add "]"

            of orgBold:
              result.add "B["
              for sub in items(n):
                result.add formatMarkup(sub)
              result.add "]"

            else: raise newUnexpectedKindError(n)


        var reslen = 0
        add "\n"
        for idx, word in pairs(n):
          if isMarkupWords(word):
            if idx == 0:
              addIndent(level + 1)

            let formatted = formatMarkup(word)
            add formatted
            reslen += len(formatted)
            if 60 < reslen:
              reslen = 0
              add "\n"
              addIndent(level + 1)

          else:
            aux(word, level + 1, none(string))
            add "\n"
            addIndent(level + 1)


      else:
        for idx, sub in pairs(n):
          add "\n"
          let subErr = validateSub(semOrgSpec, n, idx)
          if subErr.isSome():
             add subErr.get().indent(level * 2 + 2)
             add "\n"

          aux(sub, level + 1, semOrgSpec.fieldName(n, idx))



  aux(node, 0, none(string))
  endResult()

proc joinFlatText*(semOrg: SemOrg): string =
  if semOrg of orgTokenKinds:
    result = semOrg.strVal()

  else:
    for node in items(semOrg):
      result.add joinFlatText(node)

proc getName*(semOrg: SemOrg): string =
  case semOrg.kind:
    of orgSubtree:
      semOrg[semfTitle].joinFlatText()

    of orgParagraph, orgCommandName, orgTokenKinds:
      joinFlatText(semOrg)

    else:
      raise newImplementKindError(semOrg, $semOrg.treeRepr())

proc getTextContext*(sem: SemOrg): OrgTextContext =
  case sem.kind:
    of orgSubtree:
      case sem.subtree.level:
        of 0: otcSubtree0
        of 1: otcSubtree1
        of 2: otcSubtree2
        else: otcSubtreeOther

    of orgBold: otcBold
    of orgMonospace: otcMonospaceInline

    else:
      raise newImplementKindError(sem)
