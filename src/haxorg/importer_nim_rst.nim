## Convert nim RST to org-mode

import packages/docutils/[rst, rstast]
import hmisc/core/all
import hmisc/other/hpprint
import hmisc/other/oswrap
import std/[streams, strutils]

import ./defs/defs_all


proc parseRstString*(s: string, inputPath: AbsFile = AbsFile("")): PRstNode =
  const filen = "input"
  var dummyHasToc = false
  let findFile = proc(path: string): string =
    echo "find file", path

  result = rstParse(
    text     = s,
    filename = filen,
    line     = 0,
    column   = 1,
    options  = {},
    findFile = findFile,
    msgHandler = (
      proc(filename: string, line, col: int,
           msgkind: MsgKind, arg: string) =

        let mc = msgkind.whichMsgClass
        if mc == mcError:
          let a = $msgkind % arg
          var message: string
          message.add " $1: $2" % [$mc, a]
          raise newException(EParseError, message)
    )
  ).node

iterator items*(rst: PRstNode): PRstNode =
  for n in rst.sons:
    yield n

proc `[]`*(rst: PRstNode, idx: int): PRstNode = rst.sons[idx]

proc bodyText*(rst: PRstNode): string =
  result &= rst.text
  for node in rst:
    result &= node.bodyText()

proc impl(rst: PRstNode, level: int): OrgNode =
  case rst.kind:
    of rnInner:
      var oneParagraph = true
      if level == 0:
        for node in rst:
          if node.kind notin { rnSub .. rnLeaf }:
            oneParagraph = false

      if oneParagraph:
        result = orgParagraph.newTree()

      else:
        result = orgStmtList.newTree()

      for node in rst:
        result.add impl(node, level + 1)

    of rnFieldBody, rnParagraph:
      result = orgParagraph.newTree()
      for node in rst:
        result.add impl(node, level + 1)

    of rnLeaf:
      result = orgWord.newTree(rst.text)

    of rnOverline:
      # FIXME nim rst parser does not build explicit document structure and
      # instead only has 'levels' for heading, which means I have to
      # manually reconstruct this.
      result = orgSubtree.newTree()
      for node in rst:
        result.add impl(node, level + 1)

    of rnSubstitutionReferences:
      result = orgMacro.newTree(rst.bodyText())

    of rnFieldList:
      result = newOStmtList()
      for node in rst:
        let name = node[0]
        let value = node[1]

        result.add orgCommand.newTree(
          orgIdent.newTree(name[0].text),
          value.impl(level + 1)
        )



    of rnInlineLiteral, rnInterpretedText: # FIXME differentiate between these two
      var buf: string
      for node in rst:
        buf &= node.text

      result = newTree(orgMonospace, orgRawText.newTree(buf))

    of rnStrongEmphasis:
      result = newTree(orgBold)
      for node in rst:
        result.add impl(node, level + 1)

    of rnEmphasis:
      result = newTree(orgItalic)
      for node in rst:
        result.add impl(node, level + 1)

    of rnContents:
      result = orgCommand.newTree("toc")

    of rnCodeBlock:
      result = orgSrcCode.newTree()
      result.add orgIdent.newTree(rst[0].bodyText)
      result.add newEmptyNode()
      result.add orgRawText.newTree(rst[2].bodyText)

    of rnOptionList, rnOptionListItem, rnOptionGroup, rnDescription,
       rnDefName, rnDefBody, rnLineBlockItem:
      # nim RST parser is broken for inline `--flag` options on 1.4.6.
      # https://nim-lang.org/docs/system.html#disarm.t%2Ctyped and
      # `DefList` works very strangely as well, so just hack-fix for now.
      # Devel seems to work fine though.

      result = orgParagraph.newTree()
      for node in rst:
        result.add impl(node, level + 1)


    of rnBlockQuote:
      var buf: string
      for sub in rst[0]:
        buf.add sub.text

      result = orgMultilineCommand.newTree(
        orgIdent.newTree("quote"),
        orgRawText.newTree(buf)
      )

    of rnHyperlink:
      result = orgLink.newTree(
        rst[1].impl(level + 1),
        rst[0].impl(level + 1)
      )

    of rnStandaloneHyperLink:
      result = orgLink.newTree(
        rst[0].impl(level + 1),
        newEmptyNode()
      )

    of rnRef:
      # `ref:` link, TODO add support for customizing target link formats
      # NOTE first encountered in `nim-1.4.6/lib/pure/times.nim`
      result = orgLink.newTree(
        rst[0].impl(level + 1),
        newEmptyNode()
      )

    of rnLiteralBlock:
      # NOTE First encountered in `nim-1.4.6/lib/pure/os.nim`
      var buf: string
      for sub in rst[0]:
        buf.add sub.text

      result = orgMultilineCommand.newTree(
        orgIdent.newTree("example"),
        orgRawText.newTree(buf)
      )

    of rnIdx:
      # IMPLEMENT index relations
      # QUESTION how is it different from `Ref`?
      result = newEmptyNode()

    of rnTable:
      # IMPLEMENT org-mode tables structure
      result = newEmptyNode()

    of rnBulletList, rnDefList, rnLineBlock:
      result = orgList.newTree()
      for node in rst:
        result.add node.impl(level + 1)

    of rnBulletItem, rnDefItem:
      result = orgListItem.newTree(
        orgRawText.newTree("*"), # bullet
        newEmptyNode(), # counter
        newEmptyNode(), # checkbox
        newEmptyNode(), # tag
        rst[0].impl(level + 1) # header
      )

    else:
      pprint rst
      raise newImplementKindError(rst)


proc toOrgNode*(rst: PRstNode): OrgNode =
  impl(rst, 0)


when isMainModule:
  let r = parseRstString("""
``x`` is supposed to be the result of a comparator, i.e.
| ``< 0`` for *less than*,
| ``== 0`` for *equal*,
| ``> 0`` for *greater than*.
""")
  pprint r

  let org = r.toOrgNode().toSemOrg()
