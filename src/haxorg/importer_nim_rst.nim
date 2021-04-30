## Convert nim RST to org-mode

import packages/docutils/[rst, rstast]
import hmisc/base_errors
import hmisc/other/oswrap
import hpprint
import std/[streams]

import ./ast, ./exporter_xml, ./semorg


proc parseRstString*(s: string, inputPath: AbsFile = AbsFile("")): PRstNode =
  const filen = "input"
  var dummyHasToc = false
  let findFile = proc(path: string): string =
    echo "find file", path

  result = rstParse(
    s, filen, 0, 1, dummyHasToc, {},
    findFile = findFile
  )

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
        result = onkParagraph.newTree()

      else:
        result = onkStmtList.newTree()

      for node in rst:
        result.add impl(node, level + 1)

    of rnFieldBody, rnParagraph:
      result = onkParagraph.newTree()
      for node in rst:
        result.add impl(node, level + 1)

    of rnLeaf:
      result = onkWord.newTree(rst.text)

    of rnOverline:
      # FIXME nim rst parser does not build explicit document structure and
      # instead only has 'levels' for heading, which means I have to
      # manually reconstruct this.
      result = onkSubtree.newTree()
      for node in rst:
        result.add impl(node, level + 1)

    of rnSubstitutionReferences:
      result = onkMacro.newTree(rst.bodyText())

    of rnFieldList:
      result = newOStmtList()
      for node in rst:
        let name = node[0]
        let value = node[1]

        result.add onkCommand.newTree(
          onkIdent.newTree(name[0].text),
          value.impl(level + 1)
        )



    of rnInlineLiteral, rnInterpretedText: # FIXME differentiate between these two
      var buf: string
      for node in rst:
        buf &= node.text

      result = onkMarkup.newTree(
        oskMonospaced, onkRawText.newTree(buf))

    of rnStrongEmphasis:
      result = onkMarkup.newTree(oskBold)
      for node in rst:
        result.add impl(node, level + 1)

    of rnEmphasis:
      result = onkMarkup.newTree(oskItalic)
      for node in rst:
        result.add impl(node, level + 1)

    of rnContents:
      result = onkCommand.newTree("toc")


    of rnCodeBlock:
      result = onkSrcCode.newTree()
      result.add onkIdent.newTree(rst[0].bodyText)
      result.add newEmptyNode()
      result.add onkRawText.newTree(rst[2].bodyText)

    of rnOptionList, rnOptionListItem, rnOptionGroup, rnDescription,
       rnDefName, rnDefBody:
      # nim RST parser is broken for inline `--flag` options on 1.4.6.
      # https://nim-lang.org/docs/system.html#disarm.t%2Ctyped and
      # `DefList` works very strangely as well, so just hack-fix for now.
      # Devel seems to work fine though.

      result = onkParagraph.newTree()
      for node in rst:
        result.add impl(node, level + 1)


    of rnBlockQuote:
      var buf: string
      for sub in rst[0]:
        buf.add sub.text

      result = onkMultilineCommand.newTree(
        onkIdent.newTree("quote"),
        onkRawText.newTree(buf)
      )

    of rnHyperlink:
      result = onkLink.newTree(
        rst[1].impl(level + 1),
        rst[0].impl(level + 1)
      )

    of rnIdx:
      result = newEmptyNode()

    of rnBulletList, rnDefList:
      result = onkList.newTree()
      for node in rst:
        result.add node.impl(level + 1)

    of rnBulletItem, rnDefItem:
      result = onkListItem.newTree(
        onkRawText.newTree("*"), # bullet
        newEmptyNode(), # counter
        newEmptyNode(), # checkbox
        newEmptyNode(), # tag
        rst[0].impl(level + 1) # header
      )

    else:
      pprint rst
      raiseImplementKindError(rst)


proc toOrgNode*(rst: PRstNode): OrgNode =
  impl(rst, 0)


when isMainModule:
  let r = parseRstString("""
Test input *string*
""")
  pprint r

  let org = r.toOrgNode().toSemOrg()
  echo org.treeRepr()


  var writer = newXmlWriter(stdout.newFileStream())
  writer.writeXml(org, "main")
