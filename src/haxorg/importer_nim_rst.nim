## Convert nim RST to org-mode

import packages/docutils/[rst, rstast, rstgen]
import hmisc/base_errors
import hpprint

import ./ast


proc parseRstString*(s: string): PRstNode =
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

proc toOrgNode*(rst: PRstNode): OrgNode =
  case rst.kind:
    of rnInner:
      result = onkDocument.newTree()
      for node in rst:
        result.add toOrgNode(node)

    of rnFieldBody, rnParagraph:
      result = onkParagraph.newTree()
      for node in rst:
        result.add toOrgNode(node)

    of rnLeaf:
      result = onkWord.newTree(rst.text)

    of rnOverline:
      # FIXME nim rst parser does not build explicit document structure and
      # instead only has 'levels' for heading, which means I have to
      # manually reconstruct this.
      result = onkSubtree.newTree()
      for node in rst:
        result.add toOrgNode(node)

    of rnSubstitutionReferences:
      result = onkMacro.newTree(rst.bodyText())

    of rnFieldList:
      result = newOStmtList()
      for node in rst:
        let name = node[0]
        let value = node[1]

        result.add onkCommand.newTree(
          onkIdent.newTree(name[0].text),
          value.toOrgNode()
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
        result.add toOrgNode(node)

    of rnContents:
      result = onkCommand.newTree("toc")


    of rnCodeBlock:
      result = onkSrcCode.newTree()
      result.add onkIdent.newTree(rst[0].bodyText)
      result.add newEmptyNode()
      result.add onkRawText.newTree(rst[2].bodyText)

    of rnBlockQuote:
      var buf: string
      for sub in rst[0]:
        buf.add sub.text

      result = onkMultilineCommand.newTree(
        onkIdent.newTree("quote"),
        onkRawText.newTree(buf)
      )

    else:
      raiseImplementKindError(rst)


when isMainModule:
  let r = parseRstString("""
==========
Nim Manual
==========

:Authors: Andreas Rumpf, Zahary Karadjov
:Version: |nimversion|

.. contents::


  "Complexity"

The `|`, `/`


.. code-block:: nim
  var a: array[0..1, char]
  let i = 5

""")
  pprint r

  let org = r.toOrgNode()
  echo org.treeRepr()
