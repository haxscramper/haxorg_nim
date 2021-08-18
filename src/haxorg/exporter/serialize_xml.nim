 import exporter, semorg
import nimtraits/trait_xml
import ast, buf
import nimtraits
import hmisc/other/[hshell, oswrap]
import hmisc/hasts/xml_ast
export newHXmlParser
import std/[uri, streams, strutils]
import hmisc/core/all


storeTraits(ShellCmd)
storeTraits(ShellError)
storeTraits(ShellExecResult)
storeTraits(ShellResult)


#===========================  Type defintions  ===========================#

type
  OrgXmlExporter = ref object of OrgExporter

#=============================  Boilerplate  =============================#

using
  exp: OrgXmlExporter
  tree: SemOrg
  conf: RunConf
  w: var XmlWriter

proc newOrgXmlExporter*(): OrgXmlExporter =
  result = OrgXmlExporter(
    name: "xml-base",
    fileExt: "xml",
    description: "Export document to xml",
  )

#=========================  Register exporters  ==========================#

register(newOrgXmlExporter())

using
  tree: SemOrg
  conf: RunConf
  r: var HXmlParser

proc loadXml*(r; tree: var SemOrg, tag: string)
proc writeXml*(
  w: var XmlWriter, tree: SemOrg, tag: string, inParagraph: bool = false)
proc loadXml*(r; cmd: var SemMetaTag, tag: string)
proc writeXml*(w; cmd: SemMetaTag, tag: string)




# ~~~~ AnyPath ~~~~ #

proc loadXml*(r; path: var AnyPath, tag: string) =
  var str = ""
  loadXml(r, str, tag)

proc writeXml*(w; path: AnyPath, tag: string) =
  writeXml(w, path.getStr(), tag)

# ~~~~ ShellCmd ~~~~ #

proc loadXml(r; cmd: var ShellCmd, tag: string) =
  genXmlLoader(ShellCmd, cmd, r, tag, newObjExpr = ShellCmd())

proc writeXml(w; cmd: ShellCmd, tag: string) =
  genXmlWriter(ShellCmd, cmd, w, tag)

# ~~~~ ShellError ~~~~ #

proc loadXml(r; cmd: var ShellError, tag: string) =
  genXmlLoader(ShellError, cmd, r, tag, newObjExpr = ShellError())

proc writeXml(w; cmd: ShellError, tag: string) =
  genXmlWriter(ShellError, cmd, w, tag)

# ~~~~ ShellExecResult ~~~~ #

proc loadXml(r; cmd: var ShellExecResult, tag: string) =
  genXmlLoader(ShellExecResult, cmd, r, tag, newObjExpr = ShellExecResult())

proc writeXml(w; cmd: ShellExecResult, tag: string) =
  genXmlWriter(ShellExecResult, cmd, w, tag)

# ~~~~ ShellResult ~~~~ #

proc loadXml(r; cmd: var ShellResult, tag: string) =
  genXmlLoader(ShellResult, cmd, r, tag, newObjExpr = ShellResult())

proc writeXml(w; cmd: ShellResult, tag: string) =
  genXmlWriter(ShellResult, cmd, w, tag)

# ~~~~ Url ~~~~ #

proc loadXml(r; url: var Url, tag: string) =
  var str = ""
  loadXml(r, str, tag)
  url = Url(str)

proc writeXml(w; url: Url, tag: string) =
  writeXml(w, url.string, tag)

# ~~~~ OrgFile ~~~~ #

proc loadXml*(r; it: var OrgFile, tag: string) =
  raiseImplementError("")

proc writeXml*(w; it: OrgFile, tag: string) =
  raiseImplementError("")

# ~~~~ OrgDir ~~~~ #

proc loadXml*(r; it: var OrgDir, tag: string) =
  raiseImplementError("")

proc writeXml*(w; it: OrgDir, tag: string) =
  raiseImplementError("")
  # genXmlWriter(OrgDir, it, w, tag)

# ~~~~ SemMetaTag ~~~~ #

proc writeXml*(w; cmd: SemMetaTag, tag: string) =
  genXmlWriter(SemMetaTag, cmd, w, tag)


# ~~~~ SemItemTag ~~~~ #

proc loadXml*(r; cmd: var SemItemTag, tag: string) =
  genXmlLoader(SemItemTag, cmd, r, tag, newObjExpr = SemItemTag())

proc writeXml*(w; cmd: SemItemTag,    tag: string) =
  genXmlWriter(SemItemTag,     cmd, w, tag)

# ~~~~ SemMetaTag ~~~~ #


proc loadXml*(r; cmd: var OrgLink, tag: string)
proc writeXml*(w; cmd: OrgLink, tag: string)
proc loadXml*(r; cmd: var SemMetaTag, tag: string) =
  genXmlLoader(SemMetaTag, cmd, r, tag, newObjExpr = SemMetaTag())

# ~~~~ OrgCommand ~~~~ #

proc loadXml*(r; cmd: var OrgCommand, tag: string) =
  genXmlLoader(OrgCommand, cmd, r, tag, newObjExpr = OrgCommand())

proc writeXml*(w; cmd: OrgCommand, tag: string) =
  genXmlWriter(OrgCommand, cmd, w, tag)


# ~~~~ CodeLinkType ~~~~ #

# proc loadXml*(r; cmd: var CodeLinkType, tag: string) =
#   genXmlLoader(CodeLinkType, cmd, r, tag, newObjExpr = CodeLinkType())

# proc writeXml*(w; cmd: CodeLinkType, tag: string) =
#   genXmlWriter(CodeLinkType, cmd, w, tag)


# # ~~~~ CodeLinkPart ~~~~ #

# proc loadXml*(r; cmd: var CodeLinkPart, tag: string) =
#   genXmlLoader(CodeLinkPart, cmd, r, tag, newObjExpr = CodeLinkPart())

# proc writeXml*(w; cmd: CodeLinkPart, tag: string) =
#   genXmlWriter(CodeLinkPart,   cmd, w, tag)

# # ~~~~ CodeLink ~~~~ #

# proc loadXml*(r; cmd: var CodeLink, tag: string) =
#   genXmlLoader(CodeLink, cmd, r, tag, newObjExpr = CodeLink())

# proc writeXml*(w; cmd: CodeLink, tag: string) =
#   genXmlWriter(CodeLink, cmd, w, tag)

# ~~~~ OrgLink ~~~~ #

proc loadXml*(r; cmd: var OrgLink, tag: string) =
  genXmlLoader(OrgLink, cmd, r, tag, newObjExpr = OrgLink())

proc writeXml*(w; cmd: OrgLink, tag: string) =
  genXmlWriter(OrgLink, cmd, w, tag)

# ~~~~ CodeResult ~~~~ #

proc loadXml*(r; it: var CodeResult, tag: string) =
  genXmlLoader(CodeResult, it, r, tag, newObjExpr = CodeResult())

proc writeXml*(w; it: CodeResult, tag: string) =
  genXmlWriter(CodeResult, it,  w, tag)

# ~~~~ CodeEvalPost ~~~~ #

proc loadXml*(r; it: var CodeEvalPost, tag: string) =
  genXmlLoader(CodeEvalPost, it, r, tag, newObjExpr = CodeEvalPost())

proc writeXml*(w; it: CodeEvalPost, tag: string) =
  genXmlWriter(CodeEvalPost, it,  w, tag)


# ~~~~ CodeBlock ~~~~ #

proc loadXml*(r; it: var CodeBlock, tag: string) =
  genXmlLoader(CodeBlock, it, r, tag, newObjExpr = CodeBlock())

proc writeXml*(w; it: CodeBlock, tag: string) =
  genXmlWriter(
    CodeBlock, it,  w, tag, skipFieldWrite = ["code"], addClose = false)
  w.indent()
  w.xmlWrappedCdata(it.code, "code")
  w.dedent()
  w.xmlEnd(tag)

# ~~~~ OrgAssocEntry ~~~~ #

proc loadXml*(r; it: var OrgAssocEntry, tag: string) =
  genXmlLoader(OrgAssocEntry, it, r, tag, newObjExpr = OrgAssocEntry())

proc writeXml*(w; it: OrgAssocEntry, tag: string) =
  genXmlWriter(OrgAssocEntry, it, w, tag)


# ~~~~ OrgCompletion ~~~~ #

proc loadXml*(r; it: var OrgCompletion, tag: string) =
  genXmlLoader(OrgCompletion, it, r, tag, newObjExpr = OrgCompletion())

proc writeXml*(w; it: OrgCompletion, tag: string) =
  genXmlWriter(OrgCompletion, it, w, tag)


# ~~~~ StrSlice ~~~~ #

proc loadXml*(r; it: var StrSlice, tag: string) =
  var str = ""
  loadXml(r, str, tag)

proc writeXml*(w; it: StrSlice, tag: string) =
  writeXml(w, $it, tag)


# ~~~~ OrgPropertyArg ~~~~ #

proc loadXml*(r; it: var OrgPropertyArg, tag: string) =
  genXmlLoader(OrgPropertyArg, it, r, tag, newObjExpr = OrgPropertyArg())

proc writeXml*(w; it: OrgPropertyArg, tag: string) =
  genXmlWriter(OrgPropertyArg, it,  w, tag)


# ~~~~ OrgProperty ~~~~ #

proc loadXml*(r; it: var OrgProperty, tag: string) =
  genXmlLoader(OrgProperty, it, r, tag, newObjExpr = OrgProperty())

proc writeXml*(w; it: OrgProperty, tag: string) =
  genXmlWriter(OrgProperty, it, w, tag)


# ~~~~ Semorg ~~~~ #

proc loadXml*(r; kind: var OrgNodeKind, tag: string) =
  loadEnumWithPrefix(r, kind, tag, "onk")

proc loadXml*(r; kind: var OrgNodeSubKind, tag: string) =
  loadEnumWithPrefix(r, kind, tag, "osk")

proc loadXml*(r; tree: var SemOrg, tag: string) =
  if r["text"]:
    echov "Loaded text"
    var str = ""
    r.next()
    tree = newSemOrg(onkWord)
    loadXml(r, str, tag)
    r.next()
    return

  var kind: OrgNodeKind
  r.skipOpen(tag)

  if r["isGenerated"]: r.next()
  if r["kind"]: loadXml(r, kind, "kind")

  tree = newSemOrg(kind)

  while r.atAttr():
    case r.attrKey():
      of "subkind": loadXml(r, tree.subkind, "subkind")
      of "str": loadXml(r, tree.str, "str")

      else:
        r.raiseUnexpectedAttribute()

  r.skipClose()

  genXmlLoader(
    SemOrg, tree, r, tag, loadHeader = false,
    extraFieldLoad = {
      "text": loadXml(r, tree.str, "text")
    }
  )


proc writeXmlParagraph*(w; tree; tag: string) =
  case tree.kind:
    of onkWord:
      w.writeRaw($tree.node.text)

    of onkParagraph:
      for node in tree:
        writeXmlParagraph(w, node, tag)

    else:
      writeXml(w, tree, tag, true)



proc writeXml*(
    w: var XmlWriter, tree: SemOrg, tag: string,
    inParagraph: bool = false
  ) =

  if isNil(tree):
    writeXml(w, newSemOrg(onkEmptyNode), tag)

  else:
    # case tree.kind:
      # of onkParagraph:
      #   w.xmlStart(tag)
      #   writeXmlParagraph(w, tree, tag)
      #   w.xmlEnd(tag, false)

      # else:
      # if inParagraph and tree.kind in {onkWord}:
      #   writeXmlParagraph(w, tree, tag)

      # else:
        genXmlWriter(
          SemOrg, tree, w, tag, addClose = false,
          extraAttrWrite = (
            if not tree.isGenerated and
               tree.kind in orgSubnodeKinds:
              w.space()
              w.xmlAttribute("str", tree.node.str)
          ),
          addStartIndent = not inParagraph,
          addEndIndent = not inParagraph
        )

        if not tree.isGenerated and tree.node.kind in orgTokenKinds:
          w.indent()
          w.writeXml(tree.node.text, "text")
          w.dedent()

        w.xmlEnd(tag)

export newXmlWriter

method exportTo*(exp, tree; target: var string; conf = defaultRunConf) =
  var writer = newXmlWriter(newOutStringStream(target))
  writer.writeXml(tree, "main")


method exportTo*(
  exp, tree; target: AbsFile; conf: RunConf = defaultRunConf) =

  raiseImplementError("")
