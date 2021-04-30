import exporter, semorg
import nimtraits/trait_xml
import ast, buf
import nimtraits
import hmisc/other/[hshell, oswrap]
import std/[uri, streams]
import hmisc/hdebug_misc

#===========================  Type defintions  ===========================#

type
  OrgXmlExporter = ref object of OrgExporter

#=============================  Boilerplate  =============================#

using
  exp: OrgXmlExporter
  tree: SemOrg
  conf: RunConfig
  w: var XmlWriter

proc newOrgXmlExporter*(): OrgXmlExporter =
  result = OrgXmlExporter(
    name: "xml-base",
    fileExt: "xml",
    description: "Export document to xml",
  )

#=========================  Register exporters  ==========================#

register(newOrgXmlExporter())

proc writeXml*(
  w: var XmlWriter, tree: SemOrg, tag: string, inParagraph: bool = false)

proc writeXml*(w; path: AnyPath, tag: string) =
  writeXml(w, path.getStr(), tag)

storeTraits(ShellCmd)
proc writeXml(w; cmd: ShellCmd, tag: string) =
  genXmlWriter(ShellCmd, cmd, w, tag)

storeTraits(ShellError)
proc writeXml(w; cmd: ShellError, tag: string) =
  genXmlWriter(ShellError, cmd, w, tag)

storeTraits(ShellExecResult)
proc writeXml(w; cmd: ShellExecResult, tag: string) =
  genXmlWriter(ShellExecResult, cmd, w, tag)

storeTraits(ShellResult)
proc writeXml(w; cmd: ShellResult, tag: string) =
  genXmlWriter(ShellResult, cmd, w, tag)

proc writeXml(w; url: Url, tag: string) =
  writeXml(w, url.string, tag)


proc writeXml*(w; it: OrgFile, tag: string) =
  discard
  # genXmlWriter(OrgFile, it, w, tag)

proc writeXml*(w; it: OrgDir, tag: string) =
  discard
  # genXmlWriter(OrgDir, it, w, tag)


proc writeXml*(w; cmd: SemMetaTag, tag: string)

proc writeXml*(w; cmd: SemItemTag,    tag: string) = genXmlWriter(SemItemTag,     cmd, w, tag)
proc writeXml*(w; cmd: SemMetaTag,    tag: string) = genXmlWriter(SemMetaTag,     cmd, w, tag)
proc writeXml*(w; cmd: OrgCommand,    tag: string) = genXmlWriter(OrgCommand,     cmd, w, tag)
proc writeXml*(w; cmd: CodeLinkType,  tag: string) = genXmlWriter(CodeLinkType,   cmd, w, tag)
proc writeXml*(w; cmd: CodeLinkPart,  tag: string) = genXmlWriter(CodeLinkPart,   cmd, w, tag)
proc writeXml*(w; cmd: CodeLink,      tag: string) = genXmlWriter(CodeLink,       cmd, w, tag)
proc writeXml*(w; cmd: OrgLink,       tag: string) = genXmlWriter(OrgLink,        cmd, w, tag)


proc writeXml*(w; it: CodeResult,     tag: string) = genXmlWriter(CodeResult,     it,  w, tag)
proc writeXml*(w; it: CodeEvalPost,   tag: string) = genXmlWriter(CodeEvalPost,   it,  w, tag)

proc writeXml*(w; it: CodeBlock, tag: string) =
  startHaxComp()
  genXmlWriter(CodeBlock, it,  w, tag, ignoredNames = ["code"], addClose = false)
  w.xmlWrappedCdata("code", it.code)
  w.indent()
  w.xmlEnd(tag)
  w.dedent()
  stopHaxComp()

proc writeXml*(w; it: OrgAssocEntry,  tag: string) = genXmlWriter(OrgAssocEntry,  it,  w, tag)
proc writeXml*(w; it: OrgCompletion,  tag: string) = genXmlWriter(OrgCompletion,  it,  w, tag)
proc writeXml*(w; it: StrSlice,       tag: string) = writeXml(w,                  $it, tag)
proc writeXml*(w; it: OrgPropertyArg, tag: string) = genXmlWriter(OrgPropertyArg, it,  w, tag)
proc writeXml*(w; it: OrgProperty,    tag: string) = genXmlWriter(OrgProperty,    it,  w, tag)


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
    case tree.kind:
      of onkParagraph:
        w.xmlStart(tag)
        writeXmlParagraph(w, tree, tag)
        w.xmlEnd(tag, false)

      else:
        if inParagraph and tree.kind in {onkWord}:
          writeXmlParagraph(w, tree, tag)

        else:
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

type
  StringStreamOut = ref object of StreamObj
    str: ptr string

export newXmlWriter

method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  var stream = StringStreamOut(
    str: addr target,
  )

  stream.writeDataImpl = proc(
    s: StringStreamOut; buffer: pointer; bufLen: int) =
    let len = s.str[].len
    s.str[].setLen(s.str[].len + bufLen)
    copyMem(addr(s.str[len]), buffer, bufLen)

  var writer = newXmlWriter(stream)
  writer.writeXml(tree, "main")


method exportTo*(
  exp, tree; target: AbsFile; conf: RunConfig = defaultRunConfig) =

  raiseImplementError("")
