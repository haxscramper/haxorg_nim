import semorg
import nimtraits/trait_xml
import ast, buf
import nimtraits
import hmisc/other/[hshell, oswrap]
import std/[uri, streams, strutils]
import hmisc/hdebug_misc
import hmisc/hasts/xml_ast
export newHXmlParser

using
  tree: SemOrg
  conf: RunConfig
  r: var HXmlParser

proc loadXml*(r; tree: var SemOrg, tag: string)

proc loadXml*(r; path: var AnyPath, tag: string) =
  var str = ""
  loadXml(r, str, tag)

proc loadXml(r; cmd: var ShellCmd, tag: string) =
  genXmlLoader(ShellCmd, cmd, r, tag, newObjExpr = ShellCmd())


proc loadXml(r; cmd: var ShellError, tag: string) =
  genXmlLoader(ShellError, cmd, r, tag, newObjExpr = ShellError())

proc loadXml(r; cmd: var ShellExecResult, tag: string) =
  genXmlLoader(ShellExecResult, cmd, r, tag, newObjExpr = ShellExecResult())

storeTraits(ShellResult)
proc loadXml(r; cmd: var ShellResult, tag: string) =
  genXmlLoader(ShellResult, cmd, r, tag, newObjExpr = ShellResult())

proc loadXml(r; url: var Url, tag: string) =
  var str = ""
  loadXml(r, str, tag)
  url = Url(str)


proc loadXml*(r; it: var OrgFile, tag: string) =
  discard
  # genXmlWriter(OrgFile, it, r, tag)

proc loadXml*(r; it: var OrgDir, tag: string) =
  discard

proc loadXml*(r; cmd: var SemMetaTag, tag: string)

proc loadXml*(r; cmd: var SemItemTag, tag: string) =
  genXmlLoader(SemItemTag, cmd, r, tag, newObjExpr = SemItemTag())

proc loadXml*(r; cmd: var SemMetaTag, tag: string) =
  genXmlLoader(SemMetaTag, cmd, r, tag, newObjExpr = SemMetaTag())

proc loadXml*(r; cmd: var OrgCommand, tag: string) =
  genXmlLoader(OrgCommand, cmd, r, tag, newObjExpr = OrgCommand())

proc loadXml*(r; cmd: var CodeLinkType, tag: string) =
  genXmlLoader(CodeLinkType, cmd, r, tag, newObjExpr = CodeLinkType())

proc loadXml*(r; cmd: var CodeLinkPart, tag: string) =
  genXmlLoader(CodeLinkPart, cmd, r, tag, newObjExpr = CodeLinkPart())

proc loadXml*(r; cmd: var CodeLink, tag: string) =
  genXmlLoader(CodeLink, cmd, r, tag, newObjExpr = CodeLink())

proc loadXml*(r; cmd: var OrgLink, tag: string) =
  genXmlLoader(OrgLink, cmd, r, tag, newObjExpr = OrgLink())


proc loadXml*(r; it: var CodeResult, tag: string) =
  genXmlLoader(CodeResult, it, r, tag, newObjExpr = CodeResult())

proc loadXml*(r; it: var CodeEvalPost, tag: string) =
  genXmlLoader(CodeEvalPost, it, r, tag, newObjExpr = CodeEvalPost())

proc loadXml*(r; it: var CodeBlock, tag: string) =
  discard

proc loadXml*(r; it: var OrgAssocEntry, tag: string) =
  genXmlLoader(OrgAssocEntry, it, r, tag, newObjExpr = OrgAssocEntry())

proc loadXml*(r; it: var OrgCompletion, tag: string) =
  genXmlLoader(OrgCompletion, it, r, tag, newObjExpr = OrgCompletion())

proc loadXml*(r; it: var StrSlice, tag: string) =
  var str = ""
  loadXml(r, str, tag)


proc loadXml*(r; it: var OrgPropertyArg, tag: string) =
  genXmlLoader(OrgPropertyArg, it, r, tag, newObjExpr = OrgPropertyArg())

proc loadXml*(r; it: var OrgProperty, tag: string) =
  genXmlLoader(OrgProperty, it, r, tag, newObjExpr = OrgProperty())

proc skipOpen*(r; tag: string) =
  expectAt(r, {xmlElementOpen}, "skipOpen")
  assert r.elementName() == tag
  r.next()


proc skipClose*(r) =
  expectAt(r, {xmlElementClose}, "skipClose")
  r.next()


proc `[]`*(r; key: string): bool =
  expectAt(r, {xmlAttr, xmlElementStart, xmlElementOpen, xmlElementEnd}, "[]")
  case r.kind:
    of xmlAttr:
      result = r.attrKey() == key
    of xmlElementStart, xmlElementOpen, xmlElementEnd:
      result = r.elementName() == key

    else:
      discard

proc atClose*(r): bool = r.kind in {xmlElementClose}
proc atAttr*(r): bool = r.kind in {xmlAttr}

proc loadEnumWithPrefix*[E](r; kind: var E, tag, prefix: string) =
  loadPrimitive(
    r, kind, tag,
    (kind = parseEnum[E](prefix & r.strVal())),
    (kind = parseEnum[E](prefix & r.strVal()))
  )


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

  startHaxComp()
  genXmlLoader(
    SemOrg, tree, r, tag, loadHeader = false,
    extraFieldLoader = {
      "text": loadXml(r, tree.str, "text")
    }
  )
  stopHaxComp()
