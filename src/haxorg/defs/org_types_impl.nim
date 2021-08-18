
proc newCodeBlock*(config: RunConf, lang: string): CodeBlock =
  # TODO DOC
  if lang in config.codeCreateCallbacks:
    return config.codeCreateCallbacks[lang]()

  else:
    return DefaultCodeBlock()


func newOrgLink*(kind: OrgLinkKind): OrgLink = OrgLink(kind: kind)
func newOrguserLink*(): OrgUserLink = OrgUserLink()
func initOrgUserLink*(): OrgUserLink = new(result)

proc newSemOrg*(node: OrgNode): SemOrg =
  SemOrg(kind: node.kind, isGenerated: false, node: node,
         subkind: node.subkind)

proc newSemOrg*(kind: OrgNodeKind, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(kind: kind, isGenerated: true, subnodes: toSeq(subnodes))

proc newUnexpectedString*(
    entry: StrSlice,
    message: string,
    alternatives: openarray[string]
  ): CodeError =

  newCodeError(entry, message, $stringMismatchMessage($entry, alternatives))
