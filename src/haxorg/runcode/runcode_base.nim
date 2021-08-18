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