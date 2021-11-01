import
  std/[options, tables, hashes, sequtils, enumerate]

import
  nimtraits

import
  hmisc/core/all,
  hmisc/other/[hpprint, hargparse, oswrap, hshell]

import
  std/[strutils, strformat]

import
  ../defs/[
    org_types,
    impl_org_node,
    impl_sem_org,
    impl_org_params
  ]


## Root implementation of the code blocks with shared properties that
## are available for all block subtypes. New blocks should inherit from
## [[code:RootCodeBlock]] type defined in this module.

type
  ## Shared code block property types

  CodeResCollection* = enum
    ## How the results should be collected from the code block.
    crcOutput ## Stdout/stderr of code block execution
    crcValue ## Actual value of source code block.
    crcValueType ## Value of the source code block and type (if language
                 ## supports types. If not, MIGHT be identical to
                 ## @enum{crcValue})

  CodeResType* = enum
    ## For which type of result the code block will return; affects how Org
    ## processes and inserts results in the Org buffer. When used in
    ## document compilation does not really affect anything, as results are
    ## inserted in AST, not in plaintext form.
    crtVerbatim
    crtTable
    crtList
    crtScalar
    crtFile

    crtJson, crtXml, crtCsv ## @group{} Program output is a structured
                            ## markup language.

  CodeResFormat* = enum
    ## For the result; affects how Org processes results;
    crtDrawer ## Put result in verbatim drawer
    crtHtml ## Result is passthrough html
    crtLatex ## Result is passthrough latex
    crtLink ## Result is single string literal
    crtGraphics ## Result is a link or path to the image
    crtOrg
    crtPP
    crtRaw

  CodeResHandling* = enum
    ## For inserting results once they are properly formatted.
    crtReplace ## Replace old result for code block
    crtSilent ## Ignore new result of the code block
    crtAppend ## Append result output to the code block results
    crtPrepend ## Prepend result output to the code block results

  CodeResExports* = enum
    creBoth ## Export both code and produced results
    creCode ## Only export original code
    creResults ## Only results
    creNone ## Do not export code block at all
    creFile ## Result is contained in a file, or a section of the file.
            ## Backends cal introduce their own special file names, such as
            ## `@generated` for nim.

  CodeEvalComments* = enum
    # TODO DOC
    cecNone
    cecLink
    cecNoweb
    cecOrg
    cecBoth

  CodeEvalWhen* = enum
    cewNever ## Dot not evaluate code block ever
    cewNeverExport ## Do not evaluate on export run
    cewQuery ## Query before evaluation
    cewQueryExport ## Query before exporting

  CodeEvalFailPolicy* = enum
    ## Policy modes for handling failed code block executions. Regardless
    ## of the particular failure policy whole session is aborted when
    ## single block fails ([[fn::e.g. for code blocks in C session failure
    ## might lead to segmentation fault, so in general it is impossible to
    ## handle block failures in any other way. Same goes for compiled
    ## languages - coimpilation in the middle of the session means it is
    ## impossible to continue further]])

    cefpRequireFailCompile ## `:fail compile` - expected to fail compilation
    cefpRequireFailExecute ## `:fail run` - expected to fail execution
    cefpRequireFail ## `:fail t` - expected to fail any stage, but failure
                    ## must happen
    cefpNoFail ## `:fail n` - disallow fail on any stage of the execution.
               ## Failed code block will lead to abort of the whole
               ## document compilation

    cefpCanFail ## `:fail any` - allowed to fail. Default mode of
                ## operation.

type
  RootCodeBlock* = ref object of CodeBlock
    ## Default block with common shared properties that are available in
    ## all code blocks
    evalSession*  {.Attr.}: Option[seq[string]] ## Name of the Session
    ## specified for code block. Note that due to support for layered
    ## sessions it forms a *path* rather than single identifier for a
    ## session. Blocks with no session specified (either directly or
    ## inheriting from parent tree context) have it as `none()`, o
    evalCache*    {.Attr.}: bool ## Avoids re-evaluating unchanged code blocks.
    evalFileDesc* {.Attr.}: Option[string]
    evalMkdirp*   {.Attr.}: bool
    evalShebang*  {.Attr.}: Option[string]

    evalPost*: Option[CodeEvalPost]
    evalFile*: Option[OrgFile]
    evalDir*: Option[OrgDir]
    evalVars*: Table[string, string]


    # - TODO :: add support for separating cmdline pased to /compiler/ and
    #   /compiled executable/. Latter one is far less important, but
    #   sometimes also necessary.
    #
    # - IDEA :: If additional command-line options are present, it might be
    #   a good idea to also support 'execution example' for compiled
    #   binary, so you could show (1) original source code, (2) how you
    #   compile/run resulting executable and finally produced output.
    #   Implicitly passed variables are also important since it might not
    #   be obvious how particular value has been passed from previous
    #   blocks in session. NOTE Intermediate evaluation results could be
    #   implemented by adding `evalIntermediate` field with sequence of
    #   elements. When `runCode` is executed, particular implementation
    #   might append to it.

    evalCmdline*: seq[string]
    evalComments* {.Attr.}: CodeEvalComments
    evalEpilogue*: Option[string]
    evalPrologue*: Option[string]
    evalWhen* {.Attr.}: CodeEvalWhen

    codeHash* {.Attr.}: Hash ## \
    ## Hash for this particular code block source and arguments
    cumulativeHash* {.Attr.}: Hash ## \
    ## Cumulative hash for all code block encountered in the *same session*
    ## during top-down scan of the document.

# 	:noweb-ref (See section 17)
# 	:noweb-sep (See section 18)
# :colnames (See section 5)	:padline (See section 19)
# :comments (See section 6)	:post (See section 20)
# :dir (See section 7)	:prologue (See section 21)
# :epilogue (See section 8)	:results (See section 22)
# :eval (See section 9)	:rownames (See section 23)
# :exports (See section 10)	:sep (See section 24)
# :file (See section 11)	:session (See section 25)
# :file-desc (See section 12)	:shebang (See section 26)
# :hlines (See section 13)	:tangle (See section 27)
# :mkdirp (See section 14)	:tangle-mode (See section 28)
# :no-expand (See section 15)	:var (See section 29)
# :noweb (See section 16)	:wrap (See section 30)

    resExports* {.Attr.}: CodeResExports
    resCollection* {.Attr.}: CodeResCollection
    # TODO this field should be a `case` to support different `colnames`
    # properties, but only for relevant block types.
    resType* {.Attr.}: CodeResType
    resFormat* {.Attr.}: CodeResFormat
    resHandling* {.Attr.}: CodeResHandling


proc newBlockCliParser*(name, doc: string): CliApp =
  newCliApp(name, (0, 0, 0), "", doc, noDefault = cliNoDefaultOpts)

proc addRootBlockArgs*(app: var CliApp) =
  app.add opt(
    ":cmdline",
    "Additional compilation command-line args",
    check = cliCheckFor(seq[string]))

  app.add opt(
    ":session",
    "Code block session name",
    check = cliCheckFor(string))



type
  OrgCmdParamAdd* = enum
    ocpdAsgn
    ocpdAdd
    ocpdRemove
    ocpdPreInsert
    ocpdPostAdd

  OrgCmdParam* = object
    addKin*: OrgCmdParamAdd

    key*: OrgNode ## Org-mode node that was used to specify parameter
    val*: seq[OrgNode]

const
  orgsHeaderArgs* = "header-args"

proc getFullArgs*(cb: RootCodeBlock, node: OrgNode, ctx: SemOrgCtx): seq[OrgNode] =
  var props: seq[OrgNode]
  for tree in ctx.scope:
    let tree = tree.tree
    if (orgsHeaderArgs, "") in tree.subtree.properties:
      props.add tree.subtree.properties[
        (orgsHeaderArgs, "")][orgfArgs].toSeq()

    if (orgsHeaderArgs, cb.langname) in tree.subtree.properties:
      let p = tree.subtree.properties[(orgsHeaderArgs, cb.langname)]
      props.add p[orgfArgs].toSeq()

  props.add node[orgfHeaderArgs, orgCmdArguments][orgfArgs].toSeq()

  result = props


proc chainSession*(cb: RootCodeBlock, ctx: var SemOrgCtx) =
  if cb.evalSession.isSome():
    let session = cb.evalSession.get()
    var prev: CodeBlock = nil
    var path: seq[string]
    for idx in countdown(session.high, 0):
      path = session[0 .. idx]
      if path in ctx.sessionTails:
        prev = ctx.sessionTails[path]
        break

    if notNil(prev):
      cb.prevInSession = prev

    ctx.sessionTails[path] = cb



proc parseBaseBlockArgs*(cb: RootCodeBlock): CliOptionsTable =
  for name, opt in cb.blockArgs.getRootCmd().options:
    case name:
      of ":cmdline": cb.evalCmdline = opt as seq[string]
      of ":session": cb.evalSession = some(@[opt as string])
      else: result[name] = opt

proc readBlockArgs*(cb: RootCodeBlock, node: OrgNode, scope: var SemOrgCtx) =
  # TODO merge parent scope arguments from toplevel
  let args = getFullArgs(cb, node, scope)
  if not cb.blockArgs.parseArgs(args):
    discard # IMPLEMENT error reporting


      # else:
      #   raise newImplementKindError(name)
  # for arg in cmdArguments["args"]:
  #   let value = arg["value"].strVal()
  #   case $arg["name"].text:
  #     of "session":
  #       cb.evalSession = some($value)

  #     of "exports":
  #       for entry in slices(split(value, ' '), value):
  #         case $entry:
  #           of "both":     cb.resExports = creBoth
  #           of "code":     cb.resExports = creCode
  #           of "results":  cb.resExports = creResults
  #           of "none":     cb.resExports = creNone

  #           of "drawer":   cb.resFormat = crtDrawer
  #           of "html":     cb.resFormat = crtHtml
  #           of "latex":    cb.resFormat = crtLatex
  #           of "link":     cb.resFormat = crtLink
  #           of "graphics": cb.resFormat = crtGraphics
  #           of "org":      cb.resFormat = crtOrg
  #           of "pp":       cb.resFormat = crtPP
  #           of "raw":      cb.resFormat = crtRaw

  #           of "output":   cb.resCollection = crcOutput
  #           of "value":    cb.resCollection = crcValue
  #           of "value-type":
  #             cb.resCollection = crcValueType


  #           of "replace":  cb.resHandling = crtReplace
  #           of "silent":   cb.resHandling = crtSilent
  #           of "append":   cb.resHandling = crtAppend
  #           of "prepend":  cb.resHandling = crtPrepend

  #           else:
  #             raise newUnexpectedString(
  #               entry,
  #               "Unexpected export specification",
  #               [
  #                 "both", "code", "results", "none",

  #                 "drawer", "html", "latex", "link", "graphics", "org",
  #                 "pp", "raw",

  #                 "output", "value",

  #                 "replace", "silent", "append", "prepend",
  #               ]
  #             )

  #     of "eval":
  #       case $value:
  #         of "never":
  #           cb.evalWhen = cewNever

  #         of "noexport", "never-export", "no-export":
  #           cb.evalWhen = cewNeverExport

  #         of "query":
  #           cb.evalWhen = cewQuery

  #         of "query-export":
  #           cb.evalWhen = cewQuery

  #         else:
  #           raise newUnexpectedString(
  #             value,
  #             "Unexpected export specification",
  #             ["never", "noexport", "never-export", "no-export", "query",
  #              "query-export"
  #             ]
  #           )

method parseFrom*(
    codeBlock: RootCodeBlock, node: OrgNode,
    scope: var SemOrgCtx) =
  readBlockArgs(codeBlock, node, scope)
  let extra = parseBaseBlockArgs(codeBlock)
    # IMPLEMENT check for unexpected arguments, erport error

  codeBlock.chainSession(scope)


method runCode*(
    codeBlock: RootCodeBlock,
    context: var CodeRunContext,
    conf: OrgConf
  ) =

  raise newImplementBaseError(RootCodeBlock(), "runCode")

method blockPPtree*(
    codeBlock: RootCodeBlock, conf: var PPrintConf): PPrintTree =
  pptree(codeBlock, conf)


const
  charSoh* = '\x01'
  charStx* = '\x02'
  charEtx* = '\x03'
  charEot* = '\x04'

proc idxBeginStr*(idx: int, tag: string): string =
  &"\"{charSoh}org,idx:{idx},tag:{tag}{charStx}\""

proc idxEndStr*(idx: int, tag: string): string =
  &"\"{charEtx}org,idx:{idx},tag:{tag}{charEot}\""


proc splitEvalResult*(code: string, idx: int, tag: string):
  tuple[preChunk, target, postTarget: string] =
  ## Take output of the runnable code block session group and search for
  ## the delimited output chunk. Output is split on the 'SOH' (start of
  ## header) character and then searched for the appropriate block of text.
  ##
  ## All text encountered before first SOH is added into @ret{preChunk},
  ## all text after last EOT (end of transmission) is put into
  ## @ret{postTarget}
  ##
  ## Properly formatted chunks should contain
  ## @edsl{SOH <data> STX <body> ETX <data> EOT} where @edsl{<data>}
  ## is a text in form of @edsl{"org,idx:" <idx:int> ",tag" <tag:string>}.
  ## @ex{"org,idx:1,tag:r"}
  ##
  ## For automatic creation of the start/end header block for transmissions see
  ## [[code:idxBeginStr()]] and [[code:idxEndStr()]]
  var
    lastEot = 0
    foundChunk = false

  for chunk in code.split(charSoh):
    block chunkLoop:
      let
        stx = chunk.find(charStx)
        etx = chunk.find(charEtx)
        eot = chunk.find(charEot)

      if stx == -1 or etx == -1:
        result.preChunk.add chunk

      elif foundChunk:
        lastEot = eot

      else:
        lastEot = eot
        let
          header = chunk[0 ..< stx]
          body = chunk[stx + 1 ..< etx]
          footer = chunk[etx + 1 ..< eot]

        var (idxOk, tagOk) = (false, false)

        const n = high("org,")
        if header[0 .. n] != "org,":
          break chunkLoop

        for param in header[n + 1 .. ^1].split(','):
          let key = param[0 ..< param.find(':')]
          let val = param[key.len + 1 .. ^1]
          case key:
            of "idx": idxOk = parseInt(val) == idx
            of "tag": tagOk = val == tag
            else: raise newUnexpectedKindError(key)

        if idxOK and tagOK:
          result.target = body
          foundChunk = true

  if not foundChunk:
    return ("", code, "")

  elif lastEot < code.high:
    result.postTarget = code[lastEot + 1 .. ^1]

proc cutForSession*(res: var ShellResult, idx: int, tag: string) =
  let (pre, target, post) = splitEvalResult(res.execResult.stdout, idx, tag)
  res.execResult.stdout = target

proc evalCode*(sem: var SemOrg, conf: OrgConf) =
  var ctx: CodeRunContext
  proc aux(sem: var SemOrg) =
    case sem.kind:
      of orgSrcCode:
        runCode(sem.codeBlock, ctx, conf)

        if canGet(sem.codeBlock.execResult, res):
          if canGet(res.execResult, exec):
            sem["eval-result"] = newSem(orgRawText, exec.execResult.stdout)

      else:
        for sub in mitems(sem):
          aux(sub)

  aux(sem)

proc getLangDir*(conf: OrgConf, cb: RootCodeBlock): AbsDir =
  conf.tempDir / cb.langName
  # for entry in scope:
  #   for drawer in entry.tree.node["drawers"]:
  #     if drawer["name"].text == "properties":
  #       for prop in drawer["body"]:
  #         if prop["name"].text == "header-args" and
  #            prop["subname"].text == cb.langName:

  #           parseBaseBlockArgs(cb, prop["values"])

  # parseBaseBlockArgs(cb, semorg.node["header-args"])

  # cb.code = $semorg.node["body"].text
