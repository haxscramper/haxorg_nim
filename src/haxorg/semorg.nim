{.experimental: "caseStmtMacros".}

import ast, buf
import std/[options, tables, strutils, strformat, uri]
import hpprint, hpprint/hpprint_repr
import hmisc/other/hshell
import hmisc/other/oswrap
import hmisc/hdebug_misc

import fusion/matching



type
  CodeBuilder = proc(): CodeBlock

  RunConfig* = object
    tempDir*: string
    codeCreateCallbacks*: Table[string, proc(): CodeBlock]

  OrgCompletion* = object
    ## Completion status cookie
    case isPercent*: bool
      of true:
        percent*: float

      of false:
        done*: int
        total*: int


  TreeScope* = object
    ## Subtree scope. Mostly used for internal implementation in sempass
    tree*: SemOrg

  LinkTarget* = object
    case isExternal*: bool
      of true:
        targetStr*: string

      of false:
        targetEntry*: SemOrg


  CodeResCollection* = enum
    ## How the results should be collected from the code block.
    crcValue ## Actual value of source code block.
    crcOutput ## Stdout/stderr of code block execution

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

  CodeResFormat* = enum
    ## For the result; affects how Org processes results;
    crtCode
    crtDrawer
    crtHtml
    crtLatex
    crtLink
    crtGraphics
    crtOrg
    crtPP
    crtRaw

  CodeResHandling* = enum
    ## For inserting results once they are properly formatted.
    crtReplace
    crtSilent
    crtAppend
    crtPrepend

  CodeResult* = object
    # - TODO :: determine if (and how) results of multistage execution
    #  should be represented (compilation (potentially complex one) +
    #  execution)
    ## Result oc code block compilation and execution.
    execResult*: ShellResult


  CodeResExports* = enum
    creBoth ## Export both code and produced results
    creCode ## Only export original code
    creResults ## Only results
    creNone ## Do not export code block at all

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

  CodeEvalPost = object

  OrgFile* = object
    ## org-mode file object
    # FIXME this is a placeholder implementation, not supporting full
    # capabilities of org-mode file path formattin
    file*: FsFile

  OrgDir* = object
    ## org-mode directory object
    # FIXME this is a placeholder implementation, not supporting full
    # capabilities of org-mode directory path formatting
    dir*: FsDir

  CodeBlock* = ref object of RootObj
    evalSession*: Option[string]
    evalCache*: bool ## Avoids re-evaluating unchanged code blocks.
    evalVars*: Table[string, string]
    evalFile*: Option[OrgFile]
    evalFileDesc*: Option[string]
    evalDir*: Option[OrgDir]
    evalMkdirp*: bool
    evalPost*: Option[CodeEvalPost]
    evalShebang*: Option[string]

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
    evalComments*: CodeEvalComments
    evalEpilogue*: Option[string]
    evalPrologue*: Option[string]
    evalWhen*: CodeEvalWhen

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

    resExports*: CodeResExports
    resCollection*: CodeResCollection
    # TODO this field should be a `case` to support different `colnames`
    # properties, but only for relevant block types.
    resType*: CodeResType
    resFormat*: CodeResFormat
    resHandling*: CodeResHandling

    code*: string ## Source code body - possibly untangled from `noweb`
    ## block

    execResult*: Option[CodeResult] ## Result of code block execution might
    ## be filled from parsed source code or generated using code block
    ## evaluation stage. In latter case it is possible to determine
    ## differences between results and report them if necessary.


  CodeRunContext* = object
    # TODO also add cumulative hash for all code block sequences
    prevBlocks*: Table[string, seq[CodeBlock]] ## List of previous blocks for each session


  SymTable = ref object
    ## List of symbols that can be reference within documents. This mostly
    ## includes ``#+name``'d code blocks.

  CodeLink = object

  OrgLinkKind = enum
    olkWeb
    olkDoi
    olkFile
    olkAttachment
    olkDocview
    olkId
    olkInfo
    olkLisp
    olkHelp
    olkCode
    olkOtherLink

  OrgSearchTextKind = enum
    ostkPlaintext
    ostkHeadingTitle
    ostkHeadingId

  OrgLink = object
    case kind*: OrgLinkKind
      of olkWeb:
        webUrl*: Url

      of olkDoi:
        doi*: string

      of olkFile, olkAttachment, olkDocview:
        linkFile*: OrgFile
        lineNum*: Option[int]
        searchText*: Option[string]
        searchTextKind*: OrgSearchTextKind

      of olkId:
        linkId*: string

      of olkInfo:
        infoItem*: string

      of olkLisp:
        lispCode*: string

      of olkHelp:
        helpItem*: string

      of olkCode:
        codeLink*: CodeLink

      of olkOtherLink:
        linkFormat*: string
        linkBody*: string


      # of olkAttachment:
      #   attachFile*: OrgFile
      #   searchText*: string


  SemOrg* = ref object
    ## Rewrite of the parse tree with additional semantic information
    assocList*: Option[SemOrg] ## Reference to associative list
    symTable*: SymTable ## Reference to global list of named entries in
    ## document

    case isGenerated*: bool ## Can be `true` for sem nodes generated in
      ## subsequent stages (mostly code execution, but include directive
      ## resolution as well as several others can also produce new blocks)
      of false:
        slice*: Option[StrSlice]
        node* {.requiresinit.}: OrgNode ## Original org-mode parse tree node.

      of true:
        discard

    subnodes*: seq[SemOrg]

    case kind*: OrgNodeKind
      of onkSubtree:
        subtLevel*: int
        subtProperties*: Table[string, string]
        subtTitle*: OrgNode
        subtCompletion*: Option[OrgCompletion]
        subtTags*: seq[string]

      of onkSrcCode:
        codeBlock*: CodeBlock

      of onkAssocStmtList:
        attrs*: seq[tuple[name: string, body: SemOrg]]

      of onkMarkup:
        mark*: string
        content*: seq[SemOrg]

      of onkLink:
        linkTarget*: LinkTarget ## Optional reference to target node within
        ## document
        linkDescription*: SemOrg

      else:
        discard

var defaultRunConfig*: RunConfig

proc register*(lang: string, codeBuilder: CodeBuilder) =
  defaultRunConfig.codeCreateCallbacks[lang] = codeBuilder

proc newCodeBlock*(config: RunConfig, lang: string): CodeBlock =
  defaultRunConfig.codeCreateCallbacks[lang]()

proc add*(tree: var SemOrg, subtree: SemOrg) =
  assert tree.kind in orgSubnodeKinds
  tree.subnodes.add subtree

proc newSemOrg(node: OrgNode): SemOrg =
  SemOrg(kind: node.kind, isGenerated: false, node: node)

method runCode*(
    codeBlock: var CodeBlock,
    context: var CodeRunContext
  ) {.base.} =

  raiseAssert("#[ IMPLEMENT ]#")

method parseFrom*(codeBlock: var CodeBlock, semorg: var SemOrg) {.base.} =
  ## Parse code block body from semorg node. This method is called from
  ## top-level convert dispatcher loop using
  ## `parseFrom(semorg.codeBloc,semorg)` to trigger runtime dispatch.
  ## Overrides for this method can set only `codeBlock` argument, or modify
  ## `semorg` too, it doesn't really matter.
  raiseAssert("#[ IMPLEMENT ]#")

proc toSemOrg*(
    node: OrgNode,
    config: RunConfig = defaultRunConfig,
    scope: seq[TreeScope] = @[]
   ): SemOrg =

  case node:
    of SrcCode({ "lang" : @lang }):
      echov "Found semorg tree"
      result = newSemOrg(node)
      result.codeBlock = config.newCodeBlock($lang.text)

      # Actual execution of the code block will be handled by subsequent
      # pass.
      parseFrom(result.codeBlock, result)

    else:
      result = newSemOrg(node)
      if result.kind in orgSubnodeKinds:
        for subnode in node.subnodes:
          result.add toSemOrg(subnode, config, @[])

proc runCodeBlocks*(
    tree: var SemOrg,
    config: RunConfig = defaultRunConfig
  ) =
  ## Execute all code blocks in `SemOrg`.
  proc aux(tree: var SemOrg, context: var CodeRunContext) =
    case tree.kind:
      of orgTokenKinds:
        discard

      of orgSubnodeKinds:
        if tree.kind == onkSrcCode:
          runCode(tree.codeBlock, context)

        else:
          for subnode in mitems(tree.subnodes):
            aux(subnode, context)

      else:
        # Noweb and snippet nodes should have been already untangled and
        # replaced with regular semorg entries.
        raiseAssert("#[ IMPLEMENT ]#")

  var context: CodeRunContext
  aux(tree, context)
