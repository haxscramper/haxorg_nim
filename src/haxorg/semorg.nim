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

  OrgPropertyKind* = enum
    ## Built-in org properties such as `#+author`
    ##
    ## Explicitly lists all built-in properties and heaves escape hatch in
    ## form of `ockOtherProperty` for user-defined properties.
    ##
    ## Multi and single-line commands are compressed in single node kind,
    ## `onkCommand`

    opkTitle ## Main article title
    opkAuthor ## Author's name
    opkDate ## Article date
    opkEmail ## Author's email
    opkLanguage ## List of languages used in article
    opkUrl ## Url of the article
    opkSourceUrl ## Url of the article source

    opkToc ## Table of contents configuration
    opkAttr ## Export attributes for particular backend
    opkInclude ## `#+include` directive
    opkName ## `#+name`
    opkLinkAbbrev ## Link abbreviation definition
    ##
    ## https://orgmode.org/manual/Link-Abbreviations.html#Link-Abbreviations
    opkFiletags ## File-level tags
    ##
    ## https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance
    opkTagConfig # TODO https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance
    opkLatexHeader
    opkOtherProperty

  OrgProperty* = ref object of RootObj
    ## Built-in org-mode property.
    ##
    ## - NOTE :: This is only made into case object to allow for tons for
    ##   fields for /some/ properties such as `:lines` for `#+include`. You
    ##   should mostly use `kind` field and treat this as regular,
    ##   non-derived `ref`, only using conversion to get to particular
    ##   /property/ field.
    ##
    ## - TIP :: Each flag and slice is still stored as `StrSlice` to make
    ##   correct error messages possible in case of malformed arguments
    ##   passed.
    flags*: seq[StrSlice]
    args*: seq[tuple[key, val: StrSlice]]
    case kind*: OrgPropertyKind
      of opkAuthor, opkName, opkUrl:
        rawText*: string

      of opkTitle:
        text*: OrgNode

      of opkAttr:
        backend*: StrSlice ## `#+attr_<backend>`. All arguments are in
                           ## `flags` and `args`.

      of opkInclude:
        # TODO included file should support file search patterns
        # https://orgmode.org/manual/Include-Files.html
        # https://orgmode.org/manual/Search-Options.html#Search-Options
        includeFile*: OrgFile

      of opkLinkAbbrev:
        abbrevId*: StrSlice
        linkPattern*: StrSlice

      of opkFiletags:
        filetags*: seq[StrSlice]

      else:
        discard

  OrgCommandKind* = enum
    ## Built-in org commands (single and multiline) such as `#+include`
    ##
    ## Explicitly lists all built-in commands and heaves escape hatch in
    ## form of `ockOtherProperty` for user-defined properties.
    ##
    ## Properties can be transformed from single-line `onkCommand` entries,
    ## or directly from `onkProperty` in drawer elements (or `#+property`
    ## command)
    ockInclude
    ockSetupfile
    ockOtherCommand

  OrgCommand* = object
    case kind*: OrgCommandKind
      of ockInclude:
        discard

      else:
        discard

  OrgBigIdent* = enum
    obiNone

    obiMust = "MUST"
    ## MUST This word, or the terms "REQUIRED" or "SHALL", mean
    ## that the definition is an absolute requirement of the
    ## specification.

    obiMustNot = "MUST NOT"
    ## MUST NOT This phrase, or the phrase "SHALL NOT", mean that the
    ## definition is an absolute prohibition of the specification.

    obiShould = "SHOULD"
    ## SHOULD This word, or the adjective "RECOMMENDED", mean that there
    ## may exist valid reasons in particular circumstances to ignore a
    ## particular item, but the full implications must be understood and
    ## carefully weighed before choosing a different course.

    obiShouldNot = "SHOULD NOT"
    ## SHOULD NOT This phrase, or the phrase "NOT RECOMMENDED" mean that
    ## there may exist valid reasons in particular circumstances when the
    ## particular behavior is acceptable or even useful, but the full
    ## implications should be understood and the case carefully weighed
    ## before implementing any behavior described with this label.

    obiRequired = "REQUIRED"
    obiOptional = "OPTIONAL"
    ## MAY This word, or the adjective "OPTIONAL", mean that an item is
    ## truly optional. One vendor may choose to include the item because a
    ## particular marketplace requires it or because the vendor feels that
    ## it enhances the product while another vendor may omit the same item.
    ## An implementation which does not include a particular option MUST be
    ## prepared to interoperate with another implementation which does
    ## include the option, though perhaps with reduced functionality. In
    ## the same vein an implementation which does include a particular
    ## option MUST be prepared to interoperate with another implementation
    ## which does not include the option (except, of course, for the
    ## feature the option provides.)


    obiTodo      = "TODO"
    obiIdea      = "IDEA"
    obiError     = "ERROR"
    obiFixme     = "FIXME"
    obiDoc       = "DOC"
    obiRefactor  = "REFACTOR"
    obiReview    = "REVIEW"
    obiHack      = "HACK"

    obiWip       = "WIP"

    obiFix       = "FIX"
    obiClean     = "CLEAN"
    obiFeature   = "FEATURE"
    obiStyle     = "STYLE"
    obiRepo      = "REPO"
    obiSkip      = "SKIP"
    obiBreak     = "BREAK"

    obiNext      = "NEXT"
    obiLater     = "LATER"
    obiPostponed = "POSTPONED"
    obiStalled   = "STALLED"
    obiDone      = "DONE"
    obiPartially = "PARTIALLY"
    obiCancelled = "CANCELLED"
    obiFailed    = "FAILED"

    obiNote      = "NOTE"
    obiTip       = "TIP"
    obiImportant = "IMPORTANT"
    obiCaution   = "CAUTION"
    obiWarning   = "WARNING"

    obiUserCodeComment ## User-defined comment message
    obiUserCommitMsg ## User-defined commit message ident
    obiUserTaskState ## User-defined task state
    obiUserAdmonition ## User-defined admonition label

    obiOther ## User-defined big-idents, not included in default set.


  OrgMetaTag = enum
    omtArg      = "arg" ## Procedure argument
    omtParam    = "param" ## Generic entry parameter
    omtRet      = "ret" ## Procedure return value
    omtEnum     = "enum" ## Reference enum, enum value, or set of values.
    omtField    = "field" ## Entry field
    omtGroup    = "group" ## Entry group name
    omtFile     = "file" ## Filesystem filename
    omtDir      = "dir" ## Filesystem directory
    omtEnv      = "env" ## Environment variable
    omtKbdChord = "kdb" ## Keyboard chord
    omtKbdKey   = "key" ## Single keyboard key
    omtOption   = "option" ## CLI option
    omtAbbr     = "abbr" ## Abbreviation like CPS, CLI
    omtOther ## Undefined metatag


  SemOrg* = ref object of RootObj
    ## Rewrite of the parse tree with additional semantic information
    ##
    ## It provides much richer structure of the document AST with lots of
    ## different leaf node kinds, specifically designed for conversion to
    ## various backends. It still tries to keep close correspondse to
    ## original source code, though some information might be missing.
    ##
    ## General tree structure largely stays the same, except for several
    ## exceptions listed below:
    ##
    ## - NOTE :: Properties in associated statement list are saved in
    ##   `properties` field of the last node and saved into last node in
    ##   the associative list.
    ## - NOTE :: All multiline commands are converted to `onkProperty`.
    ## - NOTE :: Some single-line commands are mapped to properties - for
    ##   example ## `#+author` is mapped to property node, but `#+include`
    ##   stays as ## command.
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
    properties*: seq[OrgProperty] ## Property from associative list

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

      of onkCommand:
        command*: OrgCommand

      of onkProperty:
        property*: OrgProperty ## Standalone property

      of onkDocument:
        ## Document-level properties collected during conversion from parse
        ## tree.
        discard

      else:
        discard

const
  obiRfc2119Words* = {
    obiMust, obiMustNot,
    obiShould, obiShouldNot,
    obiRequired, obiOptional
  }

  obiCodeComments* = {
    obiTodo,
    obiIdea,
    obiError,
    obiFixme,
    obiDoc,
    obiRefactor,
    obiReview,
    obiHack,
    obiUserCodeComment
  }

  obiCommitMsg* = {
    obiFix,
    obiClean,
    obiFeature,
    obiStyle,
    obiRepo,
    obiHack,
    obiDoc,
    obiWip,
    obiBreak,
    obiSkip,
    obiUserCommitMsg
  }

  obiTaskStates* = {
    obiNext,
    obiLater,
    obiPostponed,
    obiStalled,
    obiDone,
    obiPartially,
    obiCancelled,
    obiFailed,
    obiUserTaskState
  }

  obiAdmonitions* = {
    obiNote,
    obiTip,
    obiImportant,
    obiCaution,
    obiWarning,
    obiUserAdmonition
  }

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
