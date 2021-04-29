{.experimental: "caseStmtMacros".}

import ast, buf
import std/[options, tables, strutils, strformat, uri,
            hashes, enumerate, sugar, sequtils]
import hpprint, hpprint/hpprint_repr
import hmisc/other/hshell
import hmisc/other/oswrap
import hmisc/types/colorstring
import hmisc/algo/[hlex_base, hparse_base, htree_mapping]
import hmisc/[hdebug_misc, hexceptions, helpers]
import nimtraits


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

  # LinkTarget* = object
  #   case isExternal*: bool
  #     of true:
  #       targetStr*: string

  #     of false:
  #       targetEntry*: SemOrg


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

  CodeResFormat* = enum
    ## For the result; affects how Org processes results;
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

  CodeEvalPost* = object

  OrgFile* = object
    ## org-mode file object
    # FIXME this is a placeholder implementation, not supporting full
    # capabilities of org-mode file path formatting
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

    codeHash*: Hash ## Hash for this particular code block source and
                    ## arguments
    cumulativeHash*: Hash ## Cumulative hash for all code block encountered
                          ## in the *same session* during top-down scan of
                          ## the document.

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

    langName*: string
    code*: string ## Source code body - possibly untangled from `noweb`
    ## block

    execResult*: Option[CodeResult] ## Result of code block execution might
    ## be filled from parsed source code or generated using code block
    ## evaluation stage. In latter case it is possible to determine
    ## differences between results and report them if necessary.


  CodeRunContext* = object
    # TODO also add cumulative hash for all code block sequences
    prevBlocks*: Table[string, seq[CodeBlock]] ## List of previous blocks
    ## for each session.



  SymTable* = ref object
    ## List of symbols that can be reference within documents. This mostly
    ## includes ``#+name``'d code blocks.

  CodeLinkType* = object
    ## Non-namespaced type in source code with generic parameters, like
    ## `seq[int]` or `vector<char>`.

    case isIdent*: bool
      of true:
        ## Regular identifier with zero or more generic parameters
        head*: string
        parameters*: seq[CodeLinkType]

      of false:
        ## Constant value - for languages like C++ and nim it can be
        ## anything - integer, or more complex expression like set of
        ## static enum values. Specific details are not handled by org-mode.
        value*: string

  CodeLinkPartKind* = enum
    clpDir
    clpFile
    clpNamespace
    clpStruct
    clpProc
    clpReturn
    clpArgument
    clpField
    clpPositional

    clpDoubleSlash # IMPLEMENT `xpath`-like double slash

  # CodeLinkType = object
  #   head*: string
  #   parameters*: seq[CodeLinkType]

  CodeLinkPart* = object
    partName*: string
    typeSelector*: string ## Type selector string
    case kind*: CodeLinkPartKind
      of clpProc:
        argumentTypes*: seq[CodeLinkType]

      of clpPositional:
        idx*: int

      else:
        discard

  CodeLink* = object
    ## Link to an entry in the source code
    ##
    ## #+begin_src nim
    ##   proc hello(arg: float): int =
    ## #+end_src
    ##
    ## Link to this proc would be `hello(int)`. If it is placed in
    ## `file.nim` it can also be adressed as `file/hello(int)`. Specific
    ## argument is `hello(int).arg`.

    refid*: Option[string] ## Resolved reference ID
    parts*: seq[CodeLinkPart] ## Code entry path


  OrgLinkKind* = enum
    olkOtherLink
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
    olkPage # Link to book page. Not yet designed, but probable contain
            # book name + page, and support some shortcut form of writing.


  OrgSearchTextKind* = enum
    ostkPlaintext
    ostkHeadingTitle
    ostkHeadingId

  OrgLink* = object
    ## Link to some external or internal entry.
    case kind*: OrgLinkKind
      of olkPage:
        discard

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

  OrgPropertyArg* = object
    key*: StrSlice
    value*: StrSlice

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
    args*: seq[OrgPropertyArg]
    case kind*: OrgPropertyKind
      of opkAuthor, opkName, opkUrl:
        rawText*: string

      of opkTitle:
        text*: SemOrg

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

  OrgBigIdentKind* = enum
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

    obiReallyShouldNot = "REALLY SHOULD NOT"
    obiOughtTo         = "OUGHT TO"
    obiWouldProbably   = "WOULD PROBABLY"
    obiMayWishTo       = "MAY WISH TO"
    obiCould           = "COULD"
    obiMight           = "MIGHT"
    obiPossible        = "POSSIBLE"

    obiTodo      = "TODO"
    obiIdea      = "IDEA"
    obiError     = "ERROR"
    obiFixme     = "FIXME"
    obiDoc       = "DOC"
    obiRefactor  = "REFACTOR"
    obiReview    = "REVIEW"
    obiHack      = "HACK"
    obiImplement = "IMPLEMENT"

    # http://antirez.com/news/124
    obiInternal  = "INTERNAL"
    obiDesign    = "DESIGN"
    obiWhy       = "WHY"

    obiWip       = "WIP"

    obiFix       = "FIX"
    obiClean     = "CLEAN"
    obiFeature   = "FEATURE"
    obiStyle     = "STYLE"
    obiRepo      = "REPO"
    obiSkip      = "SKIP"
    obiBreak     = "BREAK"
    obiPoc       = "POC"

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


  SemMetaTagKind* = enum
    smtArg      = "arg" ## Procedure argument
    smtParam    = "param" ## Generic entry parameter
    smtRet      = "ret" ## Procedure return value
    smtEnum     = "enum" ## Reference enum, enum value, or set of values.
    smtGlobal   = "global" ## Reference to global variable or constant
    smtAccs     = "accs" ## Documented access to external state (most often
                         ## global variable, file, or environment variable)
    smtField    = "field" ## Entry field
    smtGroup    = "group" ## Entry group name
    smtFile     = "file" ## Filesystem filename
    smtDir      = "dir" ## Filesystem directory
    smtEnv      = "env" ## Environment variable
    smtKbdChord = "kdb" ## Keyboard chord (multiple key combinations)
    smtKbdKey   = "key" ## Single keyboard key combination (key + modifiers)
    smtOption   = "option" ## CLI option
    smtSh       = "sh" ## Execute (simple) shell command
    smtAbbr     = "abbr" ## Abbreviation like CPS, CLI
    smtInject   = "inject" ## Identifier injected in scope
    smtNDSL     = "ndsl" ## Embedded nim DSL syntax description
    smtEDSL     = "edsl" ## Embedded DSL syntax description in Extended BNF
                         ## notation
    smtUnresolved ## Unresolved metatag. User-defined tags SHOULD be
                  ## converted to `smtOther`. Unresolved tag MIGHT be
                  ## treated as error/warning when generating final export.
    smtOther ## Undefined metatag

  SmtAccsKind* = enum
    oakRead
    oakWrite
    oakDelete
    oakCreate

  SemMetaTag* = ref object
    case kind*: SemMetaTagKind
      of smtAccs:
        accsKind*: set[SmtAccsKind]
        accsTarget*: SemMetaTag ## Access target. `@global{}`, `@file{}`,
                                ## `@dir{}`, `@env{}`


      of smtSh:
        shHasRoot*: bool
        shCmd*: ShellCmd

      else:
        discard

  SemItemTagKind* = enum
    sitText
    sitMeta
    sitBigIdent

  SemItemTag* = object
    case kind*: SemItemTagKind
      of sitMeta:
        meta*: SemMetaTag

      of sitBigIdent:
        idText*: string
        idKind*: OrgBigIdentKind

      of sitText:
        text*: SemOrg

  OrgAssocEntry* = object
    name*: string
    body*: SemOrg

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
    symTable* {.Skip(IO).}: SymTable ## Reference to global list of named entries in
    ## document

    case isGenerated* {.Skip(IO).}: bool ## Can be `true` for sem nodes generated in
      ## subsequent stages (mostly code execution, but include directive
      ## resolution as well as several others can also produce new blocks)
      of false:
        slice* {.Skip(IO).}: Option[StrSlice]
        node* {.requiresinit, Skip(IO).}: OrgNode ## Original org-mode parse tree node.

      of true:
        discard

    subnodes*: seq[SemOrg]
    properties*: Table[string, OrgProperty] ## Property from associative list

    case kind*: OrgNodeKind
      of onkSubtree:
        subtLevel*: int
        subtProperties*: Table[string, string]
        subtCompletion*: Option[OrgCompletion]
        subtTags*: seq[string]

      of onkSrcCode:
        codeBlock*: CodeBlock

      of onkAssocStmtList:
        attrs*: seq[OrgAssocEntry]

      of onkLink:
        linkTarget*: OrgLink ## Optional reference to target node within
        ## document
        linkDescription*: Option[SemOrg]

      of onkCommand:
        command*: OrgCommand

      of onkProperty:
        property*: OrgProperty ## Standalone property

      of onkBigIdent:
        bigIdentKind*: OrgBigIdentKind

      of onkDocument:
        ## Document-level properties collected during conversion from parse
        ## tree.
        discard

      of onkListItem:
        itemBullet*: string
        itemCounter*: Option[SemOrg]
        itemCheckbox*: Option[SemOrg]
        itemTag*: Option[SemItemTag]
        itemHeader*: SemOrg
        itemBody*: Option[SemOrg]

      of onkMetaTag:
        metaTag*: SemMetaTag

      else:
        discard

storeTraits(SemOrg)
storeTraits(OrgProperty)
storeTraits(OrgPropertyArg)
storeTraits(OrgFile)
storeTraits(OrgCompletion)
storeTraits(OrgAssocEntry)
storeTraits(CodeBlock)
storeTraits(CodeEvalPost)
storeTraits(CodeResult)
storeTraits(OrgDir)
storeTraits(OrgLink)
storeTraits(CodeLink)
storeTraits(CodeLinkPart)
storeTraits(CodeLinkType)
storeTraits(OrgCommand)
storeTraits(SemItemTag)
storeTraits(SemMetaTag)

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

proc subKind*(semorg: SemOrg): OrgNodeSubKind = semorg.node.subkind

proc register*(lang: string, codeBuilder: CodeBuilder) =
  defaultRunConfig.codeCreateCallbacks[lang] = codeBuilder

proc newCodeBlock*(config: RunConfig, lang: string): CodeBlock =
  defaultRunConfig.codeCreateCallbacks[lang]()

proc add*(tree: var SemOrg, subtree: SemOrg) =
  assert tree.kind in orgSubnodeKinds
  tree.subnodes.add subtree

iterator items*(tree: SemOrg): SemOrg =
  for subnode in tree.subnodes:
    yield subnode

proc len*(tree: SemOrg): int = tree.subnodes.len

func isEmptyNode*(tree: SemOrg): bool =
  tree.node.kind == onkEmptyNode

func `[]`*(tree: SemOrg, name: string): SemOrg =
  tree.subnodes[getNamedSubnode(tree.kind, name)]

func `[]`*(tree: SemOrg, idx: int): SemOrg =
  tree.subnodes[idx]

proc newSemOrg*(node: OrgNode): SemOrg =
  SemOrg(kind: node.kind, isGenerated: false, node: node)

proc newSemOrg*(kind: OrgNodeKind, subnodes: varargs[SemOrg]): SemOrg =
  SemOrg(kind: kind, isGenerated: true, subnodes: toSeq(subnodes))

proc newUnexpectedString*(
    entry: StrSlice,
    message: string,
    alternatives: openarray[string]
  ): CodeError =

  newCodeError(entry, message, stringMismatchMessage($entry, alternatives))

proc parseBaseBlockArgs(cb: CodeBlock, cmdArguments: OrgNode) =
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

proc updateContext*(codeBlock: CodeBlock, context: var CodeRunContext) =
  if codeBlock.evalSession.isSome():
    context.prevBlocks.mgetOrPut(
      codeBlock.evalSession.get(), newSeq[CodeBlock]()).add codeBlock

proc getSameSession*(
    codeBlock: CodeBlock, context: CodeRunContext): seq[CodeBlock] =

  if codeBlock.evalSession.isSome():
    context.prevBlocks.getOrDefault(
      codeBlock.evalSession.get(), @[codeBlock])

  else:
    @[codeBlock]


method runCode*(
    codeBlock: CodeBlock,
    context: var CodeRunContext
  ) {.base.} =

  raiseAssert("#[ IMPLEMENT ]#")

method parseFrom*(
  codeBlock: CodeBlock, semorg: SemOrg, scope: seq[TreeScope]) {.base.} =
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
   ): SemOrg

proc convertMetaTag*(
    tag: OrgNode, config: RunConfig, scope: seq[TreeScope]
  ): SemMetaTag =

  let tagKind = strutils.parseEnum[SemMetaTagKind]($tag["name"].text, smtUnresolved)
  return SemMetaTag(kind: tagKind)

type
  CodeLinkTokenKind = enum
    cltkIdent
    cltkLPar
    cltkRPar
    cltkLBrace
    cltkRBrace
    cltkExclamation
    cltkComma
    cltkSlash
    cltkDot
    cltkNum
    cltkPlaceholder

  CodeTok = HsTok[CodeLinkTokenKind]

proc `not`*[K](s: set[K]): set[K] = ({ low(K) .. high(K) } - s)

proc lexCodeLink(str: var PosStr): Option[CodeTok] =
  case str[]:
    of '_':
      if str['_', not {'_'}]:
        result = some initTok(str.popChar(), cltkPlaceholder)

      else:
        result = some initTok(str.popIdent(), cltkIdent)

    of IdentStartChars - {'_'}:
      result = some initTok(str.popIdent(), cltkIdent)

    of '[', ']', ',', '(', ')', '!', '/', '.':
      result = some initCharTok(str, {
        ']': cltkRBrace,
        '[': cltkLBrace,
        ')': cltkRPar,
        '(': cltkLPar,
        ',': cltkComma,
        '!': cltkExclamation,
        '/': cltkSlash,
        '.': cltkDot
      })

    of ' ':
      str.advance()

    of '`':
      result = some initTok(str.popBacktickIdent(), cltkIdent)

    of Digits:
      result = some initTok(str.popDigit(), cltkNum)

    else:
      raiseImplementError(&"[{str[]} (str[].int)] {$str}")



proc parseCodeLink*(str: var PosStr): CodeLink =
  var lexer = initLexer(str, lexCodeLink)

  let fileParse = parseTokenKind(cltkIdent) ^* parseTokenKind(cltkSlash)

  let res = fileParse(lexer).get()

  if res.len > 2:
    for item in res[0 .. ^2]:
      result.parts.add CodeLinkPart(kind: clpDir, partName: item.strVal())

  var hasDot = false
  if lexer[cltkDot]:
    hasDot = true
    if res.len > 1:
      result.parts.add CodeLinkPart(kind: clpDir, partName: res[^2].strVal())

    if res.len > 0:
      result.parts.add CodeLinkPart(kind: clpFile, partName: res[^1].strVal())

    lexer.advance()

  var selector: Option[string]
  if lexer[cltkIdent, cltkExclamation]:
    selector = some lexer[].strVal()
    lexer.advance(2)

  let name = lexer.pop(cltkIdent).strVal()

  if lexer[cltkLPar]:
    var namePart = CodeLinkPart(kind: clpProc, partName: name)
    if selector.isSome():
      namePart.typeSelector = selector.get()

    for tree in lexer.
        insideBalanced({cltkLPar}, {cltkRPar}).
        foldNested({cltkLBrace}, {cltkRBrace}, {cltkComma}):

      namePart.argumentTypes.add mapItDfs(
        tree, it.subnodes, CodeLinkType, not it.isToken) do:
          if it.isToken:
            CodeLinkType(
              isIdent: true, head: it.token.strVal())
          else:
            CodeLinkType(
              isIdent: true, head: it.rule.strVal(), parameters: subt)


    result.parts.add namePart


proc convertOrgLink*(
    link: OrgNode, config: RunConfig, scope: seq[TreeScope]
  ): OrgLink =

  var str = initPosStr(link[0].strVal())

  let format = str.popUntil({':'})
  str.advance()
  case format.normalize():
    of "code":
      return OrgLink(kind: olkCode, codeLink: parseCodeLink(str))



proc toSemOrg*(
    node: OrgNode,
    config: RunConfig = defaultRunConfig,
    scope: seq[TreeScope] = @[]
   ): SemOrg =


  template writeSubnodes(): untyped =
    if result.kind in orgSubnodeKinds:
      for subnode in node.subnodes:
        result.add toSemOrg(subnode, config, tern(
          node.kind == onkSubtree,
          scope & @[TreeScope(tree: result)],
          scope
        ))


  case node:
    of SrcCode({ "lang" : @lang }):
      result = newSemOrg(node)
      result.codeBlock = config.newCodeBlock($lang.text)

      parseFrom(result.codeBlock, result, scope)

    of BigIdent(text: @text):
      let ident = $text
      result = newSemOrg(node)
      result.bigIdentKind = strutils.parseEnum[OrgBigIdentKind]($text, obiOther)

    of ListItem[
      @bullet, @counter, @checkbox, @tag, @header, @completion, @body
    ]:
      result = newSemOrg(node)

      result.itemBullet = $bullet.text

      case tag:
        of Paragraph[BigIdent(text: @idText)]:
          result.itemTag = some(SemItemTag(
            kind: sitBigIdent,
            idText: $idText,
            idKind: strutils.parseEnum($idText, obiOther),
          ))

        of Paragraph[@tag is MetaTag()]:
          result.itemTag = some(SemItemTag(
            kind: sitMeta,
            meta: convertMetaTag(tag, config, scope)
          ))

      writeSubnodes()

    of Subtree[
      @prefix, @todo, @urgency, @title, @completion,
      @tags, @times, @drawers, @body
    ]:

      result = newSemOrg(node)

      result.subtLevel = len($prefix.text)
      result.subtTags = split($tags.text, ":")

      writeSubnodes()

    of MetaTag():
      result = newSemOrg(node)
      result.metaTag = convertMetaTag(node, config, scope)
      writeSubnodes()

    of Link():
      result = newSemOrg(node)
      result.linkTarget = convertOrgLink(node, config, scope)

      if node[0].kind != onkEmptyNode:
        result.linkDescription = some toSemOrg(node[0])

    else:
      result = newSemOrg(node)
      writeSubnodes()

proc toSemOrgDocument*(
    node: OrgNode,
    config: RunConfig = defaultRunConfig
  ): SemOrg =

  result = SemOrg(kind: onkDocument, isGenerated: true)
  result.add toSemOrg(node, config)

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


proc prettyPrintConverter*(
    entry: CodeLinkPart, conf: var PPRintConf, path: ObjPath): ObjTree =
  result = pptObj($entry.kind,
                  pptConst(entry.partName, conf.getStyling("string")))

proc objTreeRepr*(
  node: SemOrg, colored: bool = true, name: string = "<<fail>>"): ObjTree =
  let name = tern(
    name != "<<fail>>", &"({toGreen(name, colored)}) ", "")

  if node.isNil:
    return pptConst(name & toBlue("<nil>", colored))


  var subname = ""
  if not node.isGenerated:
    if node.node.subKind != oskNone:
      subname = &"[{toMagenta($node.node.subKind, colored)}]"

  case node.kind:
    of onkIdent:
      return pptConst(
        &"{name}{toItalic($node.kind, colored)} " &
        &"{subname} {toCyan($node.node.text, colored)}")

    of orgTokenKinds - {onkIdent, onkMarkup}:
      let txt = $node.node.text
      if '\n' in txt:
        result = pptObj(
          &"{name}{toItalic($node.kind, colored)}{subname}" &
            &"\n\"\"\"\n{toYellow(txt, colored)}\n\"\"\"")

      else:
        result = pptObj(
          &"{name}{toItalic($node.kind, colored)} " &
          &"{subname} \"{toYellow(txt, colored)}\"")

    of onkNowebMultilineBlock:
      raiseImplementError("")

    of onkSnippetMultilineBlock:
      raiseImplementError("")

    of onkSrcCode:
      result = pptConst($node.node.str)

    else:
      let mark = tern(
        not node.isGenerated and node.node.str.len > 0,
        &" <{toBlue(node.node.str, colored)}>",
        ""
      )


      var subnodes: seq[ObjTree]

      for name, value in fieldPairs(node[]):
        when name notin [
          "node", "subnodes", "symTable", "isGenerated", "kind",
          "properties"
        ]:

          when value is Option:
            if value.isSome():
              subnodes.add pptObj(
                name.toRed(colored).wrap("()"),
                objTreeRepr(value.get(), colored = colored)
              )

          else:
            subnodes.add pptObj(
              name.toRed(colored).wrap("()"),
              objTreeRepr(value, colored = colored)
            )

      for idx, subnode in enumerate(items(node)):
        subnodes.add objTreeRepr(
          subnode, colored, getSubnodeName(node.kind, idx))

      result = pptObj(
        &"{name}{toItalic($node.kind, colored)} {subname}{mark}",
        initStyle(), subnodes
      )


proc treeRepr*(tree: SemOrg, colored: bool = true): string =
  objTreeRepr(tree, colored = colored).treeRepr()
