import ./types
import hmisc/other/[hshell, hargparse]
import hmisc/core/all
import hmisc/other/oswrap
import std/algorithm
import std/options
import std/tables
import std/times
import std/macros
import std/uri
import std/sequtils
import
  hmisc/algo/[
    hlex_base,
    hparse_base,
    clformat
  ]



type
  CodeResult* = object
    # - TODO :: determine if (and how) results of multistage execution
    #  should be represented (compilation (potentially complex one) +
    #  execution)
    # Result oc code block compilation and execution.
    execResult*: Option[ShellResult]
    compileResult*: Option[ShellResult]


  CodeEvalPost* = object

  BlockCommand = ref object of RootObj

  CodeBlock* = ref object of BlockCommand
    ## Abstract root class for code blocks
    prevInSession*: CodeBlock ## Previous code block in the current session
                              ## level *or* direct parent session.
    blockArgs*: CliApp
    code*: string ## Source code body - possibly untangled from `noweb`
    ## block

    langName*: string

    execResult*: Option[CodeResult] ## Result of code block execution might
    ## be filled from parsed source code or generated using code block
    ## evaluation stage. In latter case it is possible to determine
    ## differences between results and report them if necessary.


  CodeRunContext* = object
    # TODO also add cumulative hash for all code block sequences
    prevBlocks*: Table[string, seq[CodeBlock]] ## List of previous blocks
    ## for each session.

type
  OrgImageFigure* = enum
    ## Specification for image figure placement

    oifNone

    oifWrap ## Reflow text around the image

  OrgImageSpec* = object
    ## Specification for image positioning

    width*: Option[OrgDimensions] ## Scale oroginal image to this width
    height*: Option[OrgDimensions] ## Scale original image to this height
    scale*: Option[float] ## Scale original image by a factor
    horizontal*: OrgHorizontalDirection ## Horizontal placement of the image
    figureKind*: OrgImageFigure ## How image is going to be placed in the
                                ## page

    rotate*: Option[float] ## Rotate original image before exporting


  OrgPropertyKind* = enum
    ## Built-in org properties such as `#+author`
    ##
    ## Explicitly lists all built-in properties and heaves escape hatch in
    ## form of `ockOtherProperty` for user-defined properties.
    ##
    ## Multi and single-line commands are compressed in single node kind,
    ## `orgCommand`

    # opkTitle ## Main article title
    # opkAuthor ## Author's name
    # opkDate ## Article date
    # opkEmail ## Author's email
    # opkLanguage ## List of languages used in article
    # opkUrl ## Url of the article
    # opkSourceUrl ## Url of the article source

    opkAttrImg
    opkToplevel
    opkBlocker
    opkCreated
    opkUnnumbered
    opkTrigger ## `:trigger:` org-depend property
    opkOrdered ## `:ordered:` property for subtrees
    opkNoblocking
    opkExportOptions ## General export options
    opkBackendExportOptions ## Backend-specific export options

    # opkToc ## Table of contents configuration
    opkAttrBackend ## Export attributes for particular backend

    opkColumnSpec ## Properties passed down from the column formatting
                  ## specification.

    # opkInclude ## `#+include` directive
    opkName ## `#+name`
    opkCaption ## `#+caption:`
    opkLinkAbbrev ## Link abbreviation definition
    ##
    ## https://orgmode.org/manual/Link-Abbreviations.html#Link-Abbreviations
    opkFiletags ## File-level tags
    ##
    ## https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance
    opkTagConf # TODO https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance
    opkLatexHeader
    opkOtherProperty
    opkId

  OrgUnnumberedKind* = enum
    ounNotoc
    ounTrue
    ounFalse

  # OrgPropertyArg* = object
  #   key*: PosStr
  #   value*: PosStr

type
  OrgLinkKind* = enum
    olkOtherLink
    olkWeb
    olkDoi
    olkFile
    olkInternal
    olkAttachment
    olkDocview
    olkId
    olkInfo
    olkLisp
    olkHelp
    olkCode
    olkCallout
    olkSubtree
    olkRadioLink
    olkImplicit
    olkPage # Link to book page. Not yet designed, but probable contain
            # book name + page, and support some shortcut form of writing.


  OrgUserLink* = ref object of RootObj
    defaultField: char

  OrgAnchorKind* = enum
    oakSemOrg
    oakFilePosition

  OrgAnchor* = object
    case kind*: OrgAnchorKind
      of oakSemOrg:
        targetNode*: SemOrg

      of oakFilePosition:
        targetFile*: tuple[file: AbsFile, line, column: int]

  OrgCodeLinkStepKind* = enum
    OLCodeProc  ## Code procedure
    OLCodeArg ## Procedure argument
    OLCodeResult ## Result of the procedure
    OLCodeShellCmd ## Shell command
    OLCodeShellFlag ## Shell command flag
    OLCodeShellArgument ## Shell command argument
    OLCodeShellSubcommand
    OLCodeClass ## Class/structure/object defined in code
    OLCodeField
    OLNamespace
    OLCodeModule
    OLCodeEnvVar


  OrgCodeType* = object
    name*: string
    params*: seq[OrgCodeType]
    
  OrgCodeArg* = object
    typ*: OrgCodeType
    arg*: Option[string]
    
  OrgCodeLinkStep* = object
    name*: string
    case kind*: OrgCodeLinkStepKind
      of OLCodeProc:
        arguments*: seq[OrgCodeArg]
        
      else:
        discard

  OrgCodeLink* = object
    steps*: seq[OrgCodeLinkStep]
    extended*: bool

  OrgLink* = object
    ## Link to some external or internal entry.
    anchor*: Option[OrgAnchor] ## Resolved link target
    description*: SemOrg
    case kind*: OrgLinkKind
      of olkWeb:
        webUrl*: Uri

      of olkDoi:
        doi*: string

      of olkFile, olkAttachment, olkDocview:
        linkFile*: OrgFile
        search*: Option[OrgInFileSearch]

      of olkCode:
        codeLink*: OrgCodeLink

      of olkId, olkInternal:
        linkId*: string

      of olkOtherLink:
        linkFormat*: string
        linkBody*: string

      else:
        discard

  SmtAccsKind* = enum
    oakRead
    oakWrite
    oakDelete
    oakCreate

  OrgHashTag* = object
    name*: string
    sub*: seq[OrgHashTag]
    
  OrgMetaTag* = ref object
    case kind*: OrgMetaTagKind
      of smtAccs:
        accsKind*: set[SmtAccsKind]
        accsTarget*: OrgMetaTag ## Access target. `@global{}`, `@file{}`,
                                ## `@dir{}`, `@env{}`


      of smtSh:
        shHasRoot*: bool
        shCmd*: seq[string]

      of smtImport:
        importLink*: OrgLink

      else:
        discard


#==============================  Subtrees  ===============================#
  SubtreeLogKind = enum
    SLogNote
    SLogRefile
    SLogClock
    SLogStateChange

  SubtreeLog = object
    time*: DateTime
    text*: SemOrg
    case kind: SubtreeLogKind
      of SLogClock:
        finish*: Option[DateTime]

      of SLogStateChange:
        oldState*: OrgBigIdentKind
        newState*: OrgBigIdentKind

      else:
        discard

  Subtree* = ref object
    level*: int
    todo*: Option[string]
    properties*: Table[tuple[name, subname: string], OrgProperty]
    completion*: Option[OrgCompletion]
    tags*: seq[string]
    title*: SemOrg
    description*: Option[SemOrg]
    logbook*: seq[SubtreeLog]

#================================  Lists  ================================#
  SemListItemTagKind* = enum
    ## Tag kinds for description list
    sitText ## Regular text
    sitBigIdent ## Only big idents

  SemListItemTag* = object
    case kind*: SemListItemTagKind
      of sitBigIdent:
        idText*: string
        idKind*: OrgBigIdentKind

      of sitText:
        text*: SemOrg

  OrgAnnotatedParagraphKind* = enum
    aopFootnote
    aopAdmonition
    aopListItem

  OrgFootnote* = object
    case inline*: bool
      of true:
        text*: SemOrg

      of false:
        ident*: string

  OrgAnnotatedParagraph* = object
    case kind*: OrgAnnotatedParagraphKind
      of aopListItem:
        tag*: SemListItemTag

      of aopFootnote:
        footnote*: OrgFootnote

      of aopAdmonition:
        admonition*: OrgBigIdentKind

    body*: SemOrg

  AssocEntry* = object
    name*: string
    body*: SemOrg

  SemListCheckboxState* = enum
    lcheckEmpty
    lcheckComplete

  SemListItem* = ref object
    bullet*: string
    counter*: Option[SemOrg]
    checkbox*: Option[SemListCheckboxState]
    header*: SemOrg
    body*: Option[SemOrg]


#==========================  Table parameters  ===========================#
  OrgCellSeparator* = enum
    ocsNone
    ocsSingleLine
    ocsDoubleLine

  OrgCell* = ref object
    body*: SemOrg ## Content of the cell

    horizontal*: OrgHorizontalDirection ## Horizontal alignment of the cell
    vertical*: OrgVerticalDirection

    width*: Option[OrgDimensions] ## Optional specification of the row
    ## width. Usually replicated from the wrapping table specification, but
    ## can be different on per-cell basis, if format was specified in
    ## individual `#+cell` entry

    height*: Option[OrgDimensions] ## Optional specification for height.
    ## Same rules as with `.width`, except per-row specification is
    ## originally copied into cell.

    borders*: tuple[top, bottom, left, right: OrgCellSeparator]


  OrgRow* = ref object
    text*: Option[SemOrg]
    cells*: seq[OrgCell]

    height*: Option[OrgDimensions] ## Target height for the row

    afterrow*: OrgCellSeparator ## Formatting specification for bottom row
                                ## border


  OrgTable* = ref object
    rows*: seq[OrgRow]
    widths*: seq[Option[OrgDimensions]] ## Target widths for each column
    columnCount*: int

    toprow*: OrgCellSeparator ## Border formatting parameters for the top
                              ## border of the first row. All other rows
                              ## store their respective formatting
                              ## specifications for the bottom parts.

    intercols*: seq[OrgCellSeparator] ## Intercolumn separator
    ## specification. All elements except the last one correspond to the
    ## formatting of the left border of the cell at idx. Last element
    ## corresponds to the right side of the table. So for `||_|_||` column
    ## formatting would be `@[ocsDoubleLine, ocsSingleLine, ocsDoubleLine]`


#===========================  Org-mode values  ===========================#
  OrgValueKind* = enum
    ## Kind of the org-mode value that org-mode can manipulate
    ovkString ## Regular (possibly multiline) stirng
    ovkInt ## Integer value
    ovkTable ## 2D table of values
    ovkEval ## Source code to be evaluated.
    ovkSem ## Chunk of the org-mode AST

  OrgValue* = object
    case kind*: OrgValueKind
      of ovkString:
        strVal*: string

      of ovkInt:
        intVal*: int

      of ovkTable:
        tableVal*: seq[seq[OrgValue]]

      of ovkEval:
        evalCode*: string

      of ovkSem:
        semVal*: SemOrg

#============================  File includes  ============================#
  OrgIncludeKind* = enum
    oikOrgInclude ## Including another org-mode file
    oikSrcInclude
    oikExportInclude

  OrgInclude* = object
    file*: OrgFile ## Target include file
    search*: Option[OrgInFileSearch] ## Seek to element in the target file
                                     ## before including
    linerange*: tuple[start, final: Option[int]] ## Range of lines to include
    vars*: Table[string, OrgValue]
    case kind*: OrgIncludeKind
      of oikSrcInclude:
        langname*: string ## Source code block kind for included language

      of oikExportInclude:
        backend*: seq[string] ## Target backend to embed included file's
         ## content into. Can contain multiple backends - in that case
         ## attempt to convert between input formats will be performed. Not
         ## all backends can converted into each other.

        newpage*: bool

      of oikOrgInclude:
        discard




#====================  Document-wide parametrization  ====================#
  OrgDocumentFlag* = enum
    odfSmartQuotes
    odfEmphasizedText
    odfSpecialStrings
    odfFixedWidthSections
    odfWithTimestamps
    odfPreserveBreaks
    odfLatexSuperscripts

    odfWithAuthor

    # ?
    odfArchHeadline
    odfArchHide
    odfArchExport

    odfWithSource ## Document either explicitly enabled source code setup,
                  ## or has at least one code block.

  OrgDocumentOptions* = object
    flags*: set[OrgDocumentFlag]
    tocMax*: Option[int] ## Max level of heading to include in the toc
    pagenum*: Option[int] ## Page number to start with

  OrgDocument* = object
    title*: SemOrg
    author*: SemOrg
    url*: Option[Url]
    exportname*: Option[string]
    options*: OrgDocumentOptions

    backendPage*: tuple[
      header, footer: Table[string, string]] ## Backend-specific page
    ## header and footer configurations.


  OrgProperty* = ref object of RootObj
    ## Built-in org-mode property.
    ##
    ## - NOTE :: This is only made into case object to allow for tons for
    ##   fields for /some/ properties such as `:lines` for `#+include`. You
    ##   should mostly use `kind` field and treat this as regular,
    ##   non-derived `ref`, only using conversion to get to particular
    ##   /property/ field.
    ##
    ## - TIP :: Each flag and slice is still stored as `PosStr` to make
    ##   correct error messages possible in case of malformed arguments
    ##   passed.
    # flags*: seq[PosStr]
    # args*: seq[OrgPropertyArg]
    case kind*: OrgPropertyKind
      # of opkAuthor, opkName, opkUrl:
      #   rawText*: string

      of opkOrdered:
        isOrdered*: bool

      of opkNoblocking:
        isBlocking*: bool

      of opkTrigger:
        triggers*: seq[tuple[id: string, state: Option[string]]]

      of opkBlocker:
        blockers*: seq[string]

      of opkUnnumbered:
        unnumberedKind*: OrgUnnumberedKind

      of opkCreated:
        createdAt*: DateTime

      of opkExportOptions:
        discard # TODO fill in elements

      of opkBackendExportOptions:
        exportBackendName*: string
        exportParameters*: string

      of opkAttrImg:
        image*: OrgImageSpec

      of opkAttrBackend:
        backend*: string ## `#+attr_<backend>`. All arguments are in
        ## `flags` and `args`.

      of opkLinkAbbrev:
        abbrevId*: string
        linkPattern*: string

      of opkFiletags:
        filetags*: seq[string]

      of opkColumnSpec:
        cellSpec*: tuple[
          width, height: Option[OrgDimensions]
        ]

      of opkCaption:
        caption*: SemOrg
        
      of opkId:
        id*: string

      else:
        discard

    
#==========================  Main semorg type  ===========================#
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
    assocList*: Option[SemOrg] ## Reference to associative list

    case isGenerated*: bool ## Can be `true` for sem nodes
      ## generated in subsequent stages (mostly code execution, but include
      ## directive resolution as well as several others can also produce
      ## new blocks)
      of true:
        str*: string

      of false:
        slice*: Option[PosStr]
        node* {.requiresinit.}: OrgNode ## Original org-mode parse tree
                                        ## node.

    parent*: SemOrg
    subnodes*: seq[SemOrg]
    properties*: seq[OrgProperty] ## Property from associative list

    # subkind*: OrgNodeSubKind
    case kind*: OrgNodeKind
      of orgTable:
        table*: OrgTable

      of orgSubtree:
        subtree*: Subtree

      of orgAnnotatedParagraph:
        paragraph*: OrgAnnotatedParagraph

      of orgSrcCode:
        codeBlock*: CodeBlock

      of orgAssocStmtList:
        rs*: seq[AssocEntry]

      of orgLink:
        link*: OrgLink

      of orgFootnote:
        footnoteTarget*: OrgFootnote

      of orgTimeStamp:
        time*: DateTime

      of orgProperty, orgAttrImg:
        property*: OrgProperty ## Standalone property

      of orgBigIdent:
        bigIdentKind*: OrgBigIdentKind

      of orgCommandInclude:
        includeSpec*: OrgInclude

      of orgCommandOptions:
        documentOptions*: OrgDocumentOptions

      of orgDocument:
        ## Document-level properties collected during conversion from parse
        ## tree.
        document*: OrgDocument

      of orgListItem:
        listItem*: SemListItem

      of orgMetaSymbol:
        metaTag*: OrgMetaTag

      of orgHashTag:
        hashTag*: OrgHashTag

      else:
        discard

#=========================  Conversion context  ==========================#
  SymKind* = enum
    symNamed
    symRadioTarget
    symSubtree
    symRegularTarget
    symCalloutTarget

  TreeScope* = object
    ## Subtree scope. Mostly used for internal implementation in sempass
    tree*: SemOrg

  SemOrgCtx* = object
    symTable*: array[SymKind, Table[string, SemOrg]]
    scope*: seq[TreeScope]
    associative*: seq[OrgNode]

    sessionTails*: Table[seq[string], CodeBlock]

    fileStack*: seq[AbsFile]
    kindStack*: seq[OrgNodeKind]

    fixSize*: tuple[width, height: Option[OrgDimensions]]

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

type
  OrgStmtGroup = object
    elements: seq[OrgNode]
    closed: bool




    
iterator items*(node: SemOrg, allowed: set[OrgNodeKind] = {}): SemOrg =
  for item in node.subnodes:
    if allowed.empty() or item of allowed:
      yield item

iterator pairs*(node: SemOrg): tuple[idx: int, sem: SemOrg] =
  for idx, item in node.subnodes:
      yield (idx, item)

func itemsDFS*(node: SemOrg, allowed: set[OrgNodeKind] = {}): seq[SemOrg] =
  proc aux(node: SemOrg, res: var seq[SemOrg]) =
    if allowed.empty() or node of allowed:
      res.add node
    
    for item in items(node):
      aux(item, res)

  aux(node, result)

proc nestedLeavesDfs*(
    node: SemOrg,
    allowed: proc(sem: SemOrg): bool = nil,
    endrecurse: proc(sem: SemOrg): bool = nil
  ): seq[SemOrg] =

  var shit: seq[SemOrg] # HACK doing the same thing with `result` causes
                        # `cannot be captured` error. Why? no idea.
  proc aux(node: SemOrg) =
    if notNil(endrecurse) and endrecurse(node): return
    if isNil(allowed) or allowed(node): shit.add node

    case node.kind:
      of orgSubtree:
        if node.subtree.description.canGet(it):
          aux(it)

        for sub in node:
          aux(sub)

      of orgListItem:
        aux(node.listItem.header)
        if node.listItem.body.canGet(it):
          aux(it)


      of orgContainerLikeKinds +
         orgTokenKinds +
         orgTokenLikeKinds:
        for sub in node:
          aux(sub)

      else:
        discard

  aux(node)
  return shit


func allSubtrees*(node: SemOrg): seq[SemOrg] =
  for item in itemsDFS(node):
    if item of orgSubtree:
      result.add item
  
func len*(node: SemOrg): int = node.subnodes.len()
      
func line*(org: SemOrg): int = tern(org.node.isNil(), -1, org.node.line)
func column*(org: SemOrg): int = tern(org.node.isNil(), -1, org.node.column)
func strVal*(org: SemOrg): string =
  tern(org.node.isNil(), "", org.node.strVal())

type
  SemOrgReprFlag* = enum
    sorfSkipParagraph
    sorfNoRecurse
  
  SemOrgReprConf = object
    flags: set[SemOrgReprFlag]

const defaultSemOrgReprConf* = SemOrgReprConf(
  flags: {
    sorfSkipParagraph,
  }
)

func `-`*(conf: sink SemOrgReprConf, flag: SemOrgReprFlag): SemOrgReprConf =
  result = conf
  result.flags.excl flag

func `+`*(conf: sink SemOrgReprConf, flag: SemOrgReprFlag): SemOrgReprConf =
  result = conf
  result.flags.incl flag

proc treeRepr*(
    org: SemOrg, 
    conf: SemOrgReprConf = defaultSemOrgReprConf,
    opts: HDisplayOpts = defaultHDisplay
  ): ColoredText =

  coloredResult()

  proc aux(n: SemOrg, level: int, name: Option[string] = none(string)) =
    addIndent(level)
    if isNil(n):
      add hshow(nil, opts)
      return

    add hshow(n.kind)

    if opts.withRanges:
      add " "
      add hshow(n.line, opts)
      add ":"
      add hshow(n.column, opts)


    if name.isSome():
      add " "
      add toCyan(name.get())


    proc addField(name: string) = 
      add "."
      add name + fgCyan
      add " ="
      
    template auxField(name: untyped, tree: untyped): untyped =
      add "\n"
      addIndent(level + 1)
      addField(astToStr(name))
      add "\n"
      aux(tree.name, level + 2)

    proc addFieldi(name: string) =
      add "\n"
      addIndent(level + 1)
      addField(name)
      
      
    proc addField(name: string, expr: ColoredText) =
      addFieldi(name)
      add " "
      add expr

    case n.kind:
      of orgRawLink:
        add " "
        add n.strVal()
      
      of orgLink:
        let link = n.link
        add "("
        add hshow(link.kind)
        add ") "
        case link.kind:
          of olkId:
            addField("link.linkId", hshow(link.linkId))

          of olkWeb:
            addField("link.webUrl", $link.webUrl + fgCyan)
          
          else:
            raise newUnexpectedKindError(link)
      
      of orgWord, orgSpace:
        add " "
        add hshow(n.strVal())


      of orgListItem:
        let item = n.listItem
        addField("listItem.bullet", hshow(item.bullet))
        if item.checkbox.canGet(it):
          addField("listItem.checkbox", hshow(it))

        # if item.tag.canGet(tag):
        #   case tag.kind:
        #     of sitBigIdent:
        #       addField("listItem.tag.idKind", hshow(tag.idKind))
        #       addField("listItem.tag.idtext", hshow(tag.idKind))

        #     of sitText:
        #       addFieldi("listItem.tag")
        #       add("\n")
        #       aux(tag.text, level + 2)

        if item.body.canGet(body):
          addFieldi("listItem.body")
          add "\n"
          aux(body, level + 2)

      
      of orgSubtree:
        let tree = n.subtree
        addField("subtree.level", hshow(tree.level))
        auxField(title, tree)
        if tree.completion.isSome():
          let completion = tree.completion.get()
          var text: ColoredText
          if completion.isPercent:
            addField("subtree.completion.percent", hshow(completion.percent))
          else:
            addField("subtree.completion.done", hshow(completion.done))
            addField("subtree.completion.total")
            add " "
            add hshow(completion.total)
           
        
        if 0 < tree.properties.len():
          addFieldi("properties")
          for kind, prop in tree.properties:
            case prop.kind:
              of opkId:
                addField("id", hshow(prop.id))

              of opkBackendExportOptions:
                addField("exportBackendName", hshow(prop.exportBackendName))
                addField("exportParameters", hshow(prop.exportParameters))

              of opkOrdered:
                addField("isOrdered", hshow(prop.isOrdered))

              of opkNoblocking:
                addField("isBlocking", hshow(prop.isBlocking))

              of opkBlocker:
                addFieldi("blocker")
                for it in prop.blockers:
                  add "\n"
                  addIndent(level + 1)
                  add hshow(it)

              of opkTrigger:
                addFieldi("triggers")
                for (id, state) in prop.triggers:
                  add "\n"
                  addIndent(level + 2)
                  add hshow(id)
                  if state.isSome():
                    add " "
                    add hshow(state.get)

              else:
                raise newUnexpectedKindError(prop)
      
      of orgTimeStamp:
        add " "
        add $n.time
      
      else:
        discard
    

    if n of orgParagraph and sorfSkipParagraph in conf.flags:
      add " "
      add hshow(n.len())
      add " "
      add "items"
      
    else:
      if sorfNoRecurse notin conf.flags:
        for sub in items(n):
          add "\n"
          aux(sub, level + 1)


  aux(org, 0, none(string))
  endResult()

      
    
    
func newSem*(kind: OrgNodeKind, node: OrgNode, parent: SemOrg): SemOrg =
  SemOrg(kind: kind, node: node, isGenerated: false, parent: parent)

func newSem*(kind: OrgNodeKind, parent: SemOrg): SemOrg =
  SemOrg(kind: kind, isGenerated: true, parent: parent)

func newEmptySem*(): SemOrg =
  SemOrg(kind: orgEmpty, isGenerated: true, parent: nil)
  
func newSem*(node: OrgNode, parent: SemOrg): SemOrg =
  SemOrg(kind: node.kind, node: node, isGenerated: false, parent: parent)


func newSem*(
  kind: OrgNodeKind, parent: SemOrg, subnodes: seq[SemOrg]): SemOrg =
  result = SemOrg(kind: kind, isGenerated: true, parent: parent)
  result.subnodes = subnodes
  
func `add`(node: var SemOrg, other: SemOrg) =
  node.subnodes.add other

func `[]`*(node: SemOrg, idx: int | BackwardsIndex): SemOrg =
  node.subnodes[idx]

func `[]`*(
    node: SemOrg,
    idx: Slice[int] | Slice[BackwardsIndex] | HSlice[int, BackwardsIndex]
  ): seq[SemOrg] =

  node.subnodes[idx]

func first*(node: SemOrg): SemOrg = node[0]
func last*(node: SemOrg): SemOrg = node[^1]

proc toSemOrg*(node: OrgNode, parent: SemOrg): SemOrg

proc group(item: OrgNode): OrgStmtGroup =
  OrgStmtGroup(elements: @[item])

proc add(group: var OrgStmtGroup, item: OrgNode) =
  group.elements.add item

proc expand(group: OrgStmtGroup): seq[OrgStmtGroup] =
  for item in group.elements:
    result.add group(item)

proc lastGrouped(groups: seq[OrgStmtGroup]): bool =
  not groups.empty() and 1 < groups[^1].elements.len()

proc attachable*(group: OrgStmtGroup): bool =
  group.elements[0] of orgAttachableKinds

proc foldGroups*(elements: seq[OrgNode]): seq[OrgStmtGroup] =
  for node in elements:
    case node.kind:
      of orgAttachableKinds:
        result.last(OrgStmtGroup()).add node

      of orgAssociatedKinds:
        # If associated node is found and last group is attachable, push
        # the element to it and closed the group.
        if not result.empty() and result.last().attachable():
          result.last().add node
          result.last().closed = true

        elif node of orgResult and
             not result.empty() and 
             result.last().elements.last() of orgSrcCode:
          if node of orgQuoteBlock: ploc()
          result.last().add node

        else:
          if not result.empty():
            result.add result.pop().expand()
            
          result.add group(node)

      else:
        if result.lastGrouped() and not result.last().closed:
          result.add result.pop().expand()

        result.add group(node)

func getPrevNode*(sem: SemOrg): Option[SemOrg] =
  if sem.parent.notNil():
    for idx, node in sem.parent:
      if cast[int](sem.parent[idx]) == cast[int](sem):
        if idx == 0:
          return

        else:
          return some sem.parent[idx - 1]

func getProperty*(
  tree: Subtree, name: string, subname: string = ""): Option[OrgProperty] =
  let
    name = name.normalize()
    subname = subname.normalize()

  if (name, subname) in tree.properties:
    return some tree.properties[(name, subname)]

func getAllProperties*(tree: Subtree, name: string): seq[OrgProperty] =
  ## Get all properties with specified main name and any subname
  let name = name.normalize()
  for namePair, value in tree.properties:
    if namePair.name == name:
      result.add value

proc convertProperty*(prop: OrgNode, parent: SemOrg): OrgProperty =
  case prop.kind:
    of orgCommandCaption:
      result = OrgProperty(kind: opkCaption)
      result.caption = toSemOrg(prop[0], parent)
      
    else:
      raise newUnexpectedKindError(prop)
        
proc convertStmtGroup*(group: OrgStmtGroup, parent: SemOrg): SemOrg =
  let last = group.elements.last()
  case last.kind:
    of orgAllKinds - orgAssociatedKinds:
      assert group.elements.len() == 1
      result = toSemOrg(last, parent)

    of orgQuoteBlock:
      result = newSem(last, parent)
      for prop in group.elements[0..^2]:
        result.properties.add convertProperty(prop, parent)

    of orgCommandInclude:
      result = newSem(last, parent)
      assert group.elements.len() == 1

    else:
      raise newUnexpectedKindError(last)

macro unpackNode(node: OrgNode, subnodes: untyped{nkBracket}): untyped =
  result = newStmtList()
  for idx, name in subnodes:
    result.add newVarStmt(
      name, nnkBracketExpr.newtree(node, newLit(name.strVal())))

proc convertTime*(node: OrgNode, parent: SemOrg): SemOrg
  
proc toSemProperty*(prop: OrgNode):
  tuple[name, subname: string, value: OrgProperty] =
  prop.unpackNode([name, subname, values])
  result.name = name.strVal().strip(chars = {':'}).normalize()
  result.subname = subname.strVal().strip(chars = {':'}).normalize()
  case result.name.normalize():
    of "id":
      result.value = OrgProperty(kind: opkId, id: values.strVal())

    of "exportoptions":
      if subname of orgEmpty:
        assert false, $prop.treeRepr()

      else:
        result.value = OrgProperty(
          kind: opkBackendExportOptions,
          exportBackendName: subname.strVal(),
          exportParameters: values.strVal().strip()
        )

    of "ordered":
      result.value = OrgProperty(
        kind: opkOrdered, isOrdered: values.strVal() in ["t"])

    of "noblocking":
      result.value = OrgProperty(
        kind: opkNoblocking, isBlocking: values.strVal() notin ["t"])

    of "blocker":
      result.value = OrgProperty(
        kind: opkBlocker, blockers: @[values.strVal().strip()])

    of "unnumbered":
      result.value = OrgProperty(
        kind: opkUnnumbered,
        unnumberedKind:
          case values.strVal().strip().normalize():
            of "notoc": ounNotoc
            of "t": ounTrue
            of "nil": ounFalse
            else: assert(false, values.strVal()) ; ounNotoc
      )

    of "created":
      result.value = OrgProperty(
        kind: opkCreated,
        createdAt: convertTime(values, nil).time)
    
    of "trigger":
      let
        trigger = values.strVal()
        idRange = trigger.find('(')

      result.value = OrgProperty(
        kind: opkTrigger,
        triggers: @[(
          id: trigger[0 ..< tern(idRange == -1, len(trigger), idRange)],
          state: tern(
            idRange == -1,
            none(string),
            some(trigger[idRange + 1 .. ^2])
          )
        )]
      )

    else:
      raise newUnexpectedKindError(result.name, treeRepr(prop))

proc mergeFlatProperties*(props: seq[OrgProperty]): OrgProperty =
  let first = props[0]
  case first.kind:
    of opkId,
       opkOrdered,
       opkNoblocking,
       opkUnnumbered,
       opkCreated:
      # QUESTION more than one ID per tree?
      return props.last()

    of opkBackendExportOptions:
      var values: seq[string]
      for prop in props:
        values.add prop.exportParameters

      result = OrgProperty(
        kind: opkBackendExportOptions,
        exportBackendName: first.exportBackendName,
        exportParameters: values.join(" ")
      )

    of opkBlocker:
      result = OrgProperty(
        kind: opkBlocker,
        blockers: mapIt(props, it.blockers).concat()
      )

    of opkTrigger:
      result = OrgProperty(
        kind: opkTrigger,
        triggers: mapIt(props, it.triggers).concat()
      )

    else:
      raise newUnexpectedKindError(first, $first[])

const opkNonContextual* = {
  opkTrigger,
  opkBlocker
} ## Non-contextual properties such as trigger lists.

proc mergeContextualProperties*(props: seq[OrgProperty]): OrgProperty =
  ## Merge properties for context-based grouping. Most properties stack on
  ## each other but sometimes only innermost node must contain the
  ## definition.
  let inner = props[0]
  case inner.kind:
    of opkNonContextual:
      return inner

    else:
      return mergeFlatProperties(props)

iterator ascending*(sem: SemOrg, withSelf: bool = true): SemOrg =
  var parent = tern(withSelf, sem, sem.parent)
  while parent.notNil():
    yield parent
    parent = parent.parent

func parentTree*(sem: SemOrg): Option[SemOrg] =
  for tree in ascending(sem, withSelf = false):
    if tree of orgSubtree:
      return some tree
    
    
proc getContextualProperty*(
    sem: SemOrg, name: string, subname: string = ""): Option[OrgProperty] =
  var props: seq[OrgProperty]
  for parent in ascending(sem):
    if parent.kind != orgSubtree:
      continue

    if parent.subtree.getProperty(name, subname).canGet(prop):
      props.add prop

  if props.empty():
    return

  props.reverse()
  return some mergeContextualProperties(props)

proc convertTime*(node: OrgNode, parent: SemOrg): SemOrg =
  if node.kind == orgTimeRange:
    result = newSem(orgTimeRange, node, parent)
    result.add convertTime(node[0], result)
    result.add convertTime(node[1], result)

  else:
    result = newSem(orgTimeStamp, node, parent)
    var parseOk = false
    let str = node.strVal().
      strip(leading = false, trailing = true, chars = {']', '>'}).
      strip(leading = true, trailing = false, chars = {'[', '<'})
      
    for pattern in @[
      "yyyy-MM-dd ddd HH:mm:ss",
      "yyyy-MM-dd ddd HH:mm",
      "yyyy-MM-dd HH:mm:ss",
      "yyyy-MM-dd HH:mm",
      "yyyy-MM-dd ddd",
      "yyyy-MM-dd"
    ]:
      try:
        result.time = parse(str, pattern)
        parseOk = true
        break

      except TimeParseError:
        discard

    if not parseOk:
      echov str

  
proc convertSubtree*(node: OrgNode, parent: SemOrg): SemOrg =
  result = newSem(node, parent)

  node.unpackNode(
    [prefix, todo, urgency, title, completion, tags, times, drawer, body])

  var tree = Subtree()

  if not (todo of orgEmpty):
    tree.todo = some todo.strVal()

  if not(drawer["properties"] of orgEmpty):
    var proptable: Table[(string, string), seq[OrgProperty]]
    for prop in drawer["properties"]:
      let (nameStr, subnameStr, values) = toSemProperty(prop)
      proptable.mgetOrPut((nameStr, subnameStr), @[]).add values

    for name, properties in pairs(proptable):
      tree.properties[name] = mergeFlatProperties(properties)

  if not(drawer["description"] of orgEmpty):
    tree.description = some toSemOrg(
      drawer["description"]["text"], parent)

  if not(tags of orgEmpty):
    for tag in tags:
      tree.tags.add tag.strVal()

  if not(drawer["logbook"] of orgEmpty):
    let logbook = drawer["logbook"]
    for entry in logbook:
      var log: SubtreeLog
      case entry.kind:
        of orgLogbookNote:
          log = SubtreeLog(kind: SLogNote)
          log.time = entry["time"].toSemOrg(result).time
          log.text = entry["text"].toSemOrg(result)

        of orgLogbookRefile:
          log = SubtreeLog(kind: SLogRefile)
          log.time = entry["time"].toSemOrg(result).time
          log.text = entry["text"].toSemOrg(result)

        of orgLogbookStateChange:
          log = SubtreeLog(kind: SLogStateChange)
          log.time = entry["time"].toSemOrg(result).time
          log.text = entry["text"].toSemOrg(result)

          if not(entry["oldstate"] of orgEmpty):
            log.oldState = parseEnum[OrgBigIdentKind](
              entry["oldstate"].strVal())

          log.newState = parseEnum[OrgBigIdentKind](
            entry["newstate"].strVal())

        of orgLogbookClock:
          log = SubtreeLog(kind: SLogClock)
          let time = entry["time"].toSemOrg(result)
          if time of orgTimeRange:
            log.time = time[0].time
            log.finish = some time[1].time

          else:
            log.time = time.time

        else:
          discard

      tree.logbook.add log

  tree.level = prefix.strVal().count('*')
  tree.title = toSemOrg(title, result)
  # First convert tree to the semorg entries and then add to subnodes.
  for item in toSemOrg(body, result):
    item.parent = result
    result.add item

  result.subtree = tree

type
  CodeLinkTokenKind = enum
    cltIdent
    cltKindFix
    cltNamespace
    cltDot
    cltSlash
    cltExtendStep
    cltLPar
    cltRPar
    cltTemplateOpen
    cltTemplateClose
    cltUnderscore
    cltComma
    cltColon
    cltDollar

  CodeLinkToken = HsTok[CodeLinkTokenKind]
  CodeLinkLexer = HsLexer[CodeLinkToken]

func toCodeLinkStepKind(name: string): OrgCodeLinkStepKind =
  case name.normalize():
    of "class": OLCodeClass
    of "proc": OLCodeProc
    else:
      raise newUnexpectedKindError(name)

proc parseCodeLink*(link: string): OrgCodeLink =
  result = OrgCodeLink()
  var str = initPosStr(link)
  var tokens: seq[CodeLinkToken]
  const singleTokens = {
    ',': cltComma,
    '!': cltKindFix,
    '(': cltLPar,
    ')': cltRPar,
    '+': cltExtendStep,
    '.': cltDot,
    '/': cltSlash,
    '<': cltTemplateOpen,
    '>': cltTemplateClose,
    ':': cltColon,
    '$': cltDollar
  }
  while ?str:
    case str[]:
      of IdentChars:
        tokens.addInitTok(str, cltIdent):
          str.skipWhile(IdentChars)

      of '`':
        str.skip('`')
        tokens.addInitTok(str, cltIdent):
          str.skipTo('`')

        str.skip('`')

      of toKeySet(singleTokens):
        tokens.add str.initAdvanceTok(1, mapEnum(str[], singleTokens))

      of ' ':
        str.space()

      else:
        raise newUnexpectedCharError(str)

  var lex = str.initLexer(tokens)
  while ?lex:
    case lex[].kind:
      of cltIdent:
        if not lex.hasNxt(1):
          case lex[-1].kind:
            of cltDot:
              result.steps.add OrgCodeLinkStep(
                kind: OLCodeField,
                name: lex.pop().strVal()
              )

            of cltColon:
              if result.steps.last() of OLCodeProc:
                result.steps.add OrgCodeLinkStep(
                  kind: OLCodeArg,
                  name: lex.pop().strVal()
                )
                
              else:
                assert false, $lex

            else:
              raise newUnexpectedKindError(lex[-1])
        
        elif lex[+1] of cltKindFix:
          let kind = lex.pop(cltIdent)
          lex.skip(cltKindFix)
          let name = lex.pop(cltIdent)
          result.steps.add OrgCodeLinkStep(
            kind: kind.strVal().toCodeLinkStepKind(),
            name: name.strVal(),
          )

        elif lex[+1] of cltLPar:
          result.steps.add OrgCodeLinkStep(
            kind: OLCodeProc,
            name: lex.pop().strVal()
          )

        elif lex[+1] of cltSlash:
          result.steps.add OrgCodeLinkStep(
            kind: OLCodeModule,
            name: lex.pop().strVal()
          )
          
          lex.next()

        elif not result.steps.empty() and lex[-1] of cltSlash:
          # `/module
          result.steps.add OrgCodeLinkStep(
            kind: OLCodeModule,
            name: lex.pop().strVal()
          )

        else:
          assert false, $lex

      of cltDot:
        lex.next()

      of cltColon:
        lex.next()

      of cltExtendStep:
        lex.skip(cltExtendStep)
        result.extended = true

      of cltDollar:
        lex.skip(cltDollar)
        result.steps.add OrgCodeLinkStep(
          kind: OLCodeEnvVar,
          name: lex.pop(cltIdent).strVal()
        )

      of cltLPar:
        template wrappedIn(
          lex, start, finish, sep, body: untyped): untyped =
          if lex[start]:
            lex.skip(start)
            while not lex[finish]:
              body
              if not lex[finish]:
                lex.skip(sep)
            lex.skip(finish)
        
        proc parseTyp(lex: var CodeLinkLexer): OrgCodeType =
          result.name = lex.pop(cltIdent).strVal()
          lex.wrappedIn(cltTemplateOpen, cltTemplateClose, cltComma):
            result.params.add parseTyp(lex)
              
        proc parseArgument(lex: var CodeLinkLexer): OrgCodeArg =
          result.typ = parseTyp(lex)
          if lex[cltIdent]:
            result.arg = some lex.pop(cltIdent).strVal()
        
        assert result.steps.last() of OLCodeProc
        lex.wrappedIn(cltLPar, cltRPar, cltComma):
          result.steps.last().arguments.add parseArgument(lex)

      else:
        raise newUnexpectedKindError(lex[], $lex)

proc toSemLink*(node: OrgNode, parent: SemOrg): SemOrg =
  result = newSem(node, parent)

  proc parseFileLink(text: string): tuple[
    file: OrgFile, search: Option[OrgInFileSearch]] =
    # TODO analyze file relative position, pass the location of the
    # surrounding file, parse in-file search information
    result.file = OrgFile(isRelative: true, relFile: RelFile(text))
  
  case node["link"].kind:
    of orgRawText:
      let text = node["link"].strVal()
      let protocol = tern(
        node["protocol"] of orgEmpty, "", node["protocol"].strVal()
      ).normalize()

      case protocol:
        of "id":
          result.link = OrgLink(kind: olkId, linkId: text)

        of "http", "https":
          result.link = OrgLink(kind: olkWeb, webUrl: parseUri(text))

        of "file":
          let (file, search) = parseFileLink(text)
          result.link = OrgLink(kind: olkFile, linkFile: file, search: search)

        of "code":
          result.link = OrgLink(kind: olkCode, codeLink: parseCodeLink(text))

        elif protocol == "" and text.startsWith("http"):
          result.link = OrgLink(kind: olkWeb, webUrl: parseUri(text))

        elif protocol == "":
          result.link = OrgLink(kind: olkInternal, linkId: text)

        else:
          raise newUnexpectedKindError(
            protocol, text & "\n\n" & $node.treeRepr())

    else:
      raise newUnexpectedKindError(node["link"])

  result.link.description = toSemOrg(node["desc"], result)
  assertRef(result.link.description, $node["desc"].kind)
     

proc convertStmtList*(node: OrgNode, parent: SemOrg): SemOrg = 
  result = newSem(node, parent)
  for group in foldGroups(node.subnodes):
    result.add convertStmtGroup(group, result)

proc toSemOrgTodo(todo: string): OrgBigIdentKind =
  ## Convert org-mode big ident to the todo keyword. Return 'other' on the
  ## unknown keyword kinds.
  parseEnum[OrgBigIdentKind](todo, obiOther)

proc convertList*(node: OrgNode, parent: SemOrg): SemOrg =
  assertKind(node, {orgList})
  result = newSem(node, parent)
  for item in items(node):
    item.unpackNode([
      bullet, counter, checkbox, header, completion, body])

    # echo item.treeRepr()
    var outItem = SemListItem()
    outItem.bullet = bullet.strVal()

    if not(checkbox of orgEmpty):
      outItem.checkbox = some(
        case checkbox.strVal():
          of "[ ]": lcheckEmpty
          of "[x]", "[X]": lcheckComplete
          else: lcheckEmpty
      )


    outItem.header = toSemOrg(header, result)

    if not(body of orgEmpty):
      outItem.body = some toSemOrg(body, result)

    # echo bullet.treeRepr()

    # outItem.add toSemOrg(tag, outItem)
    # outItem.add toSemOrg(header, outItem)
    # outItem.add toSemOrg(body, outItem)

    result.add newSem(item, result).withIt do:
      it.listItem = outItem

  # echov result.treeRepr()


proc toSemOptions*(node: OrgNode, parent: SemOrg): SemOrg =
  result = newSem(node, parent)


proc convertAnnotatedParagraph*(node: OrgNode, parent: SemOrg): SemOrg =
  node.unpackNode([prefix, body])
  result = newSem(node, parent)
  case prefix.kind:
    of orgListTag:
      result.paragraph = OrgAnnotatedParagraph(kind: aopListItem)

    of orgFootnote:
      result.paragraph = OrgAnnotatedParagraph(kind: aopFootnote)

    of orgBigIdent:
      result.paragraph = OrgAnnotatedParagraph(kind: aopAdmonition)

    else:
      discard

  result.paragraph.body = toSemOrg(body, result)
  
proc convertParagraph*(node: OrgNode, parent: SemOrg): SemOrg =
    # if not(tag of orgEmpty):
    #   if len(tag) == 1 and tag[0] of orgBigIdent:
    #     outItem.tag = some SemListItemTag(
    #       kind: sitBigIdent,
    #       idText: tag[0].strVal(),
    #       idKind: toSemOrgTodo(tag[0].strVal())
    #     )

    #   else:
    #     outItem.tag = some SemListItemTag(
    #       kind: sitText,
    #       text: toSemOrg(tag, result)
    #     )
  
  if node.len() == 1:
    case node[0].kind:
      of orgLink:
        result = toSemOrg(node[0], parent)

      else:
        discard

  if result.isNil():
    result = newSem(node, parent)
    for sub in node:
      result.add toSemOrg(sub, result)
  
proc toSemOrg*(node: OrgNode, parent: SemOrg): SemOrg =
  case node.kind:
    of orgStmtList:
      result = convertStmtList(node, parent)

    of orgSubtree:
      result = convertSubtree(node, parent)

    of orgTokenKinds - { orgTimeStamp }:
      result = newSem(node, parent)

    of orgParagraph:
      result = convertParagraph(node, parent)

    of orgAnnotatedParagraph:
      result = convertAnnotatedParagraph(node, parent)

    of orgQuote,
       orgVerbatim,
       orgItalic,
       orgBold,
       orgCommandCaption,
       orgCommandTitle,
       orgBacktick,
       orgUnderline,
       orgCenterBlock,
       orgAdmonitionBlock,
       orgLatexClass,
       orgStrike,
       orgInlineMath, # IMPLEMENT structured parsing using tree-sitter
       orgPlaceholder,
       orgLatexHeader,
       orgLatexCompiler,
       orgMonospace:
      result = newSem(node, parent)
      for sub in node:
        result.add toSemOrg(sub, result)

    of orgLink:
      result = toSemLink(node, parent)

    of orgCommandOptions:
      result = toSemOptions(node, parent)

    of orgList:
      result = convertList(node, parent)

    of orgTimeRange, orgTimeStamp:
      result = convertTime(node, parent)

    of orgFootnote:
      # IMPLEMENT handle inline footnote, todo handle paragraph converted
      # with footnote.
      result = newSem(node, parent)
      result.footnoteTarget = OrgFootnote(
        inline: false, ident: node["name"].strVal())

    of orgHashTag:
      proc aux(n: OrgNode): OrgHashTag =
        result = OrgHashTag(name: node[0].strVal())
        if 1 < n.len():
          for sub in node[1]:
            result.sub.add aux(sub)

      result = newSem(orgHashTag, node, parent)
      result.hashTag = aux(node)

    else:
      raise newUnexpectedKindError(node, $node.treeRepr())


type
  SemDocument* = object
    idTable*: Table[string, SemOrg]
    nameTable*: Table[string, SemOrg]
    anchorTable*: Table[string, SemOrg]
    footnoteTable*: Table[string, SemOrg]

proc auxDocument(sem: SemOrg, doc: var SemDocument) =
  case sem.kind:
    of orgSubtree:
      if sem.subtree.getProperty("id").canGet(id):
        doc.idTable[id.id] = sem

      for item in sem:
        auxDocument(item, doc)

    of orgStmtList, orgList:
      for item in sem:
        auxDocument(item, doc)

    else:
      discard


proc toDocument*(sem: SemOrg): SemDocument =
  auxDocument(sem, result)


proc getId*(sem: SemOrg): Option[string] =
  if sem.subtree.getProperty("id").canGet(prop):
    return some prop.id

proc getTodoString*(sem: SemOrg): Option[string] =
  sem.subtree.todo

proc getTodo*(sem: SemOrg): Option[OrgBigIdentKind] =
  if sem.subtree.todo.canGet(todo):
    return some toSemOrgTodo(todo)
  
proc getLinked*(document: SemDocument, link: OrgLink): Option[SemOrg] =
  case link.kind:
    of olkId:
      if link.linkId in document.idTable:
        return some document.idTable[link.linkId]

    of olkInternal:
      if link.linkId in document.anchorTable:
        return some document.anchorTable[link.linkId]

    of olkFile:
      # NOTE technically file can link to other documents or subtrees that
      # were included from the main document or brought together in a
      # bigger document collection.
      discard

    of olkWeb:
      discard

    else:
      raise newUnexpectedKindError(link, $link)
