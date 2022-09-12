import
  haxorg/[
    enum_types
  ]

const
  orgEmptyNode* = orgEmpty

  otcSubtreeKinds* = { otcSubtree0 .. otcSubtreeOther }
  otcMarkupKinds* = {
    otcBold .. otcMonospaceBlock
  }

  orgMarkupKinds* = {
    orgBold,
    orgItalic,
    orgVerbatim,
    orgBacktick,
    orgUnderline,
    orgStrike,
    orgQuote,
    orgAngle,
    orgMonospace
  }

  orgLineCommandKinds* = {
    orgCommandTitle .. orgCommandCaption,
    orgAttrImg
  }

  orgBlockCommandKinds* = { orgTable, orgSrcCode }
  orgAssociatedKinds* = { orgLink } + orgBlockCommandKinds + {
    orgCommandInclude
  } ## Line or block commands that can have associated property elements

  orgNoAssociatedKinds* = {
    orgCommandHeader, orgCommandName, orgCommandCaption
  } ## Line commands that cannot be used in standalone manner, and always
    ## have to be associated with some other block/line command

  orgDoclevelKinds* = {
    orgCommandOptions,
    orgCommandTitle,
    orgCommandAuthor,
    orgCommandBackendOptions
  } ## Nodes that should only be processed when encountered on the toplevel
    ## (initial document configuration)


  orgTokenKinds* = {
    orgCmdKey,
    orgCmdValue,
    orgCodeCallout,
    orgCmdFlag,
    orgCodeText,
    orgSubtreeStars,
    orgFilePath,
    orgLinkTarget,

    orgIdent,
    orgBullet,
    orgBareIdent,
    orgRawText,
    orgUnparsed,
    orgBigIdent,
    orgUrgencyStatus,
    orgVerbatimMultilineBlock,
    orgWord,
    orgNewline,
    orgMath,
    orgComment,
    orgCheckbox,
    orgCounter,
    orgCompletion,
    orgSymbol,
    orgTimeStamp,
    orgEmptyNode
  }

  orgSubnodeKinds* = {
    low(OrgNodeKind) .. high(OrgNodeKind)
  } - orgTokenKinds - {
    orgUserNode
  }

  orgAllKinds* = { low(OrgNodeKind) .. high(OrgNodeKind) }

#=============================  Error types  =============================#

type
  OrgSubKindError* = ref object of CatchableError
    subkind*: OrgNodeSubKind

  OrgUserNode* = ref object of RootObj
    ## User-defined org-mode node.
    ##
    ## - HINT :: This node is intended as an escape hatch for parser users
    ##   to add their own information into the tree. Parser and semcheck
    ##   won't generate nodes of this kind - this is handled only by final
    ##   user. Corresponding node kind is
    ##   [[code:OrgNodeKind.orgUserNode]]
    testField: char

type
  OskMarkupKindsRange* = range[oskBold .. oskAngle]

#==========================  Org-node objects  ===========================#

type
  OrgNodeObj* = object
    subkind*: OrgNodeSubKind
    line*: int
    column*: int
    case kind*: OrgNodeKind
      of orgTokenKinds:
        token*: OrgToken

      of orgUserNode:
        userNode*: OrgUserNode

      else:
        ranges*: PosStr
        str*: string
        subnodes*: seq[OrgNode]


  OrgNode* = ref OrgNodeObj

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
#================================  Links  ================================#
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


  OrgLink* = object
    ## Link to some external or internal entry.
    anchor*: Option[OrgAnchor] ## Resolved link target
    targetName*: string
    case kind*: OrgLinkKind
      of olkWeb:
        webUrl*: Url

      of olkDoi:
        doi*: string

      of olkFile, olkAttachment, olkDocview:
        linkFile*: OrgFile
        search*: Option[OrgInFileSearch]

      of olkCode:
        codeLink*: OrgUserLink

      of olkOtherLink:
        linkFormat*: string
        linkBody*: string

      else:
        discard

#==============================  Metatags  ===============================#
  SmtAccsKind* = enum
    oakRead
    oakWrite
    oakDelete
    oakCreate

  OrgMetaTag* = ref object
    case kind*: OrgMetaTagKind
      of smtAccs:
        accsKind*: set[SmtAccsKind]
        accsTarget*: OrgMetaTag ## Access target. `@global{}`, `@file{}`,
                                ## `@dir{}`, `@env{}`


      of smtSh:
        shHasRoot*: bool
        shCmd*: ShellCmd

      of smtImport:
        importLink*: OrgLink

      else:
        discard


#==============================  Subtrees  ===============================#
  Subtree* = ref object
    level*: int
    properties*: Table[tuple[name, subname: string], OrgNode]
    completion*: Option[OrgCompletion]
    tags*: seq[string]

#================================  Lists  ================================#
  ListItemTagKind* = enum
    ## Tag kinds for description list
    sitText ## Regular text
    sitMeta ## Only metatags
    sitBigIdent ## Only big idents

  ListItemTag* = object
    case kind*: ListItemTagKind
      of sitMeta:
        meta*: seq[OrgMetaTag]

      of sitBigIdent:
        idText*: string
        idKind*: seq[OrgBigIdentKind]

      of sitText:
        text*: SemOrg

  AssocEntry* = object
    name*: string
    body*: SemOrg


  ListItem = ref object
    bullet*: string
    counter*: Option[SemOrg]
    checkbox*: Option[SemOrg]
    tag*: Option[ListItemTag]
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

  OrgDocument* = object
    title*: SemOrg
    author*: SemOrg
    url*: Option[Url]
    exportname*: Option[string]

    flags*: set[OrgDocumentFlag]

    tocMax*: Option[int] ## Max level of heading to include in the toc
    pagenum*: Option[int] ## Page number to start with

    backendPage*: tuple[
      header, footer: Table[string, string]] ## Backend-specific page
    ## header and footer configurations.


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

    subnodes*: seq[SemOrg]
    properties*: seq[OrgProperty] ## Property from associative list

    subkind*: OrgNodeSubKind
    case kind*: OrgNodeKind
      of orgTable:
        table*: OrgTable

      of orgSubtree:
        subtree*: Subtree

      of orgSrcCode:
        codeBlock*: CodeBlock

      of orgAssocStmtList:
        rs*: seq[AssocEntry]

      of orgLink:
        link*: OrgLink

      of orgProperty, orgAttrImg:
        property*: OrgProperty ## Standalone property

      of orgBigIdent:
        bigIdentKind*: OrgBigIdentKind

      of orgCommandInclude:
        includeSpec*: OrgInclude

      of orgDocument:
        ## Document-level properties collected during conversion from parse
        ## tree.
        document*: OrgDocument

      of orgListItem:
        listItem*: ListItem

      of orgMetaTag:
        metaTag*: OrgMetaTag

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
