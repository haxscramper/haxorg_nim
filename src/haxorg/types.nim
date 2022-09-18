import
  haxorg/[
    enum_types,
    misc_types
  ],
  hmisc/algo/[
    hlex_base,
    hparse_base,
    clformat
  ],
  hmisc/core/[
    all
  ],
  hmisc/other/[
    hshell,
    hargparse,
    oswrap
  ],
  hmisc/macros/[
    ast_spec
  ],
  std/[
    options,
    tables,
    uri,
    sequtils,
    strformat
  ]

export enum_types, misc_types

type
  ParseConf* = object
    dropEmptyWords*: bool

const defaultParseConf*: ParseConf = ParseConf(
  dropEmptyWords: true
)

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
  OrgToken* = HsTok[OrgTokenKind]
  OrgLexer* = HsLexer[OrgToken]


type
  OrgNodeObj* = object
    subkind*: OrgNodeSubKind
    line*: int
    column*: int
    subnodes*: seq[OrgNode]
    case kind*: OrgNodeKind
      of orgTokenKinds:
        token*: OrgToken

      of orgUserNode:
        userNode*: OrgUserNode

      of orgError:
        # TODO implement support for error reporting directly in the AST
        # using error nodes.
        report*: int

      else:
        ranges*: PosStr
        str*: string


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


    # opkToc ## Table of contents configuration
    opkAttrBackend ## Export attributes for particular backend

    opkColumnSpec ## Properties passed down from the column formatting
                  ## specification.

    # opkInclude ## `#+include` directive
    opkName ## `#+name`
    opkLinkAbbrev ## Link abbreviation definition
    ##
    ## https://orgmode.org/manual/Link-Abbreviations.html#Link-Abbreviations
    opkFiletags ## File-level tags
    ##
    ## https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance
    opkTagConf # TODO https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance
    opkLatexHeader
    opkOtherProperty

  # OrgPropertyArg* = object
  #   key*: PosStr
  #   value*: PosStr

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

      else:
        discard



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

      of orgMetaSymbol:
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

func strVal*(node: OrgNode): string =
  case node.kind:
    of orgTokenKinds:
      return node.token.strVal()

    else:
      return node.str

#=========================  Node specification  ==========================#
const
  orgNodeSpec* = astSpec(OrgNode, OrgNodeKind):
    orgList:
      0 .. ^1 as "items":
        orgListItem

    orgSubtree:
      0 as "prefix"
      1 as "todo": orgBigIdent or orgEmpty
      2 as "urgency": orgUrgencyStatus or orgEmpty
      3 as "title": orgParagraph
      4 as "completion": orgCompletion or orgEmpty
      5 as "tags": orgOrgTag or orgEmpty
      6 as "times": orgStmtList or orgEmpty
      7 as "drawer": orgDrawer
      8 as "body": orgStmtList or orgEmpty

    orgDrawer:
      0 as "properties":
        orgPropertyList or orgEmpty

      1 as "logbook":
        orgLogbook or orgEmpty

    orgLogbook:
      0 .. ^1 as "logs":
        orgLogbookStateChange or orgLogbookNote

    orgLogbookStateChange:
      0 as "newstate": orgBigIdent or orgEmpty
      1 as "oldstate": orgBigIdent or orgEmpty
      2 as "time": orgTimeStamp
      3 as "note": orgStmtList

    orgLogbookNote:
      0 as "time": orgTimeStamp
      1 as "text": orgStmtList

    orgTimeAssoc:
      0 as "name": orgBigIdent or orgEmpty
      1 as "time": orgTimeStamp or orgTimeRange

    orgPropertyList:
      0 .. ^1:
        orgProperty

    orgProperty:
      0 as "name": orgRawText
      1 as "subname"
      2 as "values"

    orgMultilineCommand:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty
      2 as "body"

    orgMetaSymbol:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty
      2 as "body": orgRawText

    orgTable:
      0 as "args": orgCmdArguments or orgEmpty
      1 .. ^1 as "rows": orgTableRow

    orgTableRow:
      0 as "args":
        ## Optional arguments for row - can be specified using `#+row`. For
        ## pipe formatting this is not supported, so arguments would be an
        ## empty node.
        orgCmdArguments or orgEmpty

      1 as "text":
        ## It is possible to put text on the *row* level.
        orgParagraph or orgEmpty

      2 as "body":
        0 .. ^1:
          orgTableCell

    orgTableCell:
      0 as "args": orgCmdArguments or orgEmpty
      1 as "text": orgEmpty or orgStmtList

    orgCommand:
      0 as "name": orgIdent
      1 as "args": orgCmdArguments or orgEmpty

    orgCommandCaption:
      0 as "text": orgParagraph

    orgCommandOptions:
      0 as "args": orgCmdArguments


    orgCommandInclude:
      0 as "file": orgFilePath
      1 as "kind": orgEmpty or orgIdent
      2 as "lang": orgEmpty or orgIdent
      3 as "args": orgEmpty or orgCmdArguments

    orgCommandHeader:
      0 as "args": orgEmpty or orgCmdArguments

    orgCodeLine:
      0 .. ^1:
        orgCodeText or orgCodeTangle or orgCodeCallout or orgEmpty

    orgSrcCode:
      0 as "lang": orgIdent
      1 as "header-args": orgCmdArguments
      2 as "body":
        orgStmtList:
          0 .. ^1:
            orgCodeLine

      3 as "result": orgRawText or orgEmpty

    orgCallCode:
      0 as "name": orgIdent
      1 as "header-args": orgCmdArguments
      2 as "args"
      3 as "end-args"
      4 as "result": orgRawText or orgEmpty

    orgCmdArguments:
      0 as "flags":
        orgInlineStmtList:
          0 .. ^1:
            orgCmdFlag

      1 as "args":
        orgInlineStmtList:
          0 ..^ 1:
            orgCmdValue

    orgCmdValue:
      0 as "name"
      1 as "value"

    orgAssocStmtList:
      0 as "assoc"
      1 as "main"

    orgResult:
      0 as "hash"
      1 as "body"

    orgListItem:
      0 as "bullet":
        ## List prefix - either dash/plus/star (for unordered lists), or
        ## `<idx>.`/`<name>.`

      1 as "counter"

      2 as "checkbox":
        ## Optional checkbox
        orgCheckbox or orgEmptyNode

      3 as "tag":
        ## Described text (for description list)
        orgParagraph or orgEmptyNode

      4 as "header":
        ## Main part of the list
        orgParagraph

      5 as "completion":
        ## Cumulative completion progress for all subnodes
        orgCompletion or orgEmptyNode

      6 as "body":
        ## Additional list items - more sublists, or extended body (with
        ## code blocks, extra parargaphs etc.)
        orgStmtList or orgEmptyNode


    orgFootnote:
      0 as "name"
      1 as "definition"

    orgLink:
      0 as "link"
      1 as "desc"

generateFieldEnum(orgNodeSpec, "orgf")


func add*(node: var OrgNode, other: OrgNode | seq[OrgNode]) =
  assert node.kind notin orgTokenKinds, $node.kind
  if node.subnodes.len == 0:
    when other is seq:
      if other.len > 0:
        node.line = other[0].line
        node.column = other[0].column

    else:
      node.line = other.line
      node.column = other.column

  node.subnodes.add other

func len*(node: OrgNode): int =
  if node.kind in orgSubnodeKinds: node.subnodes.len else: 0

func getStr*(node: OrgNode): string =
  if node.kind in orgTokenKinds:
    assert false

  else:
    assert false

func `[]`*(node: var OrgNode, idx: BackwardsIndex): var OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, slice: HSlice[int, BackwardsIndex]): seq[OrgNode] =
  node.subnodes[slice]

func `[]`*(node: OrgNode, idx: BackwardsIndex): OrgNode =
  node.subnodes[idx]

func `[]=`*(node: var OrgNode, idx: BackwardsIndex, val: OrgNode) =
  node.subnodes[idx] = val

func `[]`*(node: var OrgNode, idx: int): var OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, idx: int): OrgNode =
  node.subnodes[idx]

func `[]`*(node: OrgNode, name: string): OrgNode =
  node[getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))]

func `[]`*(node: var OrgNode, name: string): var OrgNode =
  node[getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))]

func `[]`*(node: OrgNode, field: OrgNodeField): OrgNode =
  node[field.toName()]

func `[]`*(node: var OrgNode, field: OrgNodeField): var OrgNode =
  node[field.toName()]

func `[]`*(node: OrgNode, name: string, kind: OrgNodeKind): OrgNode =
  let idx = getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))
  result = node[idx]
  if result.kind != kind:
    raise newUnexpectedKindError(
      result,
      "Node of kind ", mkind(node.kind),
      " was expected to have subnode of kind ",
      mkind(kind), " at position ", idx,
      "(positional name - '{name}')"
    )

func `[]`*(node: OrgNode, field: OrgNodeField, kind: OrgNodeKind): OrgNode =
  node[field.toName(), kind]

func `[]=`*(node: var OrgNode, idx: int, val: OrgNode) =
  node.subnodes[idx] = val

func `[]=`*(node: var OrgNode, name: string, val: OrgNode) =
  let idx = getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))
  while node.len - 1 < idx:
    node.add OrgNode(kind: orgEmptyNode)

  node[idx] = val

func `[]`*(node: var OrgNode, name: var string): var OrgNode =
  node[getSingleSubnodeIdx(orgNodeSpec, node.kind, name, some(node.len))]

template subKindErr*(subKindVal: OrgNodeSubKind): untyped {.dirty.} =
  raise OrgSubKindError(
    subkind: subKindVal,
    msg: "Unexpected node subkind - " & $subKindVal)

iterator items*(node: OrgNode): OrgNode =
  if not(node of orgTokenKinds):
    for n in node.subnodes:
      yield n

iterator mitems*(node: var OrgNode): var OrgNode =
  if not(node of orgTokenKinds):
    for n in mitems(node.subnodes):
      yield n

iterator pairs*(node: OrgNode): (int, OrgNode) =
  if not(node of orgTokenKinds):
    for idx, n in node.subnodes:
      yield (idx, n)

iterator mpairs*(node: OrgNode): (int, var OrgNode) =
  if not(node of orgTokenKinds):
    for idx, n in mpairs(node.subnodes):
      yield (idx, n)

proc `$`*(org: OrgNodeKind): string {.inline.} = toString(org)[3 ..^ 1]
proc `$`*(org: OrgNodeSubKind): string {.inline.} = toString(org)[3 ..^ 1]


proc newTree*(kind: OrgNodeKind, token: OrgToken): OrgNode =
  result = OrgNode(kind: kind)
  result.token = token

proc newTree*(
    str: PosStr, kind: OrgNodeKind, subnodes: varargs[OrgNode]): OrgNode =
  result = OrgNode(kind: kind)

  result.line = str.line
  result.column = str.column
  for node in subnodes:
    result.subnodes.add node

proc newTree*(kind: OrgNodeKind, subnodes: varargs[OrgNode]): OrgNode =
  result = OrgNode(kind: kind)
  if 0 < subnodes.len:
    result.line = subnodes[0].line
    result.column = subnodes[0].column

  for node in subnodes:
    result.subnodes.add node

proc newTree*(
     kind: OrgNodeKind,
     subkind: OrgNodeSubKind, subnodes: varargs[OrgNode]
  ): OrgNode =

  result = newTree(kind, subnodes)
  result.subkind = subkind

proc newEmptyNode*(subkind: OrgNodeSubKind = oskNone): OrgNode =
  result = OrgNode(kind: orgEmptyNode)
  result.subKind = subKind


proc newError*(subnode: varargs[OrgNode]): OrgNode =
  ## Create new error error node with given subnodes
  newTree(orgError, @subnode)

proc newCmdArguments*(): OrgNode =
  newTree(
    orgCmdArguments,
    newTree(orgInlineStmtList),
    newTree(orgInlineStmtList))

proc newOrgEmptyNode*(): OrgNode = OrgNode(kind: orgEmptyNode)
proc newOrgEmpty*(): OrgNode = OrgNode(kind: orgEmptyNode)

proc isEmptyNode*(tree: OrgNode): bool =
  tree.kind == orgEmptyNode


proc newOStmtList*(subnodes: varargs[OrgNode]): OrgNode =
  orgStmtList.newTree(subnodes)

const treeReprDisplay = hdisplay(flags += dfWithRanges)

func `$`*(node: OrgNode): string =
  result = &"({node.kind} "
  case node.kind:
    of orgTokenKinds:
      result &= &"\"{node.strVal()}\")"

    else:
      var first = true
      result &= "["
      for sub in node:
        if not first: result &= ", "
        result &= $sub
      result &= "])"

      

proc treeRepr*(
    org: OrgNode,
    opts: HDisplayOpts = treeReprDisplay
  ): ColoredText =

  coloredResult()

  proc aux(
      n: OrgNode, level: int,
      name: Option[string]
    ) =

    addIndent(level)
    if isNil(n):
      add hshow(nil, opts)
      return

    add hshow(n.kind)

    if n.subKind != oskNone:
      add " ("
      add hshow(n.subKind)
      add ")"

    if opts.withRanges:
      add " "
      add hshow(n.line, opts)
      add ":"
      add hshow(n.column, opts)


    if name.isSome():
      add " "
      add toCyan(name.get())

    case n.kind:
      of orgTokenKinds - orgEmpty:
        add " "
        add hshow(
          n.strVal(),
          hdisplay(flags -= dfSpellEmptyStrings))

      of orgEmpty:
        discard

      else:
        var printed = false
        if n of orgParagraph and false:
          proc isMarkupWords(org: OrgNode): bool =
            org.kind in {orgWord} or
            (
              org.kind in {orgBold, orgItalic, orgWord, orgParagraph} and
               allIt(org, isMarkupWords(it))
            )

          proc formatMarkup(n: OrgNode): ColoredText =
            case n.kind:
              of orgWord:
                result.add "W["
                result.add hshow(n.strVal(), hdisplay(flags -= dfUseQuotes))
                result.add "]"

              of orgBold, orgParagraph:
                let pref =
                  case n.kind:
                    of orgWord: "B"
                    of orgParagraph: "P"
                    else: ""

                result.add pref & "["
                for sub in items(n):
                  result.add formatMarkup(sub)
                result.add "]"

              else: raise newUnexpectedKindError(n)


          var reslen = 0
          add "\n"
          var prevWord = false
          for idx, word in pairs(n):
            if isMarkupWords(word):
              if idx == 0:
                addIndent(level + 1)

              let formatted = formatMarkup(word)
              add formatted
              reslen += len(formatted)
              if 60 < reslen:
                reslen = 0
                add "\n"
                addIndent(level + 1)

              prevWord = true

            else:
              if not prevWord and idx > 0:
                add "\n"

              aux(word, level + 1, none(string))

              prevWord = false

        else:
          if not printed:
            for idx, sub in n:
              add "\n"
              let subErr = validateSub(orgNodeSpec, n, idx)
              if subErr.isSome():
                 add subErr.get().indent(level * 2 + 2)
                 add "\n"

              aux(sub, level + 1, orgNodeSpec.fieldName(n, idx))

        let err = validateSelf(orgNodeSpec, n)
        if err.isSome():
          add "\n"
          add err.get().indent(level * 2 + 2)


  aux(org, 0, none(string))
  endResult()
