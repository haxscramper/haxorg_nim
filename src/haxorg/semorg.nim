import ./types
import hmisc/core/all
import hmisc/other/oswrap
import std/options
import std/tables
import std/times
import std/macros
import std/uri
import
  hmisc/algo/[
    hlex_base,
    hparse_base,
    clformat
  ]

type
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
        shCmd*: seq[string]

      of smtImport:
        importLink*: OrgLink

      else:
        discard


#==============================  Subtrees  ===============================#
  Subtree* = ref object
    level*: int
    properties*: Table[tuple[name, subname: string], SemOrg]
    completion*: Option[OrgCompletion]
    tags*: seq[string]
    title*: SemOrg
    body*: SemOrg

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

    # subkind*: OrgNodeSubKind
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

type
  OrgStmtGroup = object
    elements: seq[OrgNode]
    closed: bool

func newSem*(kind: OrgNodeKind, node: OrgNode): SemOrg =
  result = SemOrg(kind: kind, node: node, isGenerated: false)

func newSem*(node: OrgNode): SemOrg =
  result = SemOrg(kind: node.kind, node: node, isGenerated: false)

func `add`(node: var SemOrg, other: SemOrg) =
  node.subnodes.add other

proc toSemOrg*(node: OrgNode): SemOrg

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
        result.last().add node

      of orgAssociatedKinds:
        # If associated node is found and last group is attachable, push
        # the element to it and closed the group.
        if result.last().attachable():
          result.last().add node
          result.last().closed = true

        elif node of orgResult and
             result.last().elements.last() of orgSrcCode:
          result.last().add node

        else:
          result.add result.pop().expand()
          result.last().add node

      else:
        if result.lastGrouped():
          result.add result.pop().expand()

        result.add group(node)


proc toSemOrg*(group: OrgStmtGroup): SemOrg =
  case group.elements.last().kind:
    of orgAllKinds - orgAssociatedKinds:
      assert group.elements.len() == 1
      result = toSemOrg(group.elements.last())

    else:
      raise newUnexpectedKindError(group.elements[0])

macro unpackNode(node: OrgNode, subnodes: untyped{nkBracket}): untyped =
  result = newStmtList()
  for idx, name in subnodes:
    result.add newVarStmt(
      name, nnkBracketExpr.newtree(node, newLit(name.strVal())))

proc toSemProperty*(prop: OrgNode):
  tuple[name, subname: string, value: SemOrg] =
  prop.unpackNode([name, subname, values])
  result.name = name.strVal().strip(chars = {':'})
  result.subname = subname.strVal().strip(chars = {':'})


proc toSemSubtree*(node: OrgNode): SemOrg =
  result = newSem(node)

  node.unpackNode(
    [prefix, todo, urgency, title, completion, tags, times, drawer, body])

  var tree = Subtree()

  if not(drawer["properties"] of orgEmpty):
    for prop in drawer["properties"]:
      let (nameStr, subnameStr, values) = toSemProperty(prop)
      tree.properties[(nameStr, subnameStr)] = values

  tree.level = prefix.strVal().count('*')
  tree.title = toSemOrg(title)
  tree.body = toSemOrg(body)
  result.subtree = tree

proc toSemLink*(node: OrgNode): SemOrg =
  discard

proc toSemOrg*(node: OrgNode): SemOrg =
  case node.kind:
    of orgStmtList:
      result = newSem(node)
      for group in foldGroups(node.subnodes):
        result.add toSemOrg(group)

    of orgSubtree:
      result = toSemSubtree(node)

    of orgTokenKinds:
      result = newSem(node)

    of orgParagraph:
      result = newSem(node)
      for sub in node:
        result.add toSemOrg(sub)

    of orgLink:
      result = toSemLink(node)

    else:
      raise newUnexpectedKindError(node)
