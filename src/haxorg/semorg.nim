import ./types
import hmisc/other/[hshell, hargparse]
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

  # OrgPropertyArg* = object
  #   key*: PosStr
  #   value*: PosStr

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
        codeLink*: OrgUserLink

      of olkId:
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
    properties*: Table[tuple[name, subname: string], seq[OrgProperty]]
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

      of orgSrcCode:
        codeBlock*: CodeBlock

      of orgAssocStmtList:
        rs*: seq[AssocEntry]

      of orgLink:
        link*: OrgLink

      of orgFootnote:
        footnoteTarget*: string

      of orgTimeStamp:
        time*: DateTime

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




    
iterator items*(node: SemOrg, allowed: set[OrgNodeKind] = {}): SemOrg =
  for item in node.subnodes:
    if allowed.empty() or item of allowed:
      yield item

func itemsDFS*(node: SemOrg, allowed: set[OrgNodeKind] = {}): seq[SemOrg] =
  proc aux(node: SemOrg, res: var seq[SemOrg]) =
    if allowed.empty() or node of allowed:
      res.add node
    
    for item in items(node):
      aux(item, res)

  aux(node, result)

      
func len*(node: SemOrg): int = node.subnodes.len()
      
func line*(org: SemOrg): int = tern(org.node.isNil(), -1, org.node.line)
func column*(org: SemOrg): int = tern(org.node.isNil(), -1, org.node.column)
func strVal*(org: SemOrg): string =
  tern(org.node.isNil(), "", org.node.strVal())

type
  SemOrgReprFlag = enum
    sorfSkipParagraph
  
  SemOrgReprConf = object
    flags: set[SemOrgReprFlag]

const defaultSemOrgReprConf = SemOrgReprConf(
  flags: {
    sorfSkipParagraph
  }
)
  
    
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
        add " "
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
      
      of orgSubtree:
        let tree = n.subtree
        addField("subtree.level", hshow(tree.level))
        auxField(title, tree)
        auxField(body, tree)
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
          for kind, props in tree.properties:
            for prop in props:
              case prop.kind:
                of opkId:
                  addField("id", hshow(prop.id))
                
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
      for sub in items(n):
        add "\n"
        aux(sub, level + 1)


  aux(org, 0, none(string))
  endResult()

      
    
    
func newSem*(kind: OrgNodeKind, node: OrgNode, parent: SemOrg): SemOrg =
  SemOrg(kind: kind, node: node, isGenerated: false, parent: parent)

func newSem*(kind: OrgNodeKind, parent: SemOrg): SemOrg =
  SemOrg(kind: kind, isGenerated: true, parent: parent)
  
func newSem*(node: OrgNode, parent: SemOrg): SemOrg =
  SemOrg(kind: node.kind, node: node, isGenerated: false, parent: parent)

func `add`(node: var SemOrg, other: SemOrg) =
  node.subnodes.add other

func `[]`*(node: SemOrg, idx: int | BackwardsIndex): SemOrg =
  node.subnodes[idx]
  
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


proc convertProperty*(prop: OrgNode, parent: SemOrg): OrgProperty =
  case prop.kind:
    of orgCommandCaption:
      result = OrgProperty(kind: opkCaption)
      result.caption = toSemOrg(prop[0], parent)
      
    else:
      raise newUnexpectedKindError(prop)
        
proc toSemOrg*(group: OrgStmtGroup, parent: SemOrg): SemOrg =
  let last = group.elements.last()
  case last.kind:
    of orgAllKinds - orgAssociatedKinds:
      assert group.elements.len() == 1
      result = toSemOrg(last, parent)

    of orgQuoteBlock:
      result = newSem(last, parent)
      for prop in group.elements[0..^2]:
        result.properties.add convertProperty(prop, parent)

    else:
      raise newUnexpectedKindError(last)

macro unpackNode(node: OrgNode, subnodes: untyped{nkBracket}): untyped =
  result = newStmtList()
  for idx, name in subnodes:
    result.add newVarStmt(
      name, nnkBracketExpr.newtree(node, newLit(name.strVal())))

proc toSemProperty*(prop: OrgNode):
  tuple[name, subname: string, value: OrgProperty] =
  prop.unpackNode([name, subname, values])
  result.name = name.strVal().strip(chars = {':'})
  result.subname = subname.strVal().strip(chars = {':'})
  case result.name.normalize():
    of "id":
      result.value = OrgProperty(kind: opkId, id: values.strVal())
    
    else:
      raise newUnexpectedKindError(result.name, treeRepr(prop))

proc convertTime*(node: OrgNode, parent: SemOrg): SemOrg =
  result = newSem(node, parent)
  if node.kind == orgTimeRange:
    result.add convertTime(node[0], result)
    result.add convertTime(node[1], result)

  else:
    var parseOk = false
    let str = node.strVal()
    for pattern in @[
      "'['yyyy-MM-dd ddd HH:mm:ss']'",
      "'['yyyy-MM-dd HH:mm:ss']'",
      "'['yyyy-MM-dd ddd']'",
      "'['yyyy-MM-dd']'"
    ]:
      try:
        result.time = parse(str, pattern)
        parseOk = true
        break

      except TimeParseError:
        discard

    if not parseOk:
      echov str

  
proc toSemSubtree*(node: OrgNode, parent: SemOrg): SemOrg =
  result = newSem(node, parent)

  node.unpackNode(
    [prefix, todo, urgency, title, completion, tags, times, drawer, body])

  var tree = Subtree()

  if not(drawer["properties"] of orgEmpty):
    for prop in drawer["properties"]:
      let (nameStr, subnameStr, values) = toSemProperty(prop)
      tree.properties.mgetOrPut((nameStr, subnameStr), @[]).add values

  tree.level = prefix.strVal().count('*')
  tree.title = toSemOrg(title, result)
  tree.body = toSemOrg(body, result)
  result.subtree = tree

proc toSemLink*(node: OrgNode, parent: SemOrg): SemOrg =
  result = newSem(node, parent)
  
  case node["link"].kind:
    of orgRawText:
      let text = node["link"].strVal()
      let protocol = tern(
        node["protocol"] of orgEmpty, "", node["protocol"].strVal())

      if text.startsWith("http") or text.startsWith("https"):
        result.link = OrgLink(kind: olkWeb, webUrl: parseUri(text))

      elif protocol in ["id", "ID"]:
        result.link = OrgLink(kind: olkId, linkId: text)
        
      else:
        raise newUnexpectedKindError(text)

    else:
      raise newUnexpectedKindError(node["link"])

  result.link.description = toSemOrg(node["desc"], result)
  assertRef(result.link.description, $node["desc"].kind)
     

proc convertStmtList*(node: OrgNode, parent: SemOrg): SemOrg = 
  result = newSem(node, parent)
  for group in foldGroups(node.subnodes):
    result.add toSemOrg(group, result)

proc convertList*(node: OrgNode, parent: SemOrg): SemOrg =
  assertKind(node, {orgList})
  result = newSem(node, parent)
  for item in items(node):
    item.unpackNode([
      bullet, counter, checkbox, tag, header, completion, body])

    var outItem = newSem(item, result)
    outItem.add toSemOrg(tag, outItem)
    outItem.add toSemOrg(header, outItem)
    outItem.add toSemOrg(body, outItem)

    result.add outItem
    

proc toSemOrg*(node: OrgNode, parent: SemOrg): SemOrg =
  case node.kind:
    of orgStmtList:
      result = convertStmtList(node, parent)

    of orgSubtree:
      result = toSemSubtree(node, parent)

    of orgTokenKinds - { orgTimeStamp }:
      result = newSem(node, parent)

    of orgParagraph,
       orgQuote,
       orgVerbatim,
       orgItalic,
       orgBold,
       orgStrike,
       orgInlineMath, # IMPLEMENT structured parsing using tree-sitter
       orgPlaceholder,
       orgMonospace:
      result = newSem(node, parent)
      for sub in node:
        result.add toSemOrg(sub, result)

    of orgLink:
      result = toSemLink(node, parent)

    of orgList:
      result = convertList(node, parent)

    of orgTimeRange, orgTimeStamp:
      result = convertTime(node, parent)

    of orgFootnote:
      # IMPLEMENT handle inline footnote, todo handle paragraph converted
      # with footnote.
      result = newSem(node, parent)
      result.footnoteTarget = node["name"].strVal()

    else:
      raise newUnexpectedKindError(node, $node.treeRepr())
