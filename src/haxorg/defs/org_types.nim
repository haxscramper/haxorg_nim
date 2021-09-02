import std/[options, tables, hashes, uri]

import nimtraits

import
  hmisc/algo/hlex_base,
  hmisc/other/[hshell, oswrap, hpprint, hargparse],
  hmisc/core/all

type
  OrgCommandTokenKind* = enum
    octNone

    octColonKeyword
    octDash
    octStrLit
    octIdent
    octRaw

    octEof


  OrgNodeSubKind* = enum
    ## Additional node classification that does not warrant own AST
    ## structure, but could be very useful for further processing.
    ##
    ## This listtries to cover *all* possible combinations of uses for each
    ## identifier.
    oskNone


    oskBold ## Node is bold text
    oskItalic
    oskVerbatim
    oskMonospaced
    oskBacktick
    oskUnderline
    oskStrike
    oskQuote ## Line quote text `> sometext`
    oskAngle


    oskDescriptionTagText ## Description list tag text
    oskLinkContent ## Link description text
    oskTitleText ## Paragraph in title of the subtree
    oskCaptionText ## Paragraph in `#+caption:`
    oskListHeaderText
    oskListBodyText
    oskListTagText
    oskStandaloneText
    oskSrcInlineText
    oskCallInlineText

    oskMetatagText ## Raw content of the metatag
    oskMetatagArgs
    oskLinkAddress
    oskComment
    oskMetaTag

    oskHashTagIdent
    oskSymbolIdent
    oskBracTagIdent
    oskOrgTagIdent
    oskMetaTagIdent
    oskTodoIdent


    oskDashBullet
    oskPlusBullet
    oskStarBullet

    oskRomanBullet
    oskNumBullet
    oskLetterBullet

    oskOrderedList
    oskUnorderedList
    oskMixedList
    oskFullDescList
    oskPartialDescList



    oskText
    oskSpace
    oskParen
    oskBracket
    oskCurly
    oskPunct
    oskBigWord






  OrgNodeKind* = enum
    ## Different kinds of org-mode nodes produces by parser.
    ##
    ## Note that it does not directly map to document in a way that one
    ## might expect, mainly due to extensibility of the org-mode. For
    ## example there is no `orgExampleBlock` (for `#+begin-example`), but
    ## instead it is represented as `MultilineCommand[Ident["example"]]`.
    ## This is a little more verbose, but allows to use single
    ## `MultilineCommand` node for anything, including source code,
    ## examples and more. Though /some/ command blocks that are
    ## /especially/ important do have their own node kinds and syntax (such
    ## as source code blocks)
    ##
    ## Most mulitline commands have corresponding single-line versions, and
    ## sometimes an inline too. Notable example are passthrough blocks -
    ## you can write `#+html: <some-html-code>`, `#+begin-export html` and
    ## finally `@@html: <html-code>@@`. One and multi-line blocks usually
    ## have similar syntax, but inline ones are pretty different. #[ DOC why? ]#
    ##
    ## There is no difference between multi-line and inline commands blocks
    ## in AST. #[ REVIEW is this a good idea, maybe separating those two
    ## would make things more intuitive? ]#
    ##
    ## #[ All ? ]# Elements that have inline, single-line and multiline
    ## versions are
    ##
    ## - `orgPassCode` :: Passthrough block of code to particular backend
    ## - `orgCallCode` :: Evaluate named code block
    ## - `orgSrcCode` :: Named code block
    orgNone  ## Default valye for node - invalid state

    orgDocument ## Toplevel part of the ast, not created by parser, and
                ## only used in `semorg` stage

    orgUserNode ## User-defined node [[code:OrgUserNode]]

    orgEmpty ## Empty node - valid state that does not contain any
             ## value

    orgInlineStmtList
    orgStmtList ## List of statements, possibly recursive. Used as toplevel
    ## part of the document, in recursive parsing of subtrees, or as
    ## regular list, in cases where multiple subnodes have to be grouped
    ## together.

    orgAssocStmtList ## Associated list of statements - AST elements like
    ## commands and links are grouped together if placed on adjacent lines

    orgSubtree ## Section subtree
    orgSubtreeTimes ## Time? associated with subtree entry
    orgSubtreeStars


    orgCompletion ## Task compleation cookie, indicated either in percents
    ## of completion, or as `<done>/<todo>` ratio.

    orgCheckbox ## Single checkbox item like `[X]` or `[-]`

    orgList
    orgBullet ## List item prefix
    orgListItem
    orgCounter

    orgComment ## Inline or trailling comment. Can be used addition to
    ## `#+comment:` line or `#+begin-comment` section. Nested comment
    ## syntax is allowed (`#[ level1 #[ level2 ]# ]#`), but only outermost
    ## one is represented as separate AST node, everything else is a
    ## `.text`

    orgRawText ## Raw string of text from input buffer. Things like
    ## particular syntax details of every single command, link formats are
    ## not handled in parser, deferring formatting to future processing
    ## layers

    orgCommand ## Single-line command


    # Single-line commands start
    orgCommandTitle
    orgCommandInclude
    orgCommandName

    orgCommandCaption ## `#+caption` command
    # Single-line commands end


    orgFilePath

    orgExportCommand

    orgMultilineCommand ## Multiline command such as code block, latex
    ## equation, large block of passthrough code. Some built-in org-mode
    ## commands do not requires `#+begin` prefix, (such as `#+quote` or
    ## `#+example`) are represented by this type of block as well.

    orgResult ## Command evaluation result

    orgIdent ## regular identifier - `alnum + [-_]` characters for
    ## punctuation. Identifiers are compared and parsed in
    ## style-insensetive manner, meaning `CODE_BLOCK`, `code-block` and
    ## `codeblock` are identical.

    orgBareIdent ## Bare identifier - any characters are allowed

    orgBigIdent ## full-uppsercase identifier such as `MUST` or `TODO`

    orgVerbatimMultilineBlock ## Verbatim mulitiline block that *might* be
    ## a part of `orgMultilineCommand` (in case of `#+begin-src`), but not
    ## necessarily. Can also be a part of =quote= and =example= multiline
    ## blocks.

    # TODO implement as separate node kind, different from regular non-leaf
    # subnodes.
    orgNowebMultilineBlock ## Source code block that was parsed for noweb
    ## interpolation.

    orgSnippetMultilineBlock ## Source code block that was parsed to be
    ## used as snippet. It is quite close to `noweb`, but is added to
    ## support literate snippets.

    orgCodeLine ## Single line of source code
    orgCodeText ## Block of source code text
    orgCodeTangle
    orgCodeCallout

    orgSrcCode ## Block of source code - can be multiline, single-line and
    ## inline (such as `src_nim`). Latter is different from regular
    ## monospaced text inside of `~~` pair as it contains additional
    ## internal structure, optional parameter for code evaluation etc.

    orgCallCode ## Call to named source code block. Inline, multiline, or
    ## single-line.

    orgPassCode ## Passthrough block. Inline, multiline, or single-line.
    ## Syntax is `@@<backend-name>:<any-body>@@`. Has line and block syntax
    ## respectively

    orgCmdArguments ## Command arguments

    orgCmdFlag ## Flag for source code block. For example `-n`, which is
    ## used to to make source code block export with lines

    orgCmdKey
    orgCmdValue
    orgCmdFuncArg ## Key-value pair for source code block call.

    orgUrgencyStatus ## Subtree importance level, such as `[#A]` or `[#B]`.
    ## Default org-mode only allows single character for contents inside of
    ## `[]`, but this parser makes it possible to use any regular
    ## identifier, such as `[#urgent]`.

    orgParagraph ## Single 'paragraph' of text. Used as generic container
    ## for any place in AST where unordered sentence might be encountered -
    ## not limited to actual paragraph

    orgBold, orgItalic, orgVerbatim, orgBacktick,
    orgUnderline, orgStrike, orgQuote, orgAngle, orgMonospace ##
    ## @multidoc{} Region of text with formatting, which contains standalone
    ## words - can itself contain subnodes, which allows to represent
    ## nested formatting regions, such as `*bold /italic/*` text.
    ## Particular type of identifier is stored in string form in `str`
    ## field for `OrgNode` - bold is represented as `"*"`, italic as `/`
    ## and so on. In case of explicit open/close pairs only opening one is
    ## stored.
    ##
    ## NOTE: when structured sentences are enabled, regular punctuation
    ## elements like `some text (notes)` are also represented as `Word,
    ## Word, Markup(str: "(", [Word])` - e.g. structure is not fully flat.

    orgMath ## Inline latex math. Moved in separate node kinds due to
    ## *very* large differences in syntax. Contains latex math body
    ## verbatim.

    orgWord ## Regular word - technically not different from `orgIdent`,
    ## but defined separately to disiguish between places where special
    ## syntax is required and free-form text.

    orgLink ## External or internal link. Consists of one or two elements -
    ## target (url, file location etc.) and description (`orgParagraph` of
    ## text). Description might be empty, and represented as empty node in
    ## this case. For external links particular formatting of the address
    ## is not handled by parser and instead contains raw string from input
    ## text.

    orgLinkTarget


    orgMacro ## Org-mode macro replacement - during export each macro is
    ## expanded and evaluated according to it's environment. Body of the
    ## macro is not parsed fully during org-mode evaluation, but is checked
    ## for correct parenthesis balance (as macro might contain elisp code)

    orgBackendRaw ## Raw content to be passed to a particular backend. This
    ## is the most compact way of quoting export strings, after
    ## `#+<backend>: <single-backend-line>` and `#+begin-export <backend>`
    ## `<multiple-lines>`.

    orgSymbol ## Special symbol that should be exported differently to
    ## various backends - greek letters (`\alpha`), mathematical notations
    ## and so on.

    orgTimeStamp ## Single date and time entry (active or inactive),
    ## possibly with repeater interval. Is not parsed directly, and instead
    ## contains `orgRawText` that can be parsed later

    orgTimeRange ## Date and time range format - two `orgDateTime` entries

    orgDetails ## `#+begin_details`  section
    orgSummary ## `#+begin_summary` section

    orgTable ## Org-mode table. Tables can be writtein in different
    ## formats, but in the end they are all represented using single ast
    ## type. NOTE: it is not guaranteed that all subnodes for table are
    ## exactly `orgTableRow` - sometimes additional property metadata might
    ## be used, making AST like `Table[AssocStmtList[Command[_],
    ## TableRow[_]]]` possible

    orgTableRow ## Horizontal table row
    orgTableCell ## Single cell in row. Might contain anyting, including
    ## other tables, simple text paragraph etc.

    orgFootnote ## Footnote entry. Just as regular links - internal content
    ## is not parsed, and instead just cut out verbatim into target AST
    ## node.

    orgHorizontal ## Horizotal rule. Rule body might contain other
    ## subnodes, to represnt `---- some text ----` kind of formatting.

    orgOrgTag ## Original format of org-mode tags in form of `:tagname:`.
    ## Might contain one or mode identifgiers, but does not provide support
    ## for nesting - `:tag1:tag2:`. Can only be placed within restricted
    ## set of places such as subtree headings and has separate place in AST
    ## when allowed (`orgSubtree` always has subnode `№4` with either
    ## `orgEmpty` or `orgOrgTag`)

    orgHashTag ## More commonly used `#hashtag` format, with some
    ## additional extension. Can be placed anywere in the document
    ## (including section headers), but does not have separate place in AST
    ## (e.g. considered regular part of the text)

    orgMetaTag ## Javadoc/doxygen-like metatag. Extension to org mode
    ## syntax, making it more sutiable for writing documentation. Several
    ## differen ways of writing are supported, starting from regular -
    ## `@tag arg;`, to `@tag[arg1, arg2]{tag body}` Semicolon is mandatory
    ## for metatag without curly braces enclosing body, but otherwise.
    ## Correct metatag should have three subnodes - `Ident`, `RawStr` and
    ## any other subnode kind for body.

    orgBracTag ## Custom extension to org-mode. Similarly to `BigIdent`
    ## used to have something like informal keywords `MUST`, `OPTIONAL`,
    ## but instead aimed /specifically/ at commit message headers -
    ## `[FEATURE]`, `[FIX]` and so on.

    orgDrawer ## Single enclosed drawer like `:properties: ... :end:` or
    ## `:logbook: ... :end:`

    orgPropertyList
    orgProperty ## Property entry, either in `#+property:` command, or in
                ## `:property:` drawer

    orgPlaceholder ## Placeholder entry in text, usually writte like `<text
                   ## to replace>`

    orgLogbook
    orgLogbookStateChange
    orgLogbookNote
    orgLogbookClock # https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html TODO doc and AST schema.

    orgRadioTarget
    orgTarget

  # TODO allow for macro replacement to be used as identifiers in cases
  # like `@@{{{backend}}}:<b>@@`

const orgEmptyNode* = orgEmpty

const
  orgMarkupKinds* = {
    orgBold, orgItalic, orgVerbatim, orgBacktick,
    orgUnderline, orgStrike, orgQuote, orgAngle
  }

  orgLineCommandKinds* = { orgCommandTitle .. orgCommandCaption }

  orgBlockCommandKinds* = { orgTable, orgSrcCode }
  orgAssociatedKinds* = { orgLink } + orgBlockCommandKinds


  orgTokenKinds* = {
    orgCmdKey,
    orgCmdValue,
    orgCmdFlag,
    orgCodeText,
    orgSubtreeStars,
    orgFilePath,
    orgLinkTarget,

    orgIdent,
    orgBullet,
    orgBareIdent,
    orgRawText,
    orgBigIdent,
    orgUrgencyStatus,
    orgVerbatimMultilineBlock,
    orgWord,
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
    orgNowebMultilineBlock, orgSnippetMultilineBlock, orgUserNode
  }

  orgAllKinds* = { low(OrgNodeKind) .. high(OrgNodeKind) }

type
  OrgSubKindError* = ref object of CatchableError
    subkind: OrgNodeSubKind

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

type
  NowebSlice* = object
    isPlaceholder*: bool
    slice*: PosStr

  NowebBlock* = object
    slices*: seq[NowebSlice]

  SnippetSlice* = object
    hasBody*: bool
    isPlaceholder*: bool
    slice*: PosStr

  SnippetBlock* = object
    slices*: seq[SnippetSlice]

  OrgNodeObj* = object
    subkind*: OrgNodeSubKind
    line*: int
    column*: int
    case kind*: OrgNodeKind
      of orgTokenKinds:
        text*: PosStr

      of orgNowebMultilineBlock:
        nowebBlock*: NowebBlock

      of orgSnippetMultilineBlock:
        snippetBlock*: SnippetBlock

      of orgUserNode:
        userNode*: OrgUserNode

      else:
        ranges*: PosStr
        str*: string
        subnodes*: seq[OrgNode]


  OrgNode* = ref OrgNodeObj

type
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
    obiExample   = "EXAMPLE"

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


  MetaTagKind* = enum
    smtArg      = "arg" ## Procedure argument
    smtParam    = "param" ## Generic entry parameter
    smtRet      = "ret" ## Procedure return value
    smtEnum     = "enum" ## Reference enum, enum value, or set of values.
    smtGlobal   = "global" ## Reference to global variable or constant
    smtAccs     = "accs" ## Documented access to external state (most often
                         ## global variable, file, or environment variable)
    smtField    = "field" ## Entry field
    smtCat      = "cat" ## Entry category name
    smtFile     = "file" ## Filesystem filename
    smtDir      = "dir" ## Filesystem directory
    smtEnv      = "env" ## Environment variable
    smtKbdChord = "kdb" ## Keyboard chord (multiple key combinations)
    smtKbdKey   = "key" ## Single keyboard key combination (key + modifiers)
    smtOption   = "option" ## CLI option
    smtSh       = "sh" ## Execute (simple) shell command
    smtAbbr     = "abbr" ## Abbreviation like CPS, CLI
    smtInject   = "inject" ## Identifier injected in scope
    smtEDSL     = "edsl" ## Embedded DSL syntax description in Extended BNF
                         ## notation
    smtPatt     = "patt"
    smtImport   = "import"
    smtUnresolved ## Unresolved metatag. User-defined tags SHOULD be
                  ## converted to `smtOther`. Unresolved tag MIGHT be
                  ## treated as error/warning when generating final export.
    smtValue    = "value" ## Procedure argument/return value, or field
    ## state that has some additional semantic meaning. For example, exit
    ## codes should ideally be documented using
    ##
    ## ```org
    ## - @value{-1} :: Documentation for return value `-1`. Might also
    ##   `@import{}` or link (using `[[code:]]` or other methods) different
    ##   lists/enums (for example if return value is mapped to an enum)
    ## ```
    smtOther ## Undefined metatag


type
  ## Primitive org-mode types

  OrgCompletion* = object
    ## Completion status cookie
    case isPercent*: bool
      of true:
        percent*: float

      of false:
        done*: int
        total*: int

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



type
  TreeScope* = object
    ## Subtree scope. Mostly used for internal implementation in sempass
    tree*: SemOrg

  CodeResult* = object
    # - TODO :: determine if (and how) results of multistage execution
    #  should be represented (compilation (potentially complex one) +
    #  execution)
    # Result oc code block compilation and execution.
    execResult*: Option[ShellResult]
    compileResult*: Option[ShellResult]


  CodeEvalPost* = object


  BlockCommand = ref object of RootObj
    associative: seq[SemOrg] ## List of associative properties for block command


  CodeBlock* = ref object of BlockCommand
    ## Abstract root class for code blocks
    blockArgs*: CliApp
    code*: string ## Source code body - possibly untangled from `noweb`
    ## block

    langName* {.Attr.}: string

    execResult*: Option[CodeResult] ## Result of code block execution might
    ## be filled from parsed source code or generated using code block
    ## evaluation stage. In latter case it is possible to determine
    ## differences between results and report them if necessary.



  CodeRunContext* = object
    # TODO also add cumulative hash for all code block sequences
    prevBlocks*: Table[string, seq[CodeBlock]] ## List of previous blocks
    ## for each session.

  SymKind = enum
    symNamed
    symRadioTarget
    symHeader
    symRegularTarget
    symCalloutTarget

  SemConvertCtx* = object
    symTable*: array[SymKind, Table[string, SemOrg]]
    scope*: seq[TreeScope]

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

  OrgUserLink* = ref object of RootObj
    defaultField: char

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
        codeLink*: OrgUserLink

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
    ## `orgCommand`

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
    opkTagConf # TODO https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance
    opkLatexHeader
    opkOtherProperty

  OrgPropertyArg* = object
    key*: PosStr
    value*: PosStr

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
    flags*: seq[PosStr]
    args*: seq[OrgPropertyArg]
    case kind*: OrgPropertyKind
      of opkAuthor, opkName, opkUrl:
        rawText*: string

      of opkTitle:
        text*: SemOrg

      of opkAttr:
        backend*: PosStr ## `#+attr_<backend>`. All arguments are in
                           ## `flags` and `args`.

      of opkInclude:
        # TODO included file should support file search patterns
        # https://orgmode.org/manual/Include-Files.html
        # https://orgmode.org/manual/Search-Options.html#Search-Options
        includeFile*: OrgFile

      of opkLinkAbbrev:
        abbrevId*: PosStr
        linkPattern*: PosStr

      of opkFiletags:
        filetags*: seq[PosStr]

      else:
        discard

  SmtAccsKind* = enum
    oakRead
    oakWrite
    oakDelete
    oakCreate

  MetaTag* = ref object
    case kind*: MetaTagKind
      of smtAccs:
        accsKind*: set[SmtAccsKind]
        accsTarget*: MetaTag ## Access target. `@global{}`, `@file{}`,
                                ## `@dir{}`, `@env{}`


      of smtSh:
        shHasRoot*: bool
        shCmd*: ShellCmd

      of smtImport:
        importLink*: OrgLink

      else:
        discard

  ListItemTagKind* = enum
    ## Tag kinds for description list
    sitText ## Regular text
    sitMeta ## Only metatags
    sitBigIdent ## Only big idents

  ListItemTag* = object
    case kind*: ListItemTagKind
      of sitMeta:
        meta*: seq[MetaTag]

      of sitBigIdent:
        idText*: string
        idKind*: seq[OrgBigIdentKind]

      of sitText:
        text*: SemOrg

  AssocEntry* = object
    name*: string
    body*: SemOrg

  Subtree* = ref object
    level*: int
    properties*: Table[string, string]
    completion*: Option[OrgCompletion]
    tags*: seq[string]

  ListItem = ref object
    bullet*: string
    counter*: Option[SemOrg]
    checkbox*: Option[SemOrg]
    tag*: Option[ListItemTag]
    header*: SemOrg
    body*: Option[SemOrg]

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

    case isGenerated* {.Skip(IO).}: bool ## Can be `true` for sem nodes
      ## generated in subsequent stages (mostly code execution, but include
      ## directive resolution as well as several others can also produce
      ## new blocks)
      of true:
        str* {.Attr.}: string

      of false:
        slice* {.Skip(IO).}: Option[PosStr]
        node* {.requiresinit, Skip(IO).}: OrgNode ## Original org-mode
                                                  ## parse tree node.

    subnodes*: seq[SemOrg]
    properties*: Table[string, OrgProperty] ## Property from associative list

    subkind* {.Attr.}: OrgNodeSubKind
    case kind*: OrgNodeKind
      of orgSubtree:
        subtree*: Subtree

      of orgSrcCode:
        codeBlock*: CodeBlock

      of orgAssocStmtList:
        rs*: seq[AssocEntry]

      of orgLink:
        linkTarget*: OrgLink ## Optional reference to target node within
        ## document
        linkDescription*: Option[SemOrg]

      of orgProperty:
        property*: OrgProperty ## Standalone property

      of orgBigIdent:
        bigIdentKind*: OrgBigIdentKind

      of orgDocument:
        ## Document-level properties collected during conversion from parse
        ## tree.
        discard

      of orgListItem:
        listItem*: ListItem

      of orgMetaTag:
        metaTag*: MetaTag

      else:
        discard

storeTraits(SemOrg)
storeTraits(OrgProperty)
storeTraits(OrgPropertyArg)
storeTraits(OrgFile)
storeTraits(OrgCompletion)
storeTraits(AssocEntry)
storeTraits(CodeBlock)
storeTraits(CodeEvalPost)
storeTraits(CodeResult)
storeTraits(OrgDir)
storeTraits(OrgLink)
storeTraits(ListItemTag)
storeTraits(MetaTag)

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
  CodeBuilder* = proc(): CodeBlock
  RunConf* = object
    tempDir*: AbsDir
    codeCreateCallbacks*: Table[string, CodeBuilder]
    linkResolver*: proc(linkName: string, linkText: PosStr): OrgLink

  ParseConf* = object
    dropEmptyWords*: bool

  Exporter* = ref object of RootObj
    name*: string


method exportTo*(
    exporter: Exporter,
    tree: SemOrg,
    file: AbsFile,
    conf: RunConf
  ) {.base.} =

  raise newImplementBaseError(Exporter(), "exportTo")

method runCode*(
    codeBlock: CodeBlock,
    context: var CodeRunContext,
    conf: RunConf
  ) {.base.} =

  raise newImplementBaseError(CodeBlock(), "runCode")

method parseFrom*(
    codeBlock: CodeBlock, semorg: OrgNode,
    scope: var SemConvertCtx) {.base.} =
  ## Parse code block body from semorg node. This method is called from
  ## top-level convert dispatcher loop using
  ## `parseFrom(semorg.codeBloc,semorg)` to trigger runtime dispatch.
  ## Overrides for this method can set only `codeBlock` argument, or modify
  ## `semorg` too, it doesn't really matter.
  raise newImplementBaseError(CodeBlock(), "parseFrom")

method blockPPtree*(
    codeBlock: CodeBlock, conf: var PPrintConf): PPrintTree {.base.} =
  pptree(codeBlock, conf)


proc `[]=`*(conf: var RunConf, lang: string, codeBuilder: CodeBuilder) =
  conf.codeCreateCallbacks[lang] = codeBuilder

proc initRunConf*(): RunConf =
  RunConf(tempDir: getAppTempDir())

proc initSemConvertCtx*(): SemConvertCtx =
  SemConvertCtx()
