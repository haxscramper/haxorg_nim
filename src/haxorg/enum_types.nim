type
  OrgHorizontalDirection* = enum
    ## Horizontal positioning
    ohdNone ## No specific positioning requirements

    ohdLeft ## Align to the left
    ohdRight ## Align to the right
    ohdCenter ## Align to the center

  OrgVerticalDirection* = enum
    ## Vertical positioning
    ovdNone ## No specific positioning

    ovdTop ## Align to the top
    ovdCenter ## Center
    ovdBottom ## Bottom

type
  OrgNodeSubKind* = enum
    ## Additional node classification that does not warrant own AST
    ## structure, but could be very useful for further processing.
    ##
    ## This list tries to cover *all* possible combinations of uses for
    ## each identifier.
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
    oskOrgMetaTag

    oskLinkFile
    oskLinkWeb
    oskLinkId
    oskLinkCallout
    oskLinkSubtree
    oskLinkFreeform ## Unformatted link in the text
    oskLinkImplicit ## Link with implicit resolution for target -
                    ## `[[link-target]]` will try to resolve to existing
                    ## `<<target>>`, then to existing named entry.

    oskHashTagIdent
    oskSymbolIdent
    oskBracTagIdent
    oskOrgTagIdent
    oskOrgMetaTagIdent
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
    ## have similar syntax, but inline ones are pretty different. #[ DOC
    ## why? ]#
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
    orgUnparsed ## Part of the org-mode document that is yet to be parsed.
                ## This node should not be created manually, it is only
                ## used for handling mutually recursive DSLs such as
                ## tables, which might include lists, which in turn might
                ## contain more tables in different bullet points.

    orgCommand ## Single-line command


    # Single-line commands start
    orgCommandTitle ## `#+title:` - full document title
    orgCommandAuthor ## `#+author:`
    orgCommandInclude ## `#+include:` - include other org-mode document (or
                      ## subsection of it), source code or backend-specific
                      ## chunk.
    orgCommandName ## `#+name:` - name of the associated entry
    orgCommandHeader ## `#+header:` - extended list of parameters passed to
                     ## associated block
    orgCommandOptions ## `#+options:` - document-wide formatting options

    orgCommandBackendOptions ## Backend-specific configuration options like
                             ## `#+latex_header`, `#+latex_class` etc.

    orgAttrImg

    orgCommandCaption ## `#+caption:` command
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
    orgCmdNamedValue ## Key-value pair for source code block call.

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
    orgNewline

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

  OrgTextContext* = enum
    otcPlain

    otcSubtree0
    otcSubtree1
    otcSubtree2
    otcSubtree3
    otcSubtree4
    otcSubtree5
    otcSubtree6
    otcSubtree7
    otcSubtree8
    otcSubtree9
    otcSubtree10
    otcSubtree11
    otcSubtree12
    otcSubtreeOther

    # markup kinds begin
    otcBold
    otcItalic
    otcStrike
    otcUnderline

    otcMonospaceInline
    otcMonospaceBlock
    # markup kinds end

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


    obiStructIf = "IF" ## @pushgroup{structured-english}
    obiStructAnd = "AND"
    obiStructOr = "OR"
    obiStructNot = "NOT"
    obiStructGet = "GET"
    obiStructSet = "SET"
    obiStructThen = "THEN"
    obiStructElse = "ELSE"
    obiStructWhile = "WHILE" ## @popgroup{} It is not hard to support
    ## https://en.wikipedia.org/wiki/Structured_English keywords. Maybe I
    ## will merge it with haxdoc somehow, maybe not, for not I just placed
    ## them here as a reminder to myself. My current idea is to overlay
    ## semi-structured explanation in the documenation with actual code.
    ## Structured keywords can be used as an anchor points (e.g. `IF` maps
    ## to real condition, `THEN` to the body and so on).

  OrgMetaTagKind* = enum
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
  OrgTokenKind* = enum
    otNone
    otEof

    # === structure tokens begin
    OStCommandPrefix
    OStIdent
    OStLineCommand
    OStCommandBegin ## `#+begin` part of the multiline command.
    ## `begin_<block-type>` is split into two tokens - `begin_` prefix and
    ## `ockBegin<block-type>` section.
    OStCommandEnd



    OStBigIdent
    OStColon
    OStDoubleColon
    OStText
    OStListDash
    OStListPlus
    OStListStar
    OStListItemEnd ## End of the list item
    OStCheckbox ## List or subtree checkbox

    OStSubtreeTodoState
    OStSubtreeImportance ## Subtree importance marker
    OStSubtreeCompletion ## Subtree completion marker
    OStSubtreeStars ## Subtree prefix
    OStSubtreeTag ## Subtree tag
    OStSubtreeTime
    OStAngleTime
    OStDiaryTime
    OStImplicitTime ## You can write time ranges without any additional
    ## formatting for subtrees that have a diary timestamps. For example,
    ## you have a complex date predicate, but event occurs for
    ## `18:00-21:00`, so you write it in the random place in the subtree.
    OStBracketTime
    OStTimeDash

    OStComment ## line or inline comment
    OStListDoubleColon ## Double colon between description list tag and body
    OStCommandArgumentsBegin ## List of command arguments
    OStCommandArgumentsEnd ## End of the command arguments list
    OStCommandBracket ## `#+results[HASH...]`
    OStColonLiteral ## Literal block with `:`
    OStColonIdent ## Drawer or source code block wrappers with
    ## colon-wrapped identifiers. `:results:`, `:end:` etc.
    OStColonAddIdent ## Add value to the existing drawer property - `:NAME+:`
    OStColonProperties ## Start of the `:PROPERTIES:` block drawer block
    OStColonEnd
    OStColonLogbook
    OStRawLogbook
    OStRawProperty

    OStLink ## Any kind of link
    OStHashTag ## Inline text hashtag

    OStCommandContentStart
    OStCommandContentEnd

    OStCodeContent  ## Block of code inside `#+begin_src`
    OStTableContent ## Block of text inside `#+table`
    OStQuoteContent ## `#+quote` content

    OStBackendPass ## Backend-specific passthrough

    OStLogBook ## Logbook including content
    OStDrawer ## Drawer including content

    OStIndent ## Increase in indentation
    OStDedent ## Decrease in indentation
    OStSameIndent
    OStNoIndent

    # === structure tokens end
    #
    # === text tokens begin

    OTxBoldOpen, OTxBoldClose, OTxBoldInline
    OTxItalicOpen, OTxItalicClose, OTxItalicInline
    OTxVerbatimOpen, OTxVerbatimClose, OTxVerbatimInline
    OTxMonospaceOpen, OTxMonospaceClose, OTxMonospaceInline
    OTxBacktickOpen, OTxBacktickClose, OTxBacktickInline
    OTxUnderlineOpen, OTxUnderlineClose, OTxUnderlineInline
    OTxStrikeOpen, OTxStrikeClose, OTxStrikeInline
    OTxQuoteOpen, OTxQuoteClose

    OTxPlaceholderOpen, OTxPlaceholderClose
    OTxTargetOpen, OTxTargetClose
    OTxRadiOTbrgetOpen, OTxRadiOTbrgetClose

    OTxLinkOpen, OTxLinkClose
    OTxLinkTargetOpen, OTxLinkTargetClose
    OTxLinkInternal ## No protocol is used in the link, it is targeting
                    ## some internal named entry.
    OTxLinkProtocol ## Protocol used by the link - `file:`, `https:` etc.
    OTxLinkFull ## Full token for the link, used in cases where it does not
                ## make sense to fracture the token - regular https URLs
                ## etc.
    OTxLinkHost ## Host part of the URI used in link
    OTxLinkPath ## Path part of the link
    OTxLinkTarget ## Target of the link protocol that does not follow
                  ## regular URI encoding scheme - for example `id:`,
                  ## `elisp`, or `shell` links.
    OTxLinkExtraSeparator ## Separator of the extra content in the link, `::`
    OTxLinkExtra ## Additional parametrization for the link search
    OTxLinkDescriptionOpen, OTxLinkDescriptionClose

    OTxParagraphStart ## Fake token inserted by the lexer to delimit start
                      ## of the paragraph
    OTxParagraphEnd

    OTxFootnoteStart
    OTxFootnoteEnd

    OTxWord
    OTxNewline
    OTxMaybeWord
    OTxSpace
    OTxBigIdent
    OTxRawText
    OTxInlineSrc ## Inline source code block: `src_nim[]{}`
    OTxInlineCall

    OTxDollarOpen ## Opening dollar inline latex math
    OTxDollarClose ## Closing dollar for inline latex math
    OTxDoubleDollarOpen
    OTxDoubleDollarClose
    OTxLatexParOpen ## Opening `\(` for inline latex math
    OTxLatexParClose ## Closing `\)` for inline latex math
    OTxLatexBraceOpen ## Opening `\[` for inline display latex equation
    OTxLatexBraceClose ## Closing `\]` for inline display latex equation
    OTxLatexInlineRaw

    OTxDoubleAt ## Inline backend passthrough `@@`
    OTxAtBracket ## Inline annOTbtion
    OTxAtMetaTag
    OTxAtMention
    OTxTagParams

    OTxLink

    OTxSlashEntry

    OTxHashTag

    OTxMacroOpen, OTxMacroBody, OTxMacroClose
    OTxMetaOpen, OTxMetaName, OTxMetaBody, OTxMetaClose

    OTxSrcOpen, OTxSrcName, OTxSrcArgs, OTxSrcBody, OTxSrcClose

    OTxCallOpen, OTxCallName, OTxCallInsideHeader,
    OTxCallArgs, OTxEndHeader, OTxCallClose

    # === text tokens end
    #
    # === table tokens begin

    OTbCmdArguments

    OTbTableBegin
    OTbTableEnd
    OTbCellBody ## Unformatted table cell body
    OTbRowSpec ## `#+row` command together with parameters
    OTbCellSpec ## `#+cell` command with parameters

    OTbContent

    OTbPipeOpen
    OTbPipeSeparator ## Vertical pipe (`|`) cell separator
    OTbPipeClose
    OTbPipeCellOpen

    OTbDashSeparator ## Horizontal dash (`---`, `:---`, `---:` or `:---:`)
                      ## row separator
    OTbCornerPlus ## Corner plus (`+`)

    # === table tokens end
    #
    # === command tokens begin

    OCmCommand
    OCmCommandArgs
    OCmCommandBegin
    OCmCommandPrefix
    OCmCommandEnd
    OCmBody
    OCmLangName
    OCmNewline
    OCmNowebOpen ## `<<` - open for noweb placeholder
    OCmNowebClose ## `>>` - close for noweb placeholder
    OCmNowebName ## Name of the noweb placeholder
    OCmNowebLpar ## Lpar of the noweb placeholder arguments
    OCmNowebRpar ## RPar of the noweb placeholder arguments
    OCmNowebComma ## Noweb argument separator
    OCmNowebArg ## Noweb argument
    OCmTextBlock ## Code before noweb placeholder. Requires separate token
                  ## to handle `##<<commented>>` - prefix comment should be
                  ## duplicated for each line of the placeholder expansion.

    OCmCalloutOpen
    OCmCalloutName
    OCmCalloutClose
