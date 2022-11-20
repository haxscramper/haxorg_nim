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
  # OrgNodeSubKind* = enum
  #   ## Additional node classification that does not warrant own AST
  #   ## structure, but could be very useful for further processing.
  #   ##
  #   ## This list tries to cover *all* possible combinations of uses for
  #   ## each identifier.
  #   oskNone


  #   oskBold ## Node is bold text
  #   oskItalic
  #   oskVerbatim
  #   oskMonospaced
  #   oskBacktick
  #   oskUnderline
  #   oskStrike
  #   oskQuote ## Line quote text `> sometext`
  #   oskAngle


  #   oskDescriptionTagText ## Description list tag text
  #   oskLinkContent ## Link description text
  #   oskTitleText ## Paragraph in title of the subtree
  #   oskCaptionText ## Paragraph in `#+caption:`
  #   oskListHeaderText
  #   oskListBodyText
  #   oskListTagText
  #   oskStandaloneText
  #   oskSrcInlineText
  #   oskCallInlineText

  #   oskMetatagText ## Raw content of the metatag
  #   oskMetatagArgs
  #   oskLinkAddress
  #   oskComment
  #   oskOrgMetaTag

  #   oskLinkFile
  #   oskLinkWeb
  #   oskLinkId
  #   oskLinkCallout
  #   oskLinkSubtree
  #   oskLinkFreeform ## Unformatted link in the text
  #   oskLinkImplicit ## Link with implicit resolution for target -
  #                   ## `[[link-target]]` will try to resolve to existing
  #                   ## `<<target>>`, then to existing named entry.

  #   oskHashTagIdent
  #   oskSymbolIdent
  #   oskBracTagIdent
  #   oskOrgTagIdent
  #   oskOrgMetaTagIdent
  #   oskTodoIdent


  #   oskDashBullet
  #   oskPlusBullet
  #   oskStarBullet

  #   oskRomanBullet
  #   oskNumBullet
  #   oskLetterBullet

  #   oskOrderedList
  #   oskUnorderedList
  #   oskMixedList
  #   oskFullDescList
  #   oskPartialDescList



  #   oskText
  #   oskSpace
  #   oskParen
  #   oskBracket
  #   oskCurly
  #   oskPunct
  #   oskBigWord

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

    orgError ## Failed node parse - technically there are no /wrong/ syntax
    ## in the org-mode document because everything can be considered a one
    ## large word or a paragraph with flat `Word` content.
    ##
    ## Error node can be produced by any parsing routine, although it is
    ## mostly used in the low-level text elements, since high-level
    ## structures are mostly detected based on the correct syntax - for
    ## example, `*** subtree` (and any title variations) can never be an
    ## error in itself. Title /text/ might contain an error, but invalid it
    ## is not possible to write an invalid subtree - it is either `*
    ## ANYTHING` or not a subtree at all.

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
    orgCommandAuthor ## `#+author:` Document author
    orgCommandCreator ## `#+creator:` Document creator
    orgCommandInclude ## `#+include:` - include other org-mode document (or
                      ## subsection of it), source code or backend-specific
                      ## chunk.
    orgCommandLanguage ## `#+language:`
    orgCommandAttrHtml ## `#+attr_html:`
    orgCommandName ## `#+name:` - name of the associated entry
    orgCommandHeader ## `#+header:` - extended list of parameters passed to
                     ## associated block
    orgCommandOptions ## `#+options:` - document-wide formatting options

    orgCommandBackendOptions ## Backend-specific configuration options like
                             ## `#+latex_header`, `#+latex_class` etc.

    orgAttrImg

    orgCommandCaption ## `#+caption:` command
    # Single-line commands end


    orgFile

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
    orgCodeTangle ## Single tangle target in the code block
    orgCodeCallout ## `(refs:` callout in the source code

    orgSrcCode ## Block of source code - can be multiline, single-line and
    ##

    orgSrcInlineCode ## inline piece of code (such as `src_nim`). Latter is
    ## different from regular monospaced text inside of `~~` pair as it
    ## contains additional internal structure, optional parameter for code
    ## evaluation etc.
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

    orgInlineMath ## Inline latex math. Contains latex math body - either
    ## from `$dollar-wrapped$` or `\(paren-wrapped\)` inline text.
    orgDisplayMath ## Inline display latex math from `$$double-dollar$$` or
    ## `\[bracket-wrapped\]` code.

    orgSpace ## Space or tab character in regular text
    orgPunctuation
    orgWord ## Regular word - technically not different from `orgIdent`,
    ## but defined separately to disiguish between places where special
    ## syntax is required and free-form text.
    orgEscaped ## Escaped formatting character in the text
    orgNewline

    orgRawLink ## Raw unwrapped link that was pasted in text
    orgLink ## External or internal link. Consists of one or two elements -
    ## target (url, file location etc.) and description (`orgParagraph` of
    ## text). Description might be empty, and represented as empty node in
    ## this case. For external links particular formatting of the address
    ## is not handled by parser and instead contains raw string from input
    ## text.


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

    orgTimeAssoc ## Time association pair for the subtree deadlines.
    orgTimeStamp ## Single date and time entry (active or inactive),
    ## possibly with repeater interval. Is not parsed directly, and instead
    ## contains `orgRawText` that can be parsed later

    orgTimeRange ## Date and time range format - two `orgDateTime` entries
    orgSimpleTime ## Result of the time range evaluation or trailing
    ## annotation a subtree

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

    orgInlineFootnote ## Inline footnote with text placed directly in the
    ## node body.
    orgFootnote ## Footnote entry. Just as regular links - internal content
    ## is not parsed, and instead just cut out verbatim into target AST
    ## node.

    orgHorizontal ## Horizotal rule. Rule body might contain other
    ## subnodes, to represnt `---- some text ----` kind of formatting.

    orgFiletags ## `#+filetags:` line command
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

    orgMetaSymbol ## `\sym{}` with explicit arguments
    orgAtMention ## `@user`

    orgBracTag ## Custom extension to org-mode. Similarly to `BigIdent`
    ## used to have something like informal keywords `MUST`, `OPTIONAL`,
    ## but instead aimed /specifically/ at commit message headers -
    ## `[FEATURE]`, `[FIX]` and so on.

    orgDrawer ## Single enclosed drawer like `:properties: ... :end:` or
    ## `:logbook: ... :end:`

    orgColumns ## `#+columns:` line command for specifying formatting of
    ## the org-mode clock table visualization on per-file basis.
    orgPropertyList
    orgProperty ## Property entry, either in `#+property:` command, or in
                ## `:property:` drawer
    orgPropertyAdd ## Property value extensions - `:property+:`

    orgPlaceholder ## Placeholder entry in text, usually writte like `<text
                   ## to replace>`

    orgLogbook ## `:logbook:` entry storing note information
    orgLogbookStateChange ## Annotation about change in the subtree todo state
    orgLogbookNote ## Timestamped log note on the subtree
    orgLogbookClock ## `CLOCK` entry in the subtree
    orgLogbookRefile ## `Refile` entry in the subtree logbook drawer

    orgRadioTarget ## `<<<RADIO>>>`
    orgTarget ## `<<TARGET>>`

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
    OTkCommandPrefix
    OTkLineCommand
    OTkCommandBegin ## `#+begin` part of the multiline command.
    ## `begin_<block-type>` is split into two tokens - `begin_` prefix and
    ## `ockBegin<block-type>` section.
    OTkCommandEnd



    OTkDoubleColon
    OTkText
    OTkStmtList ## Unlexed group of statements - used in the list content
    ## to enable secondary parsing.
    OTkStmtListOpen ## Start of the expanded statement list content
    OTkStmtListClose ## End of the expanded statement list content
    OTkListStart ## Start of the list token group
    OTkListDash
    OTkListPlus
    OTkListStar
    OTkListDescOpen ## Start of the description list key,
    OTkListDescClose ## End of the description list key `::`
    OTkListItemEnd ## End of the list item
    OTkListEnd ## Complete end of the list token group
    OTkCheckbox ## List or subtree checkbox

    OTkSubtreeTodoState
    OTkSubtreeUrgency ## Subtree importance marker
    OTkSubtreeCompletion ## Subtree completion marker
    OTkSubtreeStars ## Subtree prefix
    OTkSubtreeTag ## Subtree tag
    OTkSubtreeTime
    OTkSubtreeEnd
    OTkAngleTime ## Active timestamp token
    OTkDiaryTime ## Active timestamp with S-expression to check the time
    OTkImplicitTime ## You can write time ranges without any additional
    ## formatting for subtrees that have a diary timestamps. For example,
    ## you have a complex date predicate, but event occurs for
    ## `18:00-21:00`, so you write it in the random place in the subtree.
    OTkTimeDuration ## Time duration for the `effort` property or time
    ## range length evaluation
    OTkBracketTime ## Inactive timestamp token
    OTkTimeDash ## Separator dash between two periods in the time range
    ## (`<start>--<finish.`)
    OTkTimeArrow ## Time range evaluation arrow `[from]--[to] =>`

    OTkComment ## line or inline comment
    OTkListDoubleColon ## Double colon between description list tag and body
    OTkCommandArgumentsBegin ## List of command arguments
    OTkCommandArgumentsEnd ## End of the command arguments list
    OTkCommandKey
    OTkCommandValue
    OTkCommandFlag
    OTkCommandBracket ## `#+results[HASH...]`
    OTkColonLiteral ## Literal block with `:`
    OTkColonIdent ## Drawer or source code block wrappers with
    ## colon-wrapped identifiers. `:results:`, `:end:` etc.
    OTkColonAddIdent ## Add value to the existing drawer property - `:NAME+:`
    OTkColonProperties ## Start of the `:PROPERTIES:` block drawer block
    OTkColonEnd
    OTkColonLogbook
    OTkRawLogbook
    OTkLogbookStart
    OTkLogbookEnd
    OTkRawProperty

    OTkLink ## Any kind of link

    OTkCommandContentStart
    OTkCommandContentEnd

    # Code block content lexing. Additional syntax elements such as
    # callouts and tangle targets are represented using smaller punctuation
    # pieces from the regular text.
    OTkCodeContent  ## Block of code inside `#+begin_src`
    OTkCodeContentBegin ## Start of the expanded code content
    OTkCodeContentEnd ## End of the expanded code content
    OTkCodeText ## Uninterrupted text span without newlines - either a
    ## whole line or sub subsection of it if callout or tangle elements
    ## were detected

    OTkTableContent ## Block of text inside `#+table`
    OTkQuoteContent ## `#+quote` content

    OTkBackendPass ## Backend-specific passthrough

    OTkLogBook ## Logbook including content
    OTkDrawer ## Drawer including content

    OTkIndent ## Increase in indentation
    OTkDedent ## Decrease in indentation
    OTkSameIndent
    OTkNoIndent

    # === structure tokens end
    #
    # === text tokens begin

    OTkBoldOpen, OTkBoldClose, OTkBoldInline
    OTkItalicOpen, OTkItalicClose, OTkItalicInline
    OTkVerbatimOpen, OTkVerbatimClose, OTkVerbatimInline
    OTkMonospaceOpen, OTkMonospaceClose, OTkMonospaceInline
    OTkBacktickOpen, OTkBacktickClose, OTkBacktickInline
    OTkUnderlineOpen, OTkUnderlineClose, OTkUnderlineInline
    OTkStrikeOpen, OTkStrikeClose, OTkStrikeInline
    OTkQuoteOpen, OTkQuoteClose
    OTkPunctuation

    OTkLinkOpen, OTkLinkClose
    OTkRawUrl
    OTkLinkTargetOpen, OTkLinkTargetClose
    OTkLinkInternal ## No protocol is used in the link, it is targeting
                    ## some internal named entry.
    OTkLinkProtocol ## Protocol used by the link - `file:`, `https:` etc.
    OTkLinkFull ## Full token for the link, used in cases where it does not
                ## make sense to fracture the token - regular https URLs
                ## etc.
    OTkLinkHost ## Host part of the URI used in link
    OTkLinkPath ## Path part of the link
    OTkLinkTarget ## Target of the link protocol that does not follow
                  ## regular URI encoding scheme - for example `id:`,
                  ## `elisp`, or `shell` links.
    OTkLinkExtraSeparator ## Separator of the extra content in the link, `::`
    OTkLinkExtra ## Additional parametrization for the link search
    OTkLinkDescriptionOpen, OTkLinkDescriptionClose

    OTkParagraphStart ## Fake token inserted by the lexer to delimit start
                      ## of the paragraph
    OTkParagraphEnd

    OTkFootnoteStart
    OTkFootnoteEnd

    OTkWord ## Regular word in the paragraph
    OTkEscaped ## Escaped character in plain text - `\*`, `\/` etc. Escaped
    ## characters and sequences thereof are treated like a regular plain
    ## text.
    OTkDoubleSlash ## Put at the end of the lexer first logbook line to
    ## separate the note, otherwise is treated as standalone escaped slash.
    OTkNewline ## Explicit newline a paragraph
    OTkMaybeWord
    OTkSpace ## Space in the paragraph
    OTkBigIdent ## `TODO`, `NOTE` and similar capitalized words
    OTkRawText ## Unparsed raw text, either as a part of paragraph or some
    ## embedded construction such as link address.
    OTkInlineSrc ## Start of an inline source code block: `src_nim[]{}`
    OTkInlineCall ## Start of an inline call block: `call_name[]{}`
    OTkCurlyStart ## Start of the curly section of an inline source/call
    OTkCurlyEnd ## End of the curly section of an inline source/call

    OTkSymbolStart ## Unquoted `\symbol` directly in the text
    OTkIdent
    OTkDollarOpen ## Opening dollar inline latex math
    OTkDollarClose ## Closing dollar for inline latex math
    OTkDoubleDollarOpen ## Opening `$` for inline latex
    OTkDoubleDollarClose ## Closing `$` for inline latex
    OTkLatexParOpen ## Opening `\(` for inline latex math
    OTkLatexParClose ## Closing `\)` for inline latex math
    OTkLatexBraceOpen ## Opening `\[` for inline display latex equation
    OTkLatexBraceClose ## Closing `\]` for inline display latex equation
    OTkLatexInlineRaw ## Content of the brace/par-enclosed math

    OTkDoubleAt ## Inline backend passthrough `@@`
    OTkAtBracket ## Inline annotation
    OTkAtMention ## `@user` mention in the text

    OTkHashTag ## Start of the inline hashtag `#tag`
    OTkHashTagSub ## Nested hashtag separator
    OTkHashTagOpen ## Start of the nested hashtag grop bracket
    OTkHashTagClose ## End of the nested hashtag group separator

    OTkComma ## Comma - punctuation or a syntax element (e.g. for macro
             ## arguments)
    OTkParOpen ## Paren open - punctuation or a syntax element
    OTkParClose ## Paren close - punctuation or a syntax element
    OTkColon
    OTkCircumflex ## `^` possible superscript in the text


    OTkMacroOpen ## Start of the macro call `{{{`
    OTkMacroClose ## Close of the macro call `}}}`
    OTkMetaBraceOpen
    OTkMetaBraceBody
    OTkMetaBraceClose
    OTkMetaArgsOpen
    OTkMetaArgsBody
    OTkMetaArgsClose

    OTkSrcOpen, OTkSrcName, OTkSrcArgs, OTkSrcBody, OTkSrcClose

    OTkCallOpen, OTkCallName, OTkCallInsideHeader,
    OTkCallArgs, OTkEndHeader, OTkCallClose

    # === text tokens end
    #
    # === table tokens begin

    OTkCmdArguments

    OTkTableBegin
    OTkTableEnd
    OTkCellBody ## Unformatted table cell body
    OTkRowSpec ## `#+row` command together with parameters
    OTkCellSpec ## `#+cell` command with parameters

    OTkContent ## Temporary token created during initial content lexing
    OTkContentStart ## Start of the table cell content section
    OTkContentEnd ## End of the table cell content section

    OTkPipeOpen
    OTkPipeSeparator ## Vertical pipe (`|`) cell separator
    OTkPipeClose
    OTkPipeCellOpen

    OTkDashSeparator ## Horizontal dash (`---`, `:---`, `---:` or `:---:`)
                      ## row separator
    OTkCornerPlus ## Corner plus (`+`)

    # === table tokens end
    #
    # === command tokens begin

    OTkCommand
    OTkCommandArgs
    OTkBody
    OTkLangName
    OTkDoubleAngleOpen ## `<<` - open for noweb or anchor placeholder
    OTkDoubleAngleClose ## `>>` - close for noweb or anchor placeholder
    OTkTripleAngleOpen ## `<<<` - radio target open
    OTkTripleAngleClose ## `>>>` - radio target close
    OTkAngleOpen ## Placeholder open
    OTkAngleClose ## Placeholder close
    OTkTextBlock ## Code before noweb placeholder. Requires separate token
                  ## to handle `##<<commented>>` - prefix comment should be
                  ## duplicated for each line of the placeholder expansion.


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
    orgTarget,
    orgRawLink,
    orgRadioTarget,
    orgCmdFlag,
    orgOrgTag,
    orgCodeText,
    orgSubtreeStars,
    orgSpace,
    orgPunctuation,
    orgAtMention,

    orgIdent,
    orgBullet,
    orgBareIdent,
    orgRawText,
    orgUnparsed,
    orgBigIdent,
    orgUrgencyStatus,
    orgVerbatimMultilineBlock,
    orgWord,
    orgEscaped,
    orgNewline,
    orgComment,
    orgCheckbox,
    orgCounter,
    orgCompletion,
    orgTimeStamp,
    orgSimpleTime,
    orgEmptyNode
  }

  orgSubnodeKinds* = {
    low(OrgNodeKind) .. high(OrgNodeKind)
  } - orgTokenKinds - {
    orgUserNode
  }

  orgAllKinds* = { low(OrgNodeKind) .. high(OrgNodeKind) }
