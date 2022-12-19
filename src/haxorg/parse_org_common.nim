import
  hmisc/core/all,
  hmisc/algo/hlex_base

const
  OCommandChars* = IdentChars + {'-', '_'}

type
  OrgPropertyKind* = enum
    ## Built-in and user-provided properites that can be attached to the
    ## subtrees, files.
    # properties listed in the 'org-default-properties'
    opkArchive
    opkCategory
    opkSummary
    opkDescription
    opkCustomId
    opkLocation
    opkLogging
    opkColumns
    opkVisibility
    opkTableExportFormat
    opkTableExportFile
    opkExportOptions
    opkExportText
    opkExportFileName
    opkExportTitle
    opkExportAuthor
    opkExportDate
    opkUnnumbered
    opkOrdered
    opkNoblocking
    opkCookieData
    opkLogIntoDrawer
    opkRepeatToState
    opkClockModelineTotal
    opkStyle
    opkHtmlContainerClass

    # mentioned in the manual
    opkEffort
    opkEffortAll
    opkId
    opkCreated
    opkHeaderArgs

    opkOther


  
  OrgCommandKind* = enum
    ## Built-in org commands (single and multiline) such as `#+include`
    ##
    ## Explicitly lists all built-in commands and leave escape hatch in
    ## form of `ockOtherProperty` for user-defined properties.
    ##
    ## Properties can be transformed from single-line `orgCommand` entries,
    ## or directly from `orgProperty` in drawer elements (or `#+property`
    ## command)
    ockNone

    ockInclude
    ockFiletags
    ockSetupfile
    ockColumns
    ockOtherProperty

    ockBeginTable, ockEndTable ## `#+table`
    ockRow ## `#+row`
    ockCell ## `#+cell`

    ockBeginAdmonition, ockEndAdmonition
    ockBeginDynamic, ockEndDynamic ## `#+begin:`
    ockBeginCenter, ockEndCenter ## `#+begin_center`
    ockBeginQuote, ockEndQuote ## `#+quote`
    ockBeginSrc, ockEndSrc ## `#+begin_src`
    ockBeginExport, ockEndExport ## `#+end_export`
    ockBeginExample, ockEndExample
    ockBeginDetails, ockEndDetails
    ockBeginSummary, ockEndSummary

    ockLatexClassOptions ## `#+latex_class_options`
    ockLatexClass
    ockLatexCompiler
    ockAttrLatex ## `#+attr_latex:`
    ockAttrImg ## `#+attr_img:`
    ockAttrHtml ## `#+attr_html:`
    ockHtmlHead ## `#+html_head:`
    ockLanguage ## `#+language:`

    ockOptions ## `#+options: `
    ockTitle ## `#+title:`
    ockProperty ## `#+property:`
    ockAuthor ## `#+author:`
    ockBind ## `#+bind:`
    ockCreator ## `#+creator:`

    ockLatexHeader ## `#+latex_header`
    ockResults ## `#+results:`
    ockCall ## `#+call:`

    ockName ## `#+name:`
    ockCaption ## `#+caption:`

    ockHeader ## `#+header:`

  OrgBlockLexerState* = enum
    oblsNone
    oblsInHeader
    oblsInBody
    oblsEnded
    oblsComplete


func dashNormalize*(str: string): string =
  for ch in str:
    if ch in {'a' .. 'z', 'A' .. 'Z'}:
      result.add toLowerAscii(ch)

func classifyProperty*(str: string): OrgPropertyKind =
  let norm = str.strip(chars = {':', '+'}).dashNormalize()
  case norm:
    of "archive": opkArchive
    of "summary": opkSummary
    of "description": opkDescription
    of "customid": opkCustomId
    of "location": opkLocation
    of "logging": opkLogging
    of "columns": opkColumns
    of "visibility": opkVisibility
    of "tableexportformat": opkTableExportFormat
    of "tableexportfile": opkTableExportFile
    of "exportoptions": opkExportOptions
    of "exporttext": opkExportText
    of "exportfilename": opkExportFileName
    of "exporttitle": opkExportTitle
    of "exportauthor": opkExportAuthor
    of "exportdate": opkExportDate
    of "unnumbered": opkUnnumbered
    of "ordered": opkOrdered
    of "nonblocking": opkNoblocking
    of "cookiedata": opkCookieData
    of "logintodrawer": opkLogIntoDrawer
    of "repeattostate": opkRepeatToState
    of "clockmodelinetotal": opkClockModelineTotal
    of "style": opkStyle
    of "htmlcontainerclass": opkHtmlContainerClass

    of "effort": opkEffort
    of "effortall": opkEffortAll
    of "created": opkCreated
    of "id": opkId
    of "headerargs": opkHeaderArgs

    else: opkOther

func classifyCommand*(str: string): OrgCommandKind =
  let norm = str.dashNormalize()
  case norm:
    of "begin": ockBeginDynamic
    of "end": ockEndDynamic

    of "beginsrc": ockBeginSrc
    of "endsrc": ockEndSrc

    of "beginquote": ockBeginQuote
    of "endquote": ockEndQuote

    of "beginexample": ockBeginExample
    of "endexample": ockEndExample

    of "beginexport": ockBeginExport
    of "endexport": ockEndExport

    of "begintable": ockBeginTable
    of "endtable": ockEndTable

    of "begincenter": ockBeginCenter
    of "endcenter": ockEndCenter

    of "title": ockTitle
    of "include": ockInclude
    of "language": ockLanguage
    of "caption": ockCaption
    of "name": ockName
    of "attrimg": ockAttrImg
    of "author": ockAuthor
    of "bind": ockBind
    of "creator": ockCreator
    of "filetags": ockFiletags

    of "htmlhead": ockHtmlHead
    of "attrhtml": ockAttrHtml

    of "row": ockRow
    of "cell": ockCell
    of "header": ockHeader
    of "options": ockOptions
    of "property": ockProperty
    of "columns": ockColumns
    of "results": ockResults
    of "call": ockCall
    of "latexclass": ockLatexClass
    of "latexcompiler": ockLatexCompiler
    of "latexclassoptions": ockLatexClassOptions
    of "beginadmonition": ockBeginAdmonition
    of "endadmonition": ockEndAdmonition
    of "latexheader": ockLatexHeader

    else:
      raise newImplementKindError(norm)


proc classifyCommand*(str: PosStr): OrgCommandKind =
  classifyCommand(str.strVal())
