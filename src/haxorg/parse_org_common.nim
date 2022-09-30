import
  hmisc/core/all,
  hmisc/algo/hlex_base

const
  OCommandChars* = IdentChars + {'-', '_'}

type
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
    ockSetupfile
    ockOtherProperty

    ockBeginTable, ockEndTable ## `#+table`
    ockRow ## `#+row`
    ockCell ## `#+cell`

    ockBeginDynamic, ockEndDynamic ## `#+begin:`
    ockBeginQuote, ockEndQuote ## `#+quote`
    ockBeginSrc, ockEndSrc ## `#+begin_src`
    ockBeginExport, ockEndExport ## `#+end_export`
    ockBeginDetails, ockEndDetails
    ockBeginSummary, ockEndSummary

    ockAttrLatex ## `#+attr_latex:`
    ockAttrImg ## `#+attr_img:`

    ockOptions ## `#+options: `
    ockTitle ## `#+title:`
    ockProperty ## `#+property:`

    ockLatexHeader ## `#+latex_header`
    ockResults ## `#+results:`

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

func classifyCommand*(str: string): OrgCommandKind =
  let norm = str.dashNormalize()
  case norm:
    of "begin": ockBeginDynamic
    of "end": ockEndDynamic

    of "beginsrc": ockBeginSrc
    of "endsrc": ockEndSrc

    of "beginquote": ockBeginQuote
    of "endquote": ockEndQuote

    of "beginexport": ockBeginExport
    of "endexport": ockEndExport

    of "begintable": ockBeginTable
    of "endtable": ockEndTable

    of "title": ockTitle
    of "include": ockInclude
    of "caption": ockCaption
    of "name": ockName
    of "attrimg": ockAttrImg

    of "row": ockRow
    of "cell": ockCell
    of "header": ockHeader
    of "options": ockOptions
    else:
      raise newImplementKindError(norm)


proc classifyCommand*(str: PosStr): OrgCommandKind =
  classifyCommand(str.strVal())
