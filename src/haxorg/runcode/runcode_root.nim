import ../defs/org_types

## Root implementation of the code blocks with shared properties that 
## are available for all block subtypes. New blocks should inherit from
## [[code:RootCodeBlock]] type defined in this module.

type
  ## Shared code block property types

  CodeResCollection* = enum
    ## How the results should be collected from the code block.
    crcOutput ## Stdout/stderr of code block execution
    crcValue ## Actual value of source code block.
    crcValueType ## Value of the source code block and type (if language
                 ## supports types. If not, MIGHT be identical to
                 ## @enum{crcValue})

  CodeResType* = enum
    ## For which type of result the code block will return; affects how Org
    ## processes and inserts results in the Org buffer. When used in
    ## document compilation does not really affect anything, as results are
    ## inserted in AST, not in plaintext form.
    crtVerbatim
    crtTable
    crtList
    crtScalar
    crtFile

  CodeResFormat* = enum
    ## For the result; affects how Org processes results;
    crtDrawer ## Put result in verbatim drawer
    crtHtml ## Result is passthrough html
    crtLatex ## Result is passthrough latex
    crtLink ## Result is single string literal 
    crtGraphics ## Result is a link or path to the image
    crtOrg 
    crtPP
    crtRaw

  CodeResHandling* = enum
    ## For inserting results once they are properly formatted.
    crtReplace ## Replace old result for code block
    crtSilent ## Ignore new result of the code block
    crtAppend ## Append result output to the code block results
    crtPrepend ## Prepend result output to the code block results

  CodeResExports* = enum
    creBoth ## Export both code and produced results
    creCode ## Only export original code
    creResults ## Only results
    creNone ## Do not export code block at all

  CodeEvalComments* = enum
    # TODO DOC
    cecNone
    cecLink
    cecNoweb
    cecOrg
    cecBoth

  CodeEvalWhen* = enum
    cewNever ## Dot not evaluate code block ever
    cewNeverExport ## Do not evaluate on export run
    cewQuery ## Query before evaluation
    cewQueryExport ## Query before exporting

type
  RootCodeBlock* = ref object of CodeBlock
    ## Default block with common shared properties that are available in 
    ## all code blocks
    evalSession*  {.Attr.}: Option[string]
    evalCache*    {.Attr.}: bool ## Avoids re-evaluating unchanged code blocks.
    evalFileDesc* {.Attr.}: Option[string]
    evalMkdirp*   {.Attr.}: bool
    evalShebang*  {.Attr.}: Option[string]

    evalPost*: Option[CodeEvalPost]
    evalFile*: Option[OrgFile]
    evalDir*: Option[OrgDir]
    evalVars*: Table[string, string]


    # - TODO :: add support for separating cmdline pased to /compiler/ and
    #   /compiled executable/. Latter one is far less important, but
    #   sometimes also necessary.
    #
    # - IDEA :: If additional command-line options are present, it might be
    #   a good idea to also support 'execution example' for compiled
    #   binary, so you could show (1) original source code, (2) how you
    #   compile/run resulting executable and finally produced output.
    #   Implicitly passed variables are also important since it might not
    #   be obvious how particular value has been passed from previous
    #   blocks in session. NOTE Intermediate evaluation results could be
    #   implemented by adding `evalIntermediate` field with sequence of
    #   elements. When `runCode` is executed, particular implementation
    #   might append to it.

    evalCmdline*: seq[string]
    evalComments* {.Attr.}: CodeEvalComments
    evalEpilogue*: Option[string]
    evalPrologue*: Option[string]
    evalWhen* {.Attr.}: CodeEvalWhen

    codeHash* {.Attr.}: Hash ## \
    ## Hash for this particular code block source and arguments
    cumulativeHash* {.Attr.}: Hash ## \
    ## Cumulative hash for all code block encountered in the *same session*
    ## during top-down scan of the document.

# 	:noweb-ref (See section 17)
# 	:noweb-sep (See section 18)
# :colnames (See section 5)	:padline (See section 19)
# :comments (See section 6)	:post (See section 20)
# :dir (See section 7)	:prologue (See section 21)
# :epilogue (See section 8)	:results (See section 22)
# :eval (See section 9)	:rownames (See section 23)
# :exports (See section 10)	:sep (See section 24)
# :file (See section 11)	:session (See section 25)
# :file-desc (See section 12)	:shebang (See section 26)
# :hlines (See section 13)	:tangle (See section 27)
# :mkdirp (See section 14)	:tangle-mode (See section 28)
# :no-expand (See section 15)	:var (See section 29)
# :noweb (See section 16)	:wrap (See section 30)

    resExports* {.Attr.}: CodeResExports
    resCollection* {.Attr.}: CodeResCollection
    # TODO this field should be a `case` to support different `colnames`
    # properties, but only for relevant block types.
    resType* {.Attr.}: CodeResType
    resFormat* {.Attr.}: CodeResFormat
    resHandling* {.Attr.}: CodeResHandling

    langName* {.Attr.}: string
    code*: string ## Source code body - possibly untangled from `noweb`
    ## block

    execResult*: Option[CodeResult] ## Result of code block execution might
    ## be filled from parsed source code or generated using code block
    ## evaluation stage. In latter case it is possible to determine
    ## differences between results and report them if necessary.
