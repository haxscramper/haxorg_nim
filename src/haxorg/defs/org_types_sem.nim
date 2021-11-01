import
  ./org_types_misc,
  ./org_types_enums

import
  std/[options, tables]

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
