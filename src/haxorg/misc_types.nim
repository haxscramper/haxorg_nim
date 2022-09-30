import hmisc/other/oswrap

import std/[rationals, options]

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

  OrgFileCategory* = enum
    ofcNone

    ofcBitmapImage
    ofcText

  OrgFile* = object
    ## org-mode file object
    # FIXME this is a placeholder implementation, not supporting full
    # capabilities of org-mode file path formatting
    case isRelative*: bool
      of true:
        relFile*: RelFile
        relTo*: AbsFile

      of false:
        absFile*: AbsFile

    category*: OrgFileCategory

  OrgDir* = object
    ## org-mode directory object
    # FIXME this is a placeholder implementation, not supporting full
    # capabilities of org-mode directory path formatting
    dir*: FsDir

  OrgDimensionsKind* = enum
    odkNone
    odkFraction

    odkCm
    odkMm
    odkTwip
    odkEm
    odkEx
    odkInch
    odkPx

  OrgDimensions* = object
    case kind*: OrgDimensionsKind
      of odkFraction:
        fraction*: Rational[int]

      else:
        value*: float

  OrgSearchTextKind* = enum
    ostkPlaintext ## Search arbitrary plaintext in target file
    ostkHeadingTitle ## Search subtree with specific title
    ostkHeadingId ## Search subtree with specific id
    ostkRegex ## Perform regex search in target file

  OrgInFileSearch* = object
    ## Parameters for in-file search
    lineNum*: Option[int]
    searchText*: Option[string]
    searchTextKind*: OrgSearchTextKind

func orgPt*(val: int | float): OrgDimensions =
  OrgDimensions(kind: odkPx, value: float(val))
