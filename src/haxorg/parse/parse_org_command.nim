import
  ../defs/org_types

import
  hmisc/algo/[hparse_base, hlex_base]

proc lexCommand(str: var PosStr): seq[OrgCommandToken] =
  if not ?str:
    result.add str.initEof(octEof)

  else:
    case str[]:
      else:
        raise newUnexpectedCharError(str)

proc parseCommandArg*(str: PosStr, parseConf: ParseConf): OrgNode =
  var str = str
  var lexer = initLexer(str, lexCommand, true)
