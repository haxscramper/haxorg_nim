import
  ../defs/[org_types, impl_org_node]

import
  hmisc/algo/[hparse_base, hlex_base]

proc parseCommandArgs*(str: PosStr, parseConf: ParseConf): OrgNode =
  var
    str = str
    flags = newTree(orgInlineStmtList)
    args = newTree(orgInlineStmtList)
    inCmdline = false

  while ?str:
    case str[]:
      of ':':
        args.add newTree(
          orgCmdKey,
          str.asSlice str.skipWhile(IdentChars + {':'}))

        inCmdline = true

      of '-':
        if inCmdline:
          args.add newTree(
            orgCmdValue,
            str.asSlice str.skipWhile(AllChars - Whitespace))

        else:
          flags.add newTree(
            orgCmdFlag,
            str.asSlice str.skipWhile(IdentChars + {'-'}))

      of ' ':
        str.next()

      else:
        raise newUnexpectedCharError(str)

  result = newTree(orgCmdArguments, @[flags, args])
