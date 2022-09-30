import
  ../defs/[org_types, impl_org_node]

import hmisc/core/gold

import
  hmisc/algo/[hparse_base, hlex_base]

proc parseCommandArgs*(str: PosStr, parseConf: ParseConf): OrgNode =
  var
    str = str
    flags = str.newTree(orgInlineStmtList)
    args = str.newTree(orgInlineStmtList)
    inCmdline = false

  while ?str:
    case str[]:
      of ':':
        args.add newTree(
          orgCmdKey,
          str.asSlice((
            inWhile(str[{':', '+', '-', '^', '@'}], str.next());
            str.skipWhile(IdentChars))))

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

      of IdentChars:
        str.startSlice()
        let val = newTree(
          orgCmdValue,
          str.asSlice str.skipWhile(IdentChars))

        if str['=']:
          str.skip('=')
          case str[]:
            of '(':
              str.skipBalancedSlice({'('}, {')'})

            else:
              str.skipTo({' '})

        args.add newTree(orgCmdValue, str.popSlice())

      else:
        args.add newTree(orgCmdValue, str.asSlice str.skipTo(' '))


  result = newTree(orgCmdArguments, @[flags, args])
