import
  hmisc/other/[oswrap, cliparse, hargparse],
  hmisc/core/all

import
  ./org_types,
  ./impl_org_node

export hargparse

func initCliCommand*(cmd: string): CliOpt =
  CliOpt(kind: coCommand, valStr: cmd, rawStr: cmd)

func initCliFlag*(cmd: string): CliOpt =
  CliOpt(kind: coFlag, keyPath: @[cmd], rawStr: cmd)

func initCliArgument*(cmd: string): CliOpt =
  CliOpt(kind: coArgument, valStr: cmd, rawStr: cmd)

proc parseArgs*(app: var CliApp, args: seq[OrgNode]): bool =
  var opts: seq[CliOpt]
  opts.add initCliCommand(app.name)
  for idx, arg in args:
    case arg.kind:
      of orgCmdKey:
        opts.add initCliFlag(arg.strVal())

      of orgCmdValue:
        opts.add initCliArgument(arg.strVal())

      else:
        raise newUnexpectedKindError(arg)

  let tree: CliCmdTree = opts.structureSplit(app.root, app.errors)
  if app.errors.len > 0:
    raise app.errors[0].asRef()

  app.value = tree.toCliValue(app.errors)

  if app.errors.len > 0:
    raise app.errors[0].asRef()

  app.finalizeDefaults()

  return app.errors.len == 0
