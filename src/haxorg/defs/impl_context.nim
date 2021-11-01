import hmisc/core/all
import std/[tables]

import
  ./org_types,
  ./impl_sem_org

func `[]`*(ctx: SemOrgCtx, symKind: SymKind): Table[string, SemOrg] =
  ctx.symTable[symKind]

func `[]`*(ctx: SemOrgCtx, symKind: SymKind, name: string): SemOrg =
  ctx.symTable[symKind][name]

func `[]=`*(
    ctx: var SemOrgCtx, symKind: SymKind, name: string, node: SemOrg) =
  # Current implementation intentionally does simple override - document is
  # processed top-down, which means 'closest' node is resolved without the
  # need for any extra heuristics.
  ctx.symTable[symKind][name] = node

proc getTarget*(ctx: SemOrgCtx, link: SemOrg): Option[OrgAnchor] =
  assertKind link, {orgLink}
  let name = link.link.targetName
  case link.link.kind:
    of olkCallout:
      if name in ctx[symCalloutTarget]:
        result = some initAnchor(ctx[symCalloutTarget, name])

      else:
        discard

    of olkSubtree:
      if name in ctx[symSubtree]:
        result = some initAnchor(ctx[symSubtree, name])

      elif false #[ TODO search for subtrees with prefixes ]#:
        discard

      else:
        discard

    of olkImplicit:
      if name in ctx[symRegularTarget]:
        result = some initAnchor(ctx[symRegularTarget, name])

      elif name in ctx[symNamed]:
        result = some initAnchor(ctx[symNamed, name])

      else:
        discard

    of olkFile:
      discard

    else:
      raise newImplementKindError(link.link)
