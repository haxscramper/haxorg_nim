import
  ./org_types

import std/[strutils, lenientops]

import
  hmisc/other/oswrap,
  hmisc/core/all

proc getAbs*(file: OrgFile): AbsFile =
  if file.isRelative:
    file.relTo.dir() / file.relFile

  else:
    file.absFile

proc parseOrgDimensions*(dim: string): OrgDimensions =
  var dimend = dim.high
  var dimstart = dim.high
  while dim[dimstart] in {'a' .. 'z'}:
    dec dimstart

  inc dimstart

  result.kind =
    case dim[dimstart .. dimend].toLowerAscii():
      of "cm": odkCm
      of "mm": odkMm
      of "px": odkPx
      of "ex": odkEx
      else: raise newUnexpectedKindError(dim[dimstart .. dimend])

  result.value = parseFloat(dim[0 ..< dimstart])

proc pxToTwip*(px: float): float = px * 15

proc toUnits*(
    dim: OrgDimensions, 
    target: OrgDimensionsKind,
    dpi: Option[int] = none(int)
  ): float =

  if dim.kind == target:
    return dim.value

  else:
    case dim.kind:
      of odkPx:
        case target:
          of odkTwip: return pxToTwip(dim.value)
          of odkCm:
            assertOption dpi
            return dim.value * (2.54 / dpi.get)

          else: raise newImplementKindError(target)

      else:
        raise newImplementKindError(dim)


proc isInToplevel*(ctx: SemOrgCtx, node: OrgNode): bool =
  if ctx.kindStack.top() == node.kind:
    return ctx.kindStack.len == 2 or (
      ctx.kindStack.len > 2 and
      ctx.kindStack[^2] == orgStmtList and
      ctx.kindStack[^3] in {orgSubtree}
    )
