import hmisc/core/all

import ./org_types

proc getTarget*(ctx: SemOrgCtx, link: SemOrg): Option[SemOrg] =
  assertKind link, {orgLink}
  discard
