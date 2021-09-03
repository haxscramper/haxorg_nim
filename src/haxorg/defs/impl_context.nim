import hmisc/core/all

import
  ./org_types,
  ./impl_sem_org

proc getTarget*(ctx: SemOrgCtx, link: SemOrg): Option[OrgAnchor] =
  assertKind link, {orgLink}
  echov link.treeREpr()
