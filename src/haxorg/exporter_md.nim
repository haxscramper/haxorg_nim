import ast, buf, exporter, semorg
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import fusion/matching
import std/[options, strformat]
import common

import exporter_markup


using
  exp: OrgMarkupExporter
  tree: SemOrg
  conf: RunConfig

proc newOrgMarkdownExporter*(): OrgMarkupExporter =
  result = OrgMarkupExporter(
    name: "md",
    fileExt: "md",
    description: "Export to markdown file",
    impl: newOrgMarkupExporter().impl
  )


register(newOrgMarkdownExporter())
