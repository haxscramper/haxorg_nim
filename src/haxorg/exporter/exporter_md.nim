import ast, buf, exporter, semorg
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import fusion/matching
import std/[options, strformat, strutils]
import common

import exporter_markup


using
  exp: OrgMarkupExporter
  tree: SemOrg
  conf: RunConfig

proc exportSrcCode*(exp, tree, conf): string =
  &"""
```{tree.codeBlock.langName}
{tree.codeBlock.code.strip()}
```
"""

proc newOrgMarkdownExporter*(): OrgMarkupExporter =
  result = OrgMarkupExporter(
    name: "md",
    fileExt: "md",
    description: "Export to markdown file",
    impl: newOrgMarkupExporter().impl
  )

  result.impl[onkSubtree] = proc(exp, tree, conf): string =
    result = "#".repeat(tree.subtLevel) & " "
    result &= exportUsing(exp, tree["title"], conf) & "\n"

    result &= exportAllUsing(exp, tree["body"], conf)

  result.impl[onkSrcCode] = exportSrcCode



register(newOrgMarkdownExporter())
