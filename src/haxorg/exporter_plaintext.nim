import exporter, semorg, ast, buf
import std/[uri]
import hmisc/other/[hjson, hshell, oswrap]
import hmisc/helpers

type
  OrgPlaintextExporter* = ref object of OrgExporter


register(OrgPlaintextExporter(
  name: "plaintext-base",
  fileExt: "txt",
  description: "Base plaintext exporter"
))


using
  exp: OrgPlaintextExporter
  conf: RunConfig
  tree: SemOrg

method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  proc aux(tree): string =
    case tree.kind:
      of orgTokenKinds:
        result = tern(tree.isGenerated, "", $tree.node.text)

      of orgSubnodeKinds:
        for node in tree:
          result &= aux(node) & " "

      else:
        discard

  target = aux(tree)
