import ast, buf, exporter, semorg
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import fusion/matching
import std/[options, strformat]


#===========================  Type defintions  ===========================#

type
  OrgPurePdfExporter = ref object of OrgExporter


#=============================  Boilerplate  =============================#

using
  exp: OrgPurePdfExporter
  conf: RunConfig
  tree: SemOrg

#============================  Constructors  =============================#

proc newOrgPurePdfExporter*(): OrgPurePdfExporter =
  result = OrgPurePdfExporter(
    name: "pure-pdf",
    fileExt: "pdf",
    description: "Export document to pdf directly",
  )

  discard


#=========================  Register exporters  ==========================#

register(newOrgPurePdfExporter())

method exportTo*(
    exp: OrgPurePdfExporter,
    tree; target: AbsFile; conf: RunConfig = defaultRunConfig
  ) =

  echo "Exporting using pure pdf"
