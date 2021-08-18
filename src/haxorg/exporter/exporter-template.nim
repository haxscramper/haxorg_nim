# Template file for new exportters, replace placeholder with exporter name

import ast, buf, exporter, semorg
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import fusion/matching
import std/[options, strformat]


#===========================  Type defintions  ===========================#

type
  Org<++>Exporter = ref object of OrgExporter


#=============================  Boilerplate  =============================#

using
  exp: Org<++>Exporter
  tree: SemOrg
  conf: RunConfig

#============================  Constructors  =============================#

proc newOrg<++>Exporter*(): Org<++>Exporter =
  result = Org<++>Exporter(
    name: "",
    fileExt: "",
    description: "",
  )


#=========================  Register exporters  ==========================#

register(newOrg<++>Exporter())


method exportTo*(exp, tree; target: var string; conf = defaultRunConfig) =
  discard

method exportTo*(
  exp, tree; target: AbsFile; conf: RunConfig = defaultRunConfig) =

  discard
