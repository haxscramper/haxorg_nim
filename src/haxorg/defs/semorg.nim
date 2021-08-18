{.experimental: "caseStmtMacros".}

import ast, buf
import std/[options, tables, strutils, strformat, uri,
            hashes, enumerate, sequtils]

import hpprint, hpprint/hpprint_repr
import hmisc/other/hshell
import hmisc/other/oswrap
import hmisc/types/colorstring
import hmisc/algo/[hlex_base, hparse_base, htree_mapping, clformat]
import hmisc/core/all
import nimtraits



var defaultRunConf*: RunConf

proc subKind*(semorg: SemOrg): OrgNodeSubKind = semorg.node.subkind


