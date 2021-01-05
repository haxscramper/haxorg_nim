{.experimental: "caseStmtMacros".}

import ast, buf
import std/[options, tables, strutils, strformat]
import hpprint, hpprint/hpprint_repr
import hmisc/other/hshell
import hmisc/hdebug_misc

import fusion/matching



type
  RunConfig* = object
    tempDir*: string


  OrgCompletion* = object
    ## Completion status cookie
    case isPercent*: bool
      of true:
        percent*: float

      of false:
        done*: int
        total*: int


  TreeScope* = object
    ## Subtree scope. Mostly used for internal implementation in sempass
    tree*: SemOrg

  LinkTarget* = object
    case isExternal*: bool
      of true:
        targetStr*: string

      of false:
        targetEntry*: SemOrg


  CodeResult* = object
    # - TODO :: determine if (and how) results of multistage execution
    #  should be represented (compilation (potentially complex one) +
    #  execution)
    ## Result oc code block compilation and execution.
    execResult*: ShellResult

  SemOrg* = ref object
    ## Rewrite of the parse tree with additional semantic information
    assocList*: Option[SemOrg] ## Reference to associative list

    case isGenerated*: bool ## Can be `true` for sem nodes generated in
      ## subsequent stages (mostly code execution, but include directive
      ## resolution as well as several others can also produce new blocks)
      of false:
        slice*: Option[StrSlice]
        node* {.requiresinit.}: OrgNode ## Original org-mode parse tree node.

      of true:
        discard

    subnodes*: seq[SemOrg]

    case kind*: OrgNodeKind
      of onkSubtree:
        subtLevel*: int
        subtProperties*: Table[string, string]
        subtTitle*: OrgNode
        subtCompletion*: Option[OrgCompletion]
        subtTags*: seq[string]

      of onkSrcCode:
        flags*: seq[string]
        code*: string ## Source code body - possibly untangled from `noweb` block
        execResult*: Option[CodeResult] ## Result of code block execution
        ## might be filled from parsed source code or generated using code
        ## block evaluation stage. In latter case it is possible to
        ## determine differences between results and report them if
        ## necessary.

      of onkAssocStmtList:
        attrs*: seq[tuple[name: string, body: SemOrg]]

      of onkMarkup:
        mark*: string
        content*: seq[SemOrg]

      of onkLink:
        linkTarget*: LinkTarget ## Optional reference to target node within document
        linkDescription*: SemOrg

      else:
        discard

proc add*(tree: var SemOrg, subtree: SemOrg) =
  assert tree.kind in orgSubnodeKinds
  tree.subnodes.add subtree

proc newSemOrg(node: OrgNode): SemOrg =
  SemOrg(kind: node.kind, isGenerated: false, node: node)

proc toSemOrg*(
  node: OrgNode, config: RunConfig, scope: seq[TreeScope]): SemOrg =
  case node:
    of SrcCode({ "lang" : @lang, "body" : @body }):
      echov "Found semorg tree"
      result = newSemOrg(node)

    else:
      result = newSemOrg(node)
      if result.kind in orgSubnodeKinds:
        for subnode in node.subnodes:
          result.add toSemOrg(subnode, config, @[])
