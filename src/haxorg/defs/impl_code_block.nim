import ./org_types

import std/[tables, options]

var defaultRunConf: RunConf

proc updateContext*(codeBlock: CodeBlock, context: var CodeRunContext) =
  if codeBlock.evalSession.isSome():
    context.prevBlocks.mgetOrPut(
      codeBlock.evalSession.get(), newSeq[CodeBlock]()).add codeBlock

proc getSameSession*(
    codeBlock: CodeBlock, context: CodeRunContext): seq[CodeBlock] =

  if codeBlock.evalSession.isSome():
    context.prevBlocks.getOrDefault(
      codeBlock.evalSession.get(), @[codeBlock])

  else:
    @[codeBlock]

proc runCodeBlocks*(tree: var SemOrg, config: RunConf = defaultRunConf) =
  ## Execute all code blocks in `SemOrg`.
  proc aux(tree: var SemOrg, context: var CodeRunContext) =
    case tree.kind:
      of orgTokenKinds:
        discard

      of orgSubnodeKinds:
        if tree.kind == orgSrcCode:
          runCode(tree.codeBlock, context)

        else:
          for subnode in mitems(tree.subnodes):
            aux(subnode, context)

      else:
        # Noweb and snippet nodes should have been already untangled and
        # replaced with regular semorg entries.
        raiseAssert("#[ IMPLEMENT ]#")

  var context: CodeRunContext
  aux(tree, context)


proc register*(lang: string, codeBuilder: CodeBuilder) =
  defaultRunConf.codeCreateCallbacks[lang] = codeBuilder