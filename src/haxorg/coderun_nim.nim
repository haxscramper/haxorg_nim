import ast, semorg, buf
import hmisc/hdebug_misc

type
  NimCodeBlock = ref object of CodeBlock


proc newNimCodeBlock(): NimCodeBlock =
  NimCodeBlock()


method parseFrom*(codeBlock: var NimCodeBlock, semorg: var SemOrg) =
  codeBlock.code = $semorg.node["body"].text

method runCode*(codeBlock: var NimCodeBlock, context: var CodeRunContext) =
  discard


register("nim", newNimCodeBlock)
