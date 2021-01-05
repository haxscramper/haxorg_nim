import ast, semorg
import hmisc/hdebug_misc

type
  NimCodeBlock = ref object of CodeBlock


proc newNimCodeBlock(): NimCodeBlock =
  NimCodeBlock()


method parseFrom*(codeBlock: var NimCodeBlock, semorg: var SemOrg) =
  echov "Parse semorg node"

method runCode*(codeBlock: var NimCodeBlock, context: var CodeRunContext) =
  echov "Run nim code"


register("nim", newNimCodeBlock)
