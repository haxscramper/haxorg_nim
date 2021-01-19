import ast, semorg, buf
import hmisc/hdebug_misc
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import std/strformat

type
  NimCodeBlock = ref object of CodeBlock


proc newNimCodeBlock(): NimCodeBlock =
  NimCodeBlock()


proc assembleFile*(blocks: seq[CodeBlock]): string =
  echov "Assembling file for blocks"
  for idx, cblock in pairs(blocks):
    result.add &"""

# {cblock.evalSession}

{cblock.code}

"""

method parseFrom*(codeBlock: NimCodeBlock, semorg: var SemOrg) =
  parseBaseBlock(CodeBlock(codeBlock), semorg)

method runCode*(codeBlock: NimCodeBlock, context: var CodeRunContext) =
  updateContext(codeBlock, context)

  withTempFile(AbsDir("/tmp"), "XXXXX.nim"):
    let text = codeBlock.getSameSession(context).assembleFile()

    echo text
    file.writeFile text

    let cmd = makeNimShellCmd("nim").withIt do:
      it.cmd "r"
      it.arg file


    execShell cmd



register("nim", newNimCodeBlock)
