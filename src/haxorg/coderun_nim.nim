import ast, semorg, buf
import hmisc/hdebug_misc
import hmisc/other/[oswrap, hshell]
import hmisc/helpers
import std/strformat

type
  NimCodeBlock = ref object of CodeBlock


proc newNimCodeBlock(): NimCodeBlock =
  NimCodeBlock(langName: "nim")


proc assembleFile*(blocks: seq[CodeBlock]): string =
  echov "Assembling file for blocks"
  for idx, cblock in pairs(blocks):
    result.add &"""

# {cblock.evalSession}

{cblock.code}

"""

method parseFrom*(
  codeBlock: NimCodeBlock, semorg: SemOrg, scope: seq[TreeScope]) =

  parseBaseBlock(CodeBlock(codeBlock), semorg, scope)

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
