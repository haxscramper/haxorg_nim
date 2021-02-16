# import fusion/htmlparser, fusion/htmlparser/xmltree
import hmisc/other/hshell
import std/[htmlparser, strformat]
import hasts/html_ast
import hmisc/helpers


const baseCmd = makeShellCmd("pygmentize", "--", " ")

type
  CmdKwargs* = openarray[(string, string)]
  CmdArgs* = openarray[string]

proc pygmentizeTo*(
    code, lang, formatter: string,
    args: CmdArgs,
    kwargs: CmdKwargs
  ): string =

  var cmd = baseCmd

  cmd - ("f", formatter)
  cmd - ("l", lang)

  for arg in args:
    cmd - ("a", arg)

  for (key, val) in kwargs:
    cmd - ("P", &"{key}={val}")

  result = runShell(cmd, stdin = code).stdout

proc pygmentizeGetStyle*(
    formatter, name: string,
    args: CmdArgs, kwargs: CmdKwargs
  ): ShellCmd =

  result = baseCmd

  result - ("f", formatter)
  result - ("S", name)

  for arg in args:
    result - ("a", arg)

  for (key, val) in kwargs:
    result - ("P", &"{key}={val}")


proc pygmentizeToHTML*(
    code, lang: string,
    args: CmdArgs = @[],
    kwargs: CmdKwargs = { "classprefix": "src-" }
  ): XmlNode =

  pygmentizeTo(code, lang, "html", args, kwargs).parseHtml()[0]

proc pygmentizeGetHtmlStyle*(
    name: string = "default",
    args: CmdArgs = @[".highlight "],
    kwargs: CmdKwargs = { "classprefix": "src-" }
  ): string =

  result = eval(pygmentizeGetStyle("html", name, args, kwargs))


proc pygmentizeToTex*(
    code, lang: string,
    args: CmdArgs = @[],
    kwargs: CmdKwargs = @[]
  ): string =

  pygmentizeTo(code, lang, "tex", args, kwargs)

proc pygmentizeGetTexStyle*(
    name: string = "default",
    args: CmdArgs = @[],
    kwargs: CmdKwargs = @[]
  ): string =

  result = eval(pygmentizeGetStyle("tex", name, args, kwargs))
