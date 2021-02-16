# import fusion/htmlparser, fusion/htmlparser/xmltree
import hmisc/other/hshell
import std/[htmlparser]
import hasts/html_ast
import hmisc/helpers


proc pygmentizeToHTML*(code, lang: string,): XmlNode =
  let cmd = makeShellCmd("pygmentize", "--", " ").withIt do:
    it - ("l", lang)
    it - ("f", "html")
      # , -l, lang, -f, html)

  echo cmd

  let res = runShell(cmd, stdin = code)
  return parseHtml(res.stdout)[0]
