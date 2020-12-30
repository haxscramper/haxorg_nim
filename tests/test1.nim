import std/[unittest, strutils, sequtils]
import haxorg, haxorg/[lexer, parser, ast, buf, common]
import hmisc/hdebug_misc


suite "Sublexer":
  proc lex(str: string): Lexer = newLexer(newStrBufSlice(str))

  test "New lexer advance":
    var lexer = lex "12345"
    check lexer[] == '1'
    check lexer.pop() == 0
    check lexer[] == '2'

  test "Simple sublexer range":
    startHax()
    var lexer = lex "[123][456]"
    #                0123456789

    var sub1 = lexer.newSublexer(@[(0, 3)])
    check sub1[] == '['


    var sub2 = lexer.newSublexer(@[(6, 8)])
    check sub2[] == '4'
    sub2.advance()
    check sub2[] == '5'
    check sub2.pop() == 7

    var sub3 = lexer.newSublexer(@[(0, 1), (3, 4)])
    check toSeq(sub3) == "[13]"

  test "Cut indented part":
    let str = """
  ABC
  DEF
GHK
"""

    var lexer = lex str
    var sub = lexer.indentedSublexer(
      2,
      requireContinuation = false,
      fromInline = false,
      keepNewlines = false
    )

    check toSeq(sub) == "ABCDEF"

  test "Block until":
    let str = """
  #+row:

    123
#+end
"""
    echo toSeq(pairs(str)).join("\n")


    var lexer = lex str

    var sub = lexer.blockSublexer("#+end")
    check toSeq(sub) == toSeq("#+row:\n\n  123\n")

suite "Example document parser":
  test "shit":
    if false:
      let tree = parseOrg """
#+TITLE: @date:2020-12-23; @time:11:24;

* TODO    [#A] Long heading **un``con``str**ained \
  123
   :properties:
   :created:  <2020-12-24 Thu 10:01>
   :end:

Regular *text*

#+begin-code: nim
123
  5678
#+end-code
"""


    if false:
      let tree = parseOrg("* __un__**co``n``str**a")

    if false:
      let tree = parseOrg("__//un zz__**con``N``sss**")

    if true:
      let tree = parseOrg("src_sh[:eval false]{ls -l} {{{\"hello\"}}}")

    if false:
      let tree = parseOrg("*/bold*")

    if false:
      let tree = parseOrg("* *bold*")

    if false:
      let tree = parseOrg(
        r"Если угол $\phi = \atan \frac{X}{R}$ _опережает_. для /индуктивного/")

    if false:
      let tree = parseOrg("""
#[ inline comment ]#
Inline #[comment]# in text
#tag##[subtag##sub2##[sub3],sub4,subg5##sub6]
@meta{tag}
@meta2{tag}
@meta3[arg]{tag}{{ arg2 }}
""")


    if false:
      let tree = parseOrg("""
#+begin-table
  #+row:
    # One step above regular table - you can put comments on row content
    | col1 | col2 | col3 |
  #+row: :color red # Putting comments on rows is possible now
    | column1 # This allows you to put comments
    | column2 # Imagine writing some non-trivial formula and then commenting
    | column3 # On parts for it
    | column4 # Though in this case comment probably is a part of cell body

    # Yes, you loose some readability wrt. to this thing being table-like, but
    # there are always tradeoffs.
  #+row:
    #+cell:

      Longest column format, but the most flexible one. You can put
      anything in the column, including subtables, code blocks and more.

    #+cell: :name named-cell

      Another advantage - naming and custom arguments are possible too.
      Putting cell sizes (multi-col/row), naming, specialized formatting
      commands and much more.

#+end-table
""")
