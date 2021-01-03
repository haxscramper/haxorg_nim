import std/[unittest, strutils, sequtils, macros]
import haxorg, haxorg/[lexer, parser, ast, buf, common]
import hmisc/hdebug_misc
import fusion/matching


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

func s(node: OrgNode): string = node.strVal()

proc parseOrg2(str: string): OrgNode = parseOrg(str)[0][0]
proc parseOrg1(str: string): OrgNode = parseOrg(str)[0]

suite "Example document parser":
  test "Simple markup elements":
    for markup in "/_+*":
      let node = parseOrg(markup & "hello" & markup)
      # echo node.treeRepr()
      node[0][0].assertMatch:
        Markup(strVal: == $markup, [Word(strVal: "hello")])

    parseOrg("*/bold/*")[0][0].assertMatch:
      Markup(strVal: "*"):
        Markup(strVal: "/"):
          Word(strVal: "bold")

  test "Verbatim elements":
    Markup(strVal: "~", [RawText(strVal: "HELLO")]) := parseOrg("~HELLO~")[0][0]

  test "Big idents":
    BigIdent(strVal: "MUST NOT") := parseOrg("MUST NOT")[0][0]
    BigIdent(strVal: "OPTIONAL") := parseOrg("OPTIONAL")[0][0]

  test "Bracket tags":
    let node = parseOrg("[!!!|>>>] User MUST NOT trigger bugs")
    node[0].assertMatch:
      Paragraph:
        BracTag:
          BareIdent(strVal: "!!!")
          BareIdent(strVal: ">>>")
        Word(strVal: " ")
        Word(strVal: "User")
        Word(strVal: " ")
        BigIdent(strVal: "MUST NOT")
        Word(strVal: " ")
        Word(strVal: "trigger")
        Word(strVal: " ")
        Word(strVal: "bugs")

  test "Links":
    RadioTarget[RawText(s: "radio target")] := parseOrg2("<<radio target>>")
    parseOrg2("[[LINK][description]]").assertMatch:
      Link:
        RawText(s: "LINK")
        Paragraph[Word(s: "description")]

    parseOrg2("[[LINK]]").assertMatch:
      Link[RawText(s: "LINK"), EmptyNode()]

    parseOrg2("[[BROKEN]").assertMatch:
      Word(s: "[[BROKEN]")

    let node = parseOrg2("[[BROKEN]")

    echo node.treeRepr()

  test "Inline source code elements":
    parseOrg2("src_sh[:eval false]{ls -l} {{{\"hello\"}}}").assertMatch:
      SrcCode:
        Ident(s: "sh")
        RawText(s: ":eval false")
        RawText(s: "ls -l")
        Result:
          Macro:
            RawText(s: "\"hello\"")

    let node = parseOrg2("src_sh[:eval false]{ls -l} {{{\"hello\"}}}")

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
      let tree = parseOrg("src_sh[:eval false]{ls -l} {{{\"hello\"}}}")

    if false:
      let tree = parseOrg("""
#+begin_src ipython :results output verbatim :noweb yes
print("| Ток | МКТ | МУП | Кирхгоф |")
# <<hello>>
# <<world(arg=12)>>
""")

    if false:
      let tree = parseOrg("""
#+begin_src ipython :snippet yes
$1
${1:$(make-string (string-width yas-text) ?\=)}
#+end_src
""")



    if false:
      let tree = parseOrg("""
- [@cnt] [X] tag ~:::~ world
  Heeader :: :: Body [1/2]
  + zz
- @arg{input} :: Input argument
""")

# Regulartext
# - aaaa :: bbb
# - aaaa :: bbb


    if false:
      let tree = parseOrg(
        "call_hello[-r -n :eval false :var a=2](val=12)[:post args]")

    if false: discard parseOrg("* __un__**co``n``str**a")

    if false: discard parseOrg("*/bold*")
    # if false: discard
    if false: discard parseOrg("~*/bold MUST NOT/*~")
    if false: discard parseOrg("*/bold MUST NOT/*")
    if false: discard parseOrg("user MUST NOT")
    if false: discard parseOrg("~~*/bold, but in verbatim/*~~")
    if false: discard parseOrg(r"\alpha{}hello")
    if false: discard parseOrg("~[[LINK][DESCRIPTION]]~")
    if false: discard parseOrg("[[Link]Broken~")
    startHax()
    if true:
      echo treeRepr(parseOrg("""
* Методы расчета переходных процессов
  :PROPERTIES:
  :header-args:ipython: :session transient :results output :exports results
  :created: <today>
  :END:
"""))

    if false: discard parseOrg("Joe said \"Hello /world/\".")
    if false: discard parseOrg("Most (optional) arguments")
    if false: discard parseOrg("[fn:NAME: a definition with *bold*]")
    if false: discard parseOrg("""
[FEATURE] changed something

- ADDED ::
  - <++> ~<++>~
  - <<radio link>> ~<<verbatim>>~
  - <link to documentation> ~<verbatim>~
""")
    if false:
      discard parseOrg("""
IF zzz THEN
  do something
ENDIF
EXIT
""")
    # if true: discard parseOrg("__//un zz__**con``N``sss**")


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
