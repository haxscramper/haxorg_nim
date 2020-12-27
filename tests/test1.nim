import unittest
import haxorg

suite "Example document parser":
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
    let tree = parseOrg("Regular *text*")

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


  if true:
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

      Longest column format (whole six characters to declare new column,
      yes? I bet eveyone will think I need to come up with more 'concise'
      and 'simple' syntax), but the most flexible one. You can put anything
      in the column, including subtables, code blocks and more.

    #+cell: :name named-cell

      Another advantage - naming and custom arguments are possible too.
      Putting cell sizes (multi-col/row), naming, specialized formatting
      commands and much more.

#+end-table
""")

import std/strutils
