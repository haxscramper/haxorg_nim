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

  if true:
    let tree = parseOrg("""
#[ inline comment ]#
Inline #[comment]# in text
#tag##[subtag##sub2##[sub3],sub4,subg5##sub6]
@meta{tag}
@meta2{tag}
@meta3[arg]{tag}
""")
