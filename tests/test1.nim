import unittest
import haxorg

suite "Example document parser":
  let str = """
#+begin-code: nim
123
  5678
#+end-code

#+TITLE: @date:2020-12-23; @time:11:24;

* TODO    [#A] *Tasks* `for` ~today~ /nice/ _under_ +score+
"""

  let tree = parseOrg(str)
