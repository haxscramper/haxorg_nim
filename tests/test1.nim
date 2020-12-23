import unittest
import haxorg

suite "Example document parser":
  let str = """
#+begin-code: nim

#+end-code

#+TITLE: @date:2020-12-23; @time:11:24;

* TODO Tasks [6/8]
  DEADLINE: <2020-12-23 Wed 23:55>
** COMPLETED Startx
   CLOSED: [2020-12-23 Wed 11:24]
   :LOGBOOK:
   - State "COMPLETED"  from "TODO"       [2020-12-23 Wed 11:24]
   :END:
"""

  let tree = parseOrg(str)
