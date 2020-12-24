import unittest
import haxorg

# #+begin-code: nim
# 123
#   5678
# #+end-code

# #+TITLE: @date:2020-12-23; @time:11:24;



suite "Example document parser":
  let str = """
* TODO    [#A] *Tasks* `for` ~today~ /nice/ _under_ +score+ \
  Long heading __un__**co``n``str**a~~ined~~
   :properties:
   :created:  <2020-12-24 Thu 10:01>
   :end:
   :logbook:
   - State "IN_PROGRESS" from "TODO"       [2020-12-24 Thu 11:20]
   :end:
"""

  let tree = parseOrg(str)
