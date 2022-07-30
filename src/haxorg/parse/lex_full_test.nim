import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/other/[hpprint, blockfmt],
  lex_all

template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexGlobal())

for tok in l("""
- list with text

  #+begin_src cpp
c++ code
  #+end_src
"""):
  echo tok
