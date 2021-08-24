import
  hmisc/preludes/unittest,
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/types/colorstring,
  hmisc/other/[blockfmt]

import
  haxorg/parse/parse_org_structure,
  haxorg/defs/impl_org_node


template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template l(str: string): untyped =
  lexAll(varStr(str), lexStructure)


suite "Lex subtree":
  test "Simple subtree":
    let tokens = l("""
#+TITLE: Электротехника
#+INCLUDE: ../../orgheader.org

#+begin_export latex
\newcommand{\iPr}[1]{i_{#1 \text{пр.}}}
\newcommand{\iSv}[1]{i_{#1 \text{св.}}}

\newcommand{\uPr}[1]{U_{#1 \text{пр.}}}
\newcommand{\uSv}[1]{U_{#1 \text{св.}}}
#+end_export

* Теоретические вопросы

- Условие согласования нагрузки с генератором в цепи постоянного тока

** TODO [#A] Трехфазные цепи [0/10] :tags:

- Трехфазная цеть. Соединение звезда-звезда при симметричной и
  несимметричной нагрузке. Векторные диаграммы. Расчет смещения
  нейтрали.
""")


    let blc = ppblock(
      tokens,
      pconf(
        ignorePaths = matchField("baseStr", "extra", "isSlice"),
        forceLayouts = @{ matchType("OrgStructure"): forceLine() },
        extraFields = @[
          pprintExtraField(
            OrgStructureToken,
            "strVal",
            newPPrintConst(
              "\"" & it.strVal() & "\"",
              fgYellow + bgDefault))]))

    writeFile("/tmp/zz", blc.pyCodegenRepr(indent = 2, nimpref = "make"))
    echo toString(blc)
