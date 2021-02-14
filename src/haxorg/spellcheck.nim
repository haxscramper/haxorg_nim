import nimspell/[hunspell, annotations]
import hmisc/types/[langcodes, colorstring]
import common, ast, buf
import hmisc/hexceptions

type OrgAnnot = AnnotationGroup[OrgNode]

func toWords*(node: OrgNode): seq[TextWord[OrgAnnot]] =

  case node.kind:
    of onkWord, onkIdent, onkBigIdent:
      result.add initTextWord[OrgAnnot]($node.text, OrgAnnot(data: node))

    of orgSubnodeKinds:
      for subnode in node:
        result.add toWords(subnode)

    else:
      discard


proc checkSpelling*(node: OrgNode, str: string) =
  echo treeRepr(node)
  var words = toWords(node)
  let hunsp = initHunSpell(lcEnglish, ccUnitedStatesOfAmerica)
  markTypos(words, hunsp)
  echo words.highlightSuggestions()

  var errors: seq[ErrorAnnotation]

  for word in words:
    let annot = word.attr.annot
    if annot.kind != wakNone:
      var err = ErrorAnnotation(
        fromString: true, offset: word.attr.data.text.startPos())

      case annot.kind:
        of wakSpelling:
          err.annotation = $annot.replacements

        else:
          discard

      errors.add err

  var resError = CodeError(
    fromString: true, annots: errors,
    substr: GlobalSubstring(str: str)
  )

  echo toColorString(resError)
