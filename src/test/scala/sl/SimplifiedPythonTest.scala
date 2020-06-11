package sl

import Expressions._
import scala.language.implicitConversions
import sl.Positions.WithPosition

class SimplifiedPythonTest extends OutputComparisonSpec with Lexers {
  val inputExtension: String = ".py"
  val outputExtension: String = ".txt"
  trait T
  type Token = T
  type Value = Int

  case class KeywordToken(value: String) extends T
  case object Space extends T
  case object Error extends T

  val lexer = Lexer(
    oneOf("True", "False", "if", "else") |> {
      case (i, List(str)) => (0, List(KeywordToken(str.str)))
    },
    unit("""\W*""") |> {
      case (i, List(str)) => (0, List(Space))
    }
  )(Error, 0)

  val pipeline = path => lexer.tokenizeFromFile(path)
    .get.map(token => f"${token.value}(${token.start.line},${token.start.column})")
    .reduce(_ ++ "\n" ++ _)

  "simplified python lexer" should "tokenize keywords correctly" in {
    outputMatch("simplified-python-1")
  }
}