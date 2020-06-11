package sl

import Expressions._
import scala.language.implicitConversions

class SimplifiedPythonTest extends OutputComparisonSpec with Lexers {
  val inputExtension: String = ".py"
  val outputExtension: String = ".txt"

  type Token = T
  type Value = Int

  trait T
  case class KeywordToken(value: String) extends T
  case object Space extends T
  case object Error extends T

  val lexer = Lexer(
    oneOf("True", "False", "if", "else") |> {
      case (i, str, pos) => (0, List(Positioned(KeywordToken(str), pos)))
    },
    unit("""\W*""") |> {
      case (i, str, pos) => (0, List(Positioned(Space, pos)))
    }
  )(Error, 0)

  val pipeline = path => lexer.tokenizeFromFile(path)
    .get.map{case Positioned(token, pos) => f"$token(${pos.line},${pos.column})"}
    .reduce(_ ++ "\n" ++ _)

  "simplified python lexer" should "tokenize keywords correctly" in {
    outputMatch("simplified-python-1")
  }
}