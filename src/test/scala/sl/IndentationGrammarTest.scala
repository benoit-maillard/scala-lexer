package sl

import org.scalatest._
import scala.language.implicitConversions
import Expressions._

class IndentationGrammarTest extends OutputComparisonSpec with Lexers {
  val inputExtension: String = ".txt"
  val outputExtension: String = ".txt"

  type Token = T
  type Value = Seq[Int]

  trait T
  case class Keyword(value: String) extends T
  case object Newline extends T
  case object Indent extends T
  case object Dedent extends T
  case object Error extends T

  // counts the number of spaces
  val re1: Expr[String] = "Token"
  val re2: Expr[String ~ String] = "\n" ~/~ """[ ]*"""
  
  val lexer = Lexer(
    re1 |> {
      case (indents, List(str)) => (indents, List(Keyword(str.str)))
    },
    re2 |> {
      case (indents, List(_, spaces)) => indents match {
        case current +: tl =>
          if (spaces.str.length == current) (indents, List(Newline))
          else if (spaces.str.length > current)
            (spaces.str.length +: indents, List(Newline, Indent))
          else if (spaces.str.length == tl.head) (tl, List(Newline, Dedent))
          else (tl, List(Error))
      }
    }
  )(Error, Seq(0))

  val pipeline = path => lexer.tokenizeFromFile(path)
    .get.map(positioned => f"${positioned.value}(${positioned.start.line},${positioned.start.column})")
    .mkString("\n")

  "indentation-based lexer" should "tokenize basic file correctly" in {
    outputMatch("indent-grammar-1")
  }

  it should "tokenize an empty file correctly" in {
    outputMatch("empty")
  }

  it should "fail if input contains invalid tokens" in {
    assertThrows[LexerError](output("indent-grammar-invalid"))
  }

  it should "fail if input contains inconsistent indentation" in {
    outputContains("indent-grammar-inconsistent", "Error")
  }
}