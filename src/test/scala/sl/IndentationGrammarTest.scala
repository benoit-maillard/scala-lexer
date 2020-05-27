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
  val indentExpr: Expr[Int] = unit("""[ ]*""").map(s => s.length)
  val re1: Expr[String] = "Token"
  val re2: Expr[String ~ Int] = "\n" ~/~ indentExpr

  lazy val indentRules: RuleSet = RuleSet(
    re1 |~> {
      case (indents, str, pos) => (randomRules(indents), List(Positioned(Keyword(str), pos)))
    },
  )

  lazy val randomRules: RuleSet = RuleSet(
    re2 |~> {
      case (indents, _ ~ spaces, pos) => indents match {
        case current +: tl =>
          if (spaces == current) (indentRules(indents), List(Positioned(Newline, pos)))
          else if (spaces > current)
            (LexerState(indentRules, spaces +: indents), List(Positioned(Newline, pos), Positioned(Indent, pos)))
          else if (spaces == tl.head) (LexerState(indentRules, tl), List(Positioned(Newline, pos), Positioned(Dedent, pos)))
          else (LexerState(indentRules, tl), List(Positioned(Error, pos)))
      }
    }
  )
  
  val lexer = Lexer(LexerState(indentRules, Seq(0)), Error)

  val pipeline = path => lexer.tokenizeFromFile(path)
    .get.map{case Positioned(token, pos) => f"$token(${pos.line},${pos.column})"}
    .mkString("\n")

  "indentation-based lexer" should "tokenize basic file correctly" in {
    outputMatch("indent-grammar-1")
  }

  it should "tokenize an empty file correctly" in {
    outputMatch("empty")
  }

  it should "fail if input contains invalid tokens" in {
    outputContains("indent-grammar-invalid", "Error")
  }

  it should "fail if input contains inconsistent indentation" in {
    outputContains("indent-grammar-inconsistent", "Error")
  }
}