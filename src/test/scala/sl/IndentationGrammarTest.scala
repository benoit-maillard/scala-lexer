package sl

import org.scalatest._
import scala.language.implicitConversions
import Expressions._

class IndentationGrammarTest extends OutputComparisonSpec with Lexers {
  val inputExtension: String = ".txt"
  val outputExtension: String = ".txt"

  type Token = T
  type Value = Seq[Int]

  implicit def compile[A](e: Expr[A]): CompiledExpr[A] = new CompiledExpr(e)
  implicit def toUnit(p: String): Expr[String] = Expr.unit(p)

  trait T
  case class Keyword(value: String) extends T
  case object Newline extends T
  case object Indent extends T
  case object Dedent extends T

  // counts the number of spaces
  val indentExpr: Expr[Int] = Expr.unit("""[ ]*""").map(s => s.length)
  val re1 = compile(Expr.unit("""Token"""))
  val re2 = compile(Expr.unit("\n") ~ indentExpr)

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
          else throw LexerError("Wrong indentation level", pos)
      }
    }
  )
  
  val lexer = Lexer(LexerState(indentRules, Seq(0)))
  val result = lexer.tokenizeFromFile("examples/example.txt")

  val pipeline = path => lexer.tokenizeFromFile(path)
    .get.map{case Positioned(token, pos) => f"$token(${pos.line},${pos.column})"}
    .reduce(_ ++ _)

  "lexer" should "tokenize basic file correctly" in {
    outputMatch("indent-grammar-1")
  }

  "lexer" should "fail if input contains invalid tokens" in {
    assertThrows[LexerError](output("indent-grammar-invalid"))
  }

  "lexer" should "fail if input contains inconsistent indentation" in {
    assertThrows[LexerError](output("indent-grammar-inconsistent"))
  }
}