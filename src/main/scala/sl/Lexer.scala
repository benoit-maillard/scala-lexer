package sl
import scala.util.matching.Regex
import Expressions._
import Positions._
import scala.annotation.tailrec
import java.io.File
import scala.io.Source
import scala.language.implicitConversions

import Rules._

import scala.util.{Try, Success, Failure}

trait Lexers {
  type Token
  type Value
  val debug = false

  implicit def positionedToken(token: Token): Positioned[Token] = NoPosition(token)

  /**
    * Error in the lexing process
    *
    * @param message error message
    * @param pos position where the problem appeared
    */
  case class LexerError(message: String, pos: Position) extends Error(f"$message at (${pos.line}, ${pos.column})")

  /**
    * Lexer that can produce tokens given an input string.
    *
    * @param initialState set of rules and value that can be matched at the start of input string
    * @param error token that should be produced when no matching rule can be found
    */
  case class Lexer(rules: Rule[Value, Token]*)(initialValue: Value, finalAction: (Value, Position) => List[Positioned[Token]] = (_, _) => List()) {
    /**
      * Produces tokens using the successive rules and the given string.
      *
      * @param input string containing tokens
      * @return produced tokens if matching rules were found for the entire input, None otherwise
      */
    def tokenizeFromString(input: String): Try[List[WithPosition[Token]]] = {
      val start = InputState(Position.initial, new ArrayCharSequence(input.toArray))
      if (debug) {
        println(f"# Starting tokenization on [${input.take(30).replace("\n", "\\n")}]")
      }
      advance(start, initialValue, Nil).map(s => s.reverse)
    }

    /**
      * Produces tokens using the successive rules and the given input file.
      *
      * @param path file containing tokens
      * @return produced tokens if matching rules were found for the entire input, None otherwise
      */
    def tokenizeFromFile(path: String): Try[List[WithPosition[Token]]] = {
      val content = Source.fromFile(path).mkString
      tokenizeFromString(content)
    }
    
    // uses successive rules to make progress with input
    @tailrec
    private def advance(input: InputState, state: Value, acc: List[WithPosition[Token]]): Try[List[WithPosition[Token]]] = {
      if (input.chars.length == 0) {
        val finalTokens = finalAction(state, input.fromStart).collect{
          case t:WithPosition[Token] => t
          case t:NoPosition[Token] => t.setPos(input.fromStart, input.fromStart)
        }
        Success(finalTokens.reverse ++ acc)
      } else {
        if (debug) {
          println(f"- Attempting match on [${input.chars.toString.take(15)}]")
        }
        firstMatch(input, state) match {
          case None => Failure(LexerError("Invalid symbol", input.fromStart))
          case Some(RuleMatch(tokens, nextState, remainingInput)) =>
            if (debug) {
              println(f"-> Produced tokens [$tokens]")
            }
            advance(remainingInput, nextState, tokens ++ acc)
        }
      }
    }

    /**
      * Tries to match the rules against the input in priority order recursively.
      *
      * @param input input against which rules must be matched
      * @param value state value before attempting to match
      * @param remainingRules rules against which matching is yet to be attempted
      * @return a RuleMatch instance if any of the rules is matching the input prefix, None otherwise
      */
    def firstMatch(input: InputState, value: Value, remainingRules: Seq[Rule[Value, Token]] = rules, 
                   best: Option[RuleMatch[Value, Token]] = None): Option[RuleMatch[Value, Token]] =
      remainingRules match {
        case Seq() => best
        case r +: rs => r.tryTransition(value, input) match {
          case None => {
            firstMatch(input, value, rs, best)
          }
          case Some(m) => {
            if (m.inputState.fromStart.index > best.map(_.inputState.fromStart.index).getOrElse(0))
              firstMatch(input, value, rs, Some(m))
            else
              firstMatch(input, value, rs, best)
          }
        }
      }
  }
}