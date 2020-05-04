package sl
import scala.util.matching.Regex
import Expressions._
import scala.annotation.tailrec

object Lexers {
  class Lexer[T, S](initialState: State[T, S]) {
    def tokenize(input: CharSequence): Option[Seq[T]] = advance(input, initialState, Seq())

    def advance(input: CharSequence, state: State[T, S], acc: Seq[T]): Option[Seq[T]] =
      state.firstMatch(input) match {
        case None => None
        case Some((tokens, optNext, remainingInput)) => optNext match {
          case None => Some(acc ++ tokens)
          case Some(state) => advance(remainingInput, state, acc ++ tokens)
        } 
      }
  }

  object Lexer {
    def apply[T, S](initialState: State[T, S]) = new Lexer(initialState)
  }
  
  // should contain the type of the state (exemple: level of parenthesis or indentation stack)
  class State[T, C](val rules: Seq[Rule[T, C, _]], val value: C) {
    def firstMatch(input: CharSequence, remainingRules: Seq[Rule[T, C, _]] = rules): Option[(Seq[T], Option[State[T, C]], CharSequence)] = remainingRules match {
      case Seq() => None
      case r +: rs => r.tryTransition(this, input) match {
        case None => firstMatch(input, rs)
        case Some((nextState, producedTokens, remainingInput)) =>
          if (remainingInput.length() == 0) Some(producedTokens, None, "")
          else Some(producedTokens, Some(nextState), remainingInput)
      }
    }
  }

  object State {
    def apply[T, C](value: C)(rules: Rule[T, C, _]*) = new State(rules, value)
  }
  
  class Rule[T, C, E](val expr: CompiledExpr[E], val transition: (C, E) => (State[T, C], Seq[T])) {
    def tryTransition(state: State[T, C], input: CharSequence): Option[(State[T, C], Seq[T], CharSequence)] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, matchLength)) => {
          val (newState, producedTokens) = transition(state.value, value)
          Some((newState, producedTokens, input.subSequence(matchLength, input.length)))
        }
      }
  }

  object Rule {
    def apply[T, C, E](expr: CompiledExpr[E], transition: (C, E) => (State[T, C], Seq[T])) =
      new Rule(expr, transition)
  }
}