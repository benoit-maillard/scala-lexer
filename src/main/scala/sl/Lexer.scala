package sl
import scala.util.matching.Regex
import Expressions._
import scala.annotation.tailrec

object Lexers {
  class Lexer[T, S](initialState: State[T, S]) {
    def tokenize(input: String): Option[Seq[Positioned[T]]] = {
      val start = InputState(Position(0, 1, 0), new ArrayCharSequence(input.toArray))
      advance(start, initialState, Seq()).map(s => s.reverse)
    }
    
    @tailrec
    private def advance(input: InputState, state: State[T, S], acc: Seq[Positioned[T]]): Option[Seq[Positioned[T]]] =
      state.firstMatch(input) match {
        case None => None
        case Some((tokens, optNext, remainingInput)) => optNext match {
          case None => Some(tokens ++ acc)
          case Some(state) => advance(remainingInput, state, tokens ++ acc)
        }
      }
  }

  object Lexer {
    def apply[T, S](initialState: State[T, S]) = new Lexer(initialState)
  }
  
  // should contain the type of the state (exemple: level of parenthesis or indentation stack)
  class State[T, C](val rules: Seq[Rule[T, C, _]], val value: C) {
    def firstMatch(input: InputState, remainingRules: Seq[Rule[T, C, _]] = rules): Option[(Seq[Positioned[T]], Option[State[T, C]], InputState)] = remainingRules match {
      case Seq() => None
      case r +: rs => r.tryTransition(this, input) match {
        case None => firstMatch(input, rs)
        case Some((nextState, producedTokens, remainingInput)) =>
          if (remainingInput.chars.length == 0) Some(producedTokens, None, remainingInput)
          else Some(producedTokens, Some(nextState), remainingInput)
      }
    }
  }

  object State {
    def apply[T, C](value: C)(rules: Rule[T, C, _]*) = new State(rules, value)
  }
  
  class Rule[T, C, E](val expr: CompiledExpr[E], val transition: (C, E, Position) => (State[T, C], Seq[Positioned[T]])) {
    def tryTransition(state: State[T, C], input: InputState): Option[(State[T, C], Seq[Positioned[T]], InputState)] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, endPos)) => {
          val (newState, producedTokens) = transition(state.value, value, input.fromStart)
          Some((newState, producedTokens.reverse, InputState(endPos, input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length))))
        }
      }
  }

  object Rule {
    def apply[T, C, E](expr: CompiledExpr[E], transition: (C, E, Position) => (State[T, C], Seq[Positioned[T]])) =
      new Rule(expr, transition)
  }
}