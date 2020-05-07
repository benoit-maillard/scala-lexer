package sl
import scala.util.matching.Regex
import Expressions._
import scala.annotation.tailrec

object Lexers {
  class Lexer[T, S](initialState: State[T, S]) {
    def tokenize(input: String): Option[List[Positioned[T]]] = {
      val start = InputState(Position(0, 1, 0), new ArrayCharSequence(input.toArray))
      advance(start, initialState, Nil).map(s => s.reverse)
    }
    
    @tailrec
    private def advance(input: InputState, state: State[T, S], acc: List[Positioned[T]]): Option[List[Positioned[T]]] =
      state.rules.firstMatch(input, state.content) match {
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

  case class State[T, C](val rules: RuleSet[T, C], val content: C)
  
  // should contain the type of the state (exemple: level of parenthesis or indentation stack)
  class RuleSet[T, C](val rules: Seq[Rule[T, C, _]]) {
    def firstMatch(input: InputState, value: C, remainingRules: Seq[Rule[T, C, _]] = rules): Option[(List[Positioned[T]], Option[State[T, C]], InputState)] = remainingRules match {
      case Seq() => None
      case r +: rs => r.tryTransition(State(this, value), input) match {
        case None => firstMatch(input, value, rs)
        case Some((nextState, producedTokens, remainingInput)) =>
          if (remainingInput.chars.length == 0) Some(producedTokens, None, remainingInput)
          else Some(producedTokens, Some(nextState), remainingInput)
      }
    }
  }

  object RuleSet {
    def apply[T, C](rules: Rule[T, C, _]*) = new RuleSet(rules)
  }

  trait Rule[T, C, E] {
    def tryTransition(state: State[T, C], input: InputState): Option[(State[T, C], List[Positioned[T]], InputState)]
  }
  
  case class StandardRule[T, C, E](val expr: CompiledExpr[E], val transition: (C, E, Position) => (State[T, C], List[Positioned[T]])) extends Rule[T, C, E] {
    override def tryTransition(state: State[T, C], input: InputState): Option[(State[T, C], List[Positioned[T]], InputState)] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, endPos)) => {
          val (newState, producedTokens) = transition(state.content, value, input.fromStart)
          Some((newState, producedTokens.reverse, InputState(endPos, input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length))))
        }
      }
  }

  case class ReflectiveRule[T, C, E](val expr: CompiledExpr[E], val transition: (C, E, Position) => (C, List[Positioned[T]])) extends Rule[T, C, E] {
    override def tryTransition(state: State[T, C], input: InputState): Option[(State[T, C], List[Positioned[T]], InputState)] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, endPos)) => {
          val (newValue, producedTokens) = transition(state.content, value, input.fromStart)
          Some((State(state.rules, newValue), producedTokens.reverse, InputState(endPos, input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length))))
        }
      }
  }
}