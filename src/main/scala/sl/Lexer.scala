package sl
import scala.util.matching.Regex
import Expressions._

object Lexers {
  class Lexer[T, S](initialState: State[T, S]) {
    def tokenize(input: CharSequence): Option[Seq[T]] = initialState.tokenize(input)
  }

  object Lexer {
    def apply[T, S](initialState: State[T, S]) = new Lexer(initialState)
  }
  
  
  // should contain the type of the state (exemple: level of parenthesis or indentation stack)
  class State[T, C](val rules: Seq[Rule[T, C, _]], val value: C) {
    def tokenize(input: CharSequence): Option[Seq[T]] = {
      /*
      TODO
      check if there is a rule that matches the beginning of the sequence
      if we found one, we apply the transition of the rule
      we use the result of the transition to produce the token and find the next state
      we call tokenize on the next state with the remainder of the string as argument
      */
      findMatch(input)
    }
  
    def findMatch(input: CharSequence, remainingRules: Seq[Rule[T, C, _]] = rules): Option[Seq[T]] = remainingRules match {
      case Seq() => None
      case r +: rs => r.tryTransition(this, input) match {
        case None => findMatch(input, rs)
        case Some((nextState, producedTokens, remainingInput)) =>
          if (remainingInput.length() == 0) Some(producedTokens)
          else nextState.findMatch(remainingInput).map(nextStateTokens => producedTokens ++ nextStateTokens)
      }
    }
  }

  object State {
    def apply[T, C](value: C)(rules: Rule[T, C, _]*) = new State(rules, value)
  }
  // class CustomState extends State[(Int, List[Int])]
  
  // should have a way to compile the expression
  // should have a map and mapWithState method
  
  
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