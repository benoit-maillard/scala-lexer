package sl
import scala.util.matching.Regex
import Expressions._
import scala.annotation.tailrec

trait Lexers {
  type Token
  type Value

  class Lexer(initialState: State) {
    def tokenize(input: String): Option[List[Positioned[Token]]] = {
      val start = InputState(Position(0, 1, 0), new ArrayCharSequence(input.toArray))
      advance(start, initialState, Nil).map(s => s.reverse)
    }
    
    @tailrec
    private def advance(input: InputState, state: State, acc: List[Positioned[Token]]): Option[List[Positioned[Token]]] =
      state.rules.firstMatch(input, state.content) match {
        case None => None
        case Some((tokens, nextState, remainingInput)) =>
          if (remainingInput.chars.length == 0) Some(tokens ++ acc)
          else advance(remainingInput, nextState, tokens ++ acc)
      }
  }

  object Lexer {
    def apply(initialState: State) = new Lexer(initialState)
  }

  case class State(val rules: RuleSet, val content: Value)
  
  // should contain the type of the state (exemple: level of parenthesis or indentation stack)
  class RuleSet(val rules: Seq[Rule[_]]) {
    def firstMatch(input: InputState, value: Value, remainingRules: Seq[Rule[_]] = rules): Option[(List[Positioned[Token]], State, InputState)] = remainingRules match {
      case Seq() => None
      case r +: rs => r.tryTransition(State(this, value), input) match {
        case None => firstMatch(input, value, rs)
        case Some((producedTokens, nextState, remainingInput)) => Some(producedTokens, nextState, remainingInput)
      }
    }
  }

  object RuleSet {
    def apply[Token, Value](rules: Rule[_]*) = new RuleSet(rules)
  }

  trait Rule[ExprRes] {
    def tryTransition(state: State, input: InputState): Option[(List[Positioned[Token]], State, InputState)]
  }

  type StandardTransition[ExprRes] = (Value, ExprRes, Position) => (State, List[Positioned[Token]])

  case class StandardRule[ExprRes](expr: CompiledExpr[ExprRes], transition: StandardTransition[ExprRes]) extends Rule[ExprRes] {
    override def tryTransition(state: State, input: InputState): Option[(List[Positioned[Token]], State, InputState)] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, endPos)) => {
          val (newState, producedTokens) = transition(state.content, value, input.fromStart)
          Some((producedTokens.reverse, newState, InputState(endPos, input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length))))
        }
      }
  }

  type ReflectiveTransition[ExprRes] = (Value, ExprRes, Position) => (Value, List[Positioned[Token]])

  case class ReflectiveRule[ExprRes](expr: CompiledExpr[ExprRes], transition: ReflectiveTransition[ExprRes]) extends Rule[ExprRes] {
    override def tryTransition(state: State, input: InputState): Option[(List[Positioned[Token]], State, InputState)] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, endPos)) => {
          val (newValue, producedTokens) = transition(state.content, value, input.fromStart)
          Some((producedTokens.reverse, State(state.rules, newValue), InputState(endPos, input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length))))
        }
      }
  }

  class CompiledExpr[A](expr: Expr[A]) {
    val re = expr.build().map(s => "(" + s + ")").reduce(_ + _).r

    def get() = re

    def matchWith(input: InputState): Option[(A, Position)] = re.findPrefixMatchOf(input.chars) match {
      case None => None
      case Some(m) => {
        val startPos = input.fromStart
        val afterPos = m.subgroups.foldLeft(startPos)((pos, str) => pos + str)
        Some(expr.transform(m.subgroups), afterPos)
      }
    }

    def |~>[Token, Value](transform: StandardTransition[A]) = StandardRule(this, transform)

    def |>[Token, Value](transform: ReflectiveTransition[A]) = ReflectiveRule(this, transform)
  }
}