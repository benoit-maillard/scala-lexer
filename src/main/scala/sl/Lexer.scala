package sl
import scala.util.matching.Regex
import Expressions._
import scala.annotation.tailrec

trait Lexers {
  type Token
  type Value

  case class LexerState(rules: RuleSet, content: Value)
  case class RuleMatch(tokens: List[Positioned[Token]], state: LexerState, inputState: InputState)

  case class Lexer(initialState: LexerState) {
    def tokenize(input: String): Option[List[Positioned[Token]]] = {
      val start = InputState(Position(0, 1, 0), new ArrayCharSequence(input.toArray))
      advance(start, initialState, Nil).map(s => s.reverse)
    }
    
    @tailrec
    private def advance(input: InputState, state: LexerState, acc: List[Positioned[Token]]): Option[List[Positioned[Token]]] =
      state.rules.firstMatch(input, state.content) match {
        case None => None
        case Some(RuleMatch(tokens, nextState, remainingInput)) =>
          if (remainingInput.chars.length == 0) Some(tokens ++ acc)
          else advance(remainingInput, nextState, tokens ++ acc)
      }
  }

  // should contain the type of the state (exemple: level of parenthesis or indentation stack)
  case class RuleSet(val rules: Rule[_]*) {
    def firstMatch(input: InputState, value: Value, remainingRules: Seq[Rule[_]] = rules): Option[RuleMatch] = remainingRules match {
      case Seq() => None
      case r +: rs => r.tryTransition(LexerState(this, value), input) match {
        case None => firstMatch(input, value, rs)
        case someMatch => someMatch
      }
    }
  }

  abstract class Rule[ExprRes](expr: CompiledExpr[ExprRes]) {
    def tryTransition(state: LexerState, input: InputState): Option[RuleMatch] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, endPos)) => {
          val (newState, producedTokens) = transitionResult(state, value, input)
          val remainingChars = input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length())
          Some(RuleMatch(producedTokens.reverse, newState, InputState(endPos, remainingChars)))
        }
      }

    def transitionResult(state: LexerState, matchVal: ExprRes, input: Expressions.InputState): (LexerState, List[Positioned[Token]])
  }

  type StandardTransition[ExprRes] = (Value, ExprRes, Position) => (LexerState, List[Positioned[Token]])

  case class StandardRule[ExprRes](expr: CompiledExpr[ExprRes], transition: StandardTransition[ExprRes]) extends Rule[ExprRes](expr) {
    override def transitionResult(state: LexerState, matchVal: ExprRes, input: Expressions.InputState) =
      transition(state.content, matchVal, input.fromStart)
  }

  type ReflectiveTransition[ExprRes] = (Value, ExprRes, Position) => (Value, List[Positioned[Token]])

  case class ReflectiveRule[ExprRes](expr: CompiledExpr[ExprRes], transition: ReflectiveTransition[ExprRes]) extends Rule[ExprRes](expr) {
    override def transitionResult(state: LexerState, matchVal: ExprRes, input: Expressions.InputState) = {
      val (newValue, producedTokens) = transition(state.content, matchVal, input.fromStart)
      (LexerState(state.rules, newValue), producedTokens)
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