package sl
import scala.util.matching.Regex
import Expressions._
import scala.annotation.tailrec
import java.io.File
import scala.io.Source

trait Lexers {
  type Token
  type Value

  /**
    * Lexer state at a given time. A state is composed of a set of rules and of a value.
    * The value can be any value that has some incidence on the tokens to be produced by the lexers.
    * For example, the value could be the current indentation level in an indentation-based grammar.
    *
    * @param rules rules that are currently relevant
    * @param value state content
    */
  case class LexerState(rules: RuleSet, value: Value)

  /**
    * Result of matching a given rule with input prefix.
    *
    * @param tokens tokens produced by matching
    * @param state state produced by matching
    * @param inputState progression in input string
    */
  case class RuleMatch(tokens: List[Positioned[Token]], state: LexerState, inputState: InputState)

  /**
    * Error in the lexing process
    *
    * @param message error message
    * @param pos position where the problem appeared
    */
  case class LexerError(message: String, pos: Position) extends Error

  /**
    * Lexer that can produce tokens given an input string.
    *
    * @param initialState set of rules and value that can be matched at the start of input string
    */
  case class Lexer(initialState: LexerState) {
    /**
      * Produces tokens using the successive rules and the given string.
      *
      * @param input string containing tokens
      * @return produced tokens if matching rules were found for the entire input, None otherwise
      */
    def tokenizeFromString(input: String): Option[List[Positioned[Token]]] = {
      val start = InputState(Position(0, 1, 0), new ArrayCharSequence(input.toArray))
      advance(start, initialState, Nil).map(s => s.reverse)
    }

    /**
      * Produces tokens using the successive rules and the given input file.
      *
      * @param path file containing tokens
      * @return produced tokens if matching rules were found for the entire input, None otherwise
      */
    def tokenizeFromFile(path: String): Option[List[Positioned[Token]]] = {
      val content = Source.fromFile(path).mkString
      tokenizeFromString(content)
    }
    
    // uses successive rules to make progress with input
    @tailrec
    private def advance(input: InputState, state: LexerState, acc: List[Positioned[Token]]): Option[List[Positioned[Token]]] =
      state.rules.firstMatch(input, state.value) match {
        case None => None
        case Some(RuleMatch(tokens, nextState, remainingInput)) =>
          if (remainingInput.chars.length == 0) Some(tokens ++ acc)
          else advance(remainingInput, nextState, tokens ++ acc)
      }
  }

  /**
    * Ordered set of rules for matching against input. The order in which rules are given is the
    * order of priority in which they are matched. If rule A has higher priority than B, only
    * rule A is taken into account if it is a match, even if B is a match as well.
    *
    * @param rules
    */
  case class RuleSet(val rules: Rule[_]*) {
    /**
      * Tries to match the rules of the set against the input in priority order recursively.
      *
      * @param input input against which rules must be matched
      * @param value state value before attempting to match
      * @param remainingRules rules against which matching is yet to be attempted
      * @return a RuleMatch instance if any of the rules is matching the input prefix, None otherwise
      */
    def firstMatch(input: InputState, value: Value, remainingRules: Seq[Rule[_]] = rules): Option[RuleMatch] = remainingRules match {
      case Seq() => throw LexerError("Unkown token", input.fromStart)
      case r +: rs => r.tryTransition(LexerState(this, value), input) match {
        case None => firstMatch(input, value, rs)
        case someMatch => someMatch
      }
    }

    /**
      * Creates a new state based on the current set and a given value.
      *
      * @param value value for the state
      * @return State based on the current state and the value
      */
    def apply(value: Value): LexerState = LexerState(this, value)
  }

  /**
    * Rule associating a regular expression with a transition function
    *
    * @param expr regular expression that is matched against the input
    */
  abstract class Rule[ExprRes](expr: CompiledExpr[ExprRes]) {
    /**
      * Tries to match the rule expression against the input.
      *
      * @param state state before the matching is attempted
      * @param input input against which matching is attempted
      * @return a RuleMatch instance if the regular expression is matching the input prefix, None otherwise
      */
    def tryTransition(state: LexerState, input: InputState): Option[RuleMatch] =
      expr.matchWith(input) match {
        case None => None
        case Some((value, endPos)) => {
          val (newState, producedTokens) = transitionResult(state, value, input)
          val remainingChars = input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length())
          Some(RuleMatch(producedTokens.reverse, newState, InputState(endPos, remainingChars)))
        }
      }
    
    // gets the result of the transition associated with the rule
    protected def transitionResult(state: LexerState, matchVal: ExprRes, input: InputState): (LexerState, List[Positioned[Token]])
  }

  
  type StandardTransition[ExprRes] = (Value, ExprRes, Position) => (LexerState, List[Positioned[Token]])

  /**
    * Rule with a transition from a state to any other state
    *
    * @param expr regular expression that is matched against the input
    * @param transition transition to any other state
    */
  case class StandardRule[ExprRes](expr: CompiledExpr[ExprRes], transition: StandardTransition[ExprRes]) extends Rule[ExprRes](expr) {
    override def transitionResult(state: LexerState, matchVal: ExprRes, input: InputState) =
      transition(state.value, matchVal, input.fromStart)
  }

  type ReflectiveTransition[ExprRes] = (Value, ExprRes, Position) => (Value, List[Positioned[Token]])

  /**
    * Rule with a transition from a state to a state with the same rules as previously
    *
    * @param expr regular expression that is matched against the input
    * @param transition transition to a state with same rules as previously
    */
  case class ReflectiveRule[ExprRes](expr: CompiledExpr[ExprRes], transition: ReflectiveTransition[ExprRes]) extends Rule[ExprRes](expr) {
    override def transitionResult(state: LexerState, matchVal: ExprRes, input: InputState) = {
      val (newValue, producedTokens) = transition(state.value, matchVal, input.fromStart)
      (LexerState(state.rules, newValue), producedTokens)
    }
  }

  /**
    * Top-level, compiled regular expression
    *
    * @param expr underlying expression
    */
  class CompiledExpr[A](expr: Expr[A]) {
    val re = expr.build().map(s => "(" + s + ")").reduce(_ + _).r

    /**
      * The regular expression in its scala standard library form
      *
      * @return regex
      */
    def get: Regex = re

    /**
      * Tries to match the expression against the input
      *
      * @param input input against which the regex is matched
      * @return the match result and the position after the entire match in input
      */
    def matchWith(input: InputState): Option[(A, Position)] = re.findPrefixMatchOf(input.chars) match {
      case None => None
      case Some(m) => {
        val startPos = input.fromStart
        val afterPos = m.subgroups.foldLeft(startPos)((pos, str) => pos + str)
        Some(expr.transform(m.subgroups), afterPos)
      }
    }
    
    /**
      * Associates the expression with a transition to any state to create a rule
      *
      * @param transform transformation function
      * @return resulting rule
      */
    def |~>[Token, Value](transform: StandardTransition[A]) = StandardRule(this, transform)

    /**
      * Associated the expression with a transition to a state with the same set of rules to create a rule
      *
      * @param transform transformation function
      * @return resulting rule
      */
    def |>[Token, Value](transform: ReflectiveTransition[A]) = ReflectiveRule(this, transform)
  }
}