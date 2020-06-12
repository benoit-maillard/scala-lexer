package sl

import Positions._
import Expressions._

object Rules {
  /**
   * Result of matching a given rule with input prefix.
   *
   * @param tokens tokens produced by matching
   * @param state state produced by matching
   * @param inputState progression in input string
   */
  case class RuleMatch[Value, Token](tokens: List[WithPosition[Token]], state: Value, inputState: InputState)

  case class GroupRes(str: String, start: Position, end: Position)
  
  /**
    * Rule associating a regular expression with a transition function
    *
    * @param expr regular expression that is matched against the input
    */
  case class Rule[Value, Token](expr: Expr, transition: (Value, List[GroupRes]) => (Value, List[Positioned[Token]])) {
    /**
      * Tries to match the rule expression against the input.
      *
      * @param state state before the matching is attempted
      * @param input input against which matching is attempted
      * @return a RuleMatch instance if the regular expression is matching the input prefix, None otherwise
      */
    def tryTransition(state: Value, input: InputState): Option[RuleMatch[Value, Token]] =
      expr.matchWith(input) match {
        case None => {
          //if (debug) println(f"  - No match for ${expr.re.pattern.pattern}")
          None
        } 
        case Some(res) => {
          val endPos = res.last.end
          val (newState, producedTokens) = transitionResult(state, res, input)
          val remainingChars = input.chars.subSequence(endPos.index - input.fromStart.index, input.chars.length())
          
          /*if (debug){
            println(f"  - Match for ${expr.re.pattern.pattern}")
            println(f"    - Result [$res] for tokens ${producedTokens.reverse}")
          }*/

          Some(RuleMatch(producedTokens.reverse, newState, InputState(endPos, remainingChars)))
        }
      }
    
    // gets the result of the transition associated with the rule
    protected def transitionResult(state: Value, matchVal: List[GroupRes], input: InputState): (Value, List[WithPosition[Token]]) = {
      val (newValue, producedTokens) = transition(state, matchVal)

      // add a default position (spans the entire result of the regex)
      val positionedTokens = producedTokens.collect {
        case t:WithPosition[Token] => t
        case t:NoPosition[Token] => t.setPos(matchVal.head.start, matchVal.last.end)
      }

      (newValue, positionedTokens)
    }
  }
}