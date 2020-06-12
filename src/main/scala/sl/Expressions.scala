package sl

import Positions._
import Rules._

import scala.language.implicitConversions
import scala.util.matching.Regex

object Expressions {
  /**
    * Two arbitrary values
    *
    * @param left left value
    * @param right right value
    */
  case class ~[A, B](left: A, right: B)

  type Transform[A] = Seq[String] => A
  type BuildExpr = () => Seq[String]

  /**
    * A regular expression group
    *
    * @param build
    */
  case class Group(val build: () => String) {
    /**
      * Disjunction between two regular expressions
      *
      * @param that regex group
      * @return an expression that matches if at least one side matches
      */
    def |(that: Group): Group = Group(() => 
      f"(?:${this.build()})|(?:${that.build()})"
    )

    /**
      * Concatenation between two regular expressions
      *
      * @param that regex group
      * @return matches`ab` if `a` matches the left side and `b` matches the right side
      */
    def ~(that: Group): Group = Group(() =>
      f"(?:${this.build()})(?:${that.build()})"  
    )
  }

  /**
    * Repetition of a regular expression
    *
    * @param group regex group
    * @return matches if the pattern is repeated 0 or more times
    */
  def many(group: Group): Group = Group(() => f"(?:${group.build()})*")

  /**
    * Repetition at least once of a regular expression
    *
    * @param group regex group
    * @return matches if the pattern is repeated at least once
    */
  def many1(group: Group): Group = Group(() => f"(?:${group.build()})+")

  /**
    * Optional pattern
    *
    * @param group regular expression
    * @return matches the empty string or the given pattern
    */
  def opt(group: Group): Group = Group(() => f"(?:${group.build()})?")

  /**
    * Creates a regex with a single group
    *
    * @param g regexp group
    * @return expression with a single group
    */
  implicit def toExpr(g: Group): Expr = GroupUnit(g)

  /**
    * Creates a basic regular expression from a scala standard library regex string
    *
    * @param re regex string
    * @return resulting expression
    */
  implicit def unit(re: String): Group = Group(() => re)

  /**
    * Creates a basic regular expression from a scala standard library regex
    *
    * @param re regex
    * @return resulting expression
    */
  implicit def unit(re: Regex): Group = unit(re.pattern.pattern)

  /**
    * Creates a regex with a single group from a regex string
    *
    * @param re regex string
    * @return resulting expression
    */
  implicit def groupUnit(re: String): Expr = toExpr(unit(re))

  /**
    * Creates a regular expression that accepts any of the given patterns
    *
    * @param res patterns
    * @return resulting expression
    */
  def oneOf(res: String*): Group =
    unit(res.map(s => f"(?:$s)").reduceLeft(_ ++ "|" ++ _))

  /**
    * Creates a regular expression that accepts any of the given patterns after having escaped
    * special characters in each pattern.
    *
    * @param res
    * @return
    */
  def oneOfEscaped(res: String*): Group = oneOf(res.map(escape(_)):_*)

  /**
    * Escapes characters that have a particular meaning in regex syntax
    *
    * @param s regex string with unescaped characters
    * @return regex string with escaped characters
    */
  def escape(s: String): String = """[\.\^\$\*\+\?\(\)\[\{\\\|]""".r.replaceAllIn(s, """\\$0""")

  /**
    * Creates a regular expression that accepts any of the given patterns
    *
    * @param res
    * @return resulting expression
    */
  def oneOfRe(res: Regex*): Expr =
    oneOf(res.map(r => r.pattern.pattern):_*)

  /**
    * Represents the remaining input.
    *
    * @param fromStart position from the start of the original input
    * @param chars sequence of remaining characters
    */
  case class InputState(val fromStart: Position, val chars: CharSequence)

  trait Expr {
    lazy val re = build().map(s => "(" + s + ")").reduce(_ + _).r

    def build(): Seq[String]

    /**
      * Creates a new expression that matches if the second expression matches directly after the first one.
      * Groups of each expression are preserved.
      *
      * @param right second expression
      * @return resulting expression
      */
    def ~/~(right: Group): GroupSeq = GroupSeq(this, right)

    /**
      * Tries to match the expression against the input
      *
      * @param input input against which the regex is matched
      * @return the match result and the position after the entire match in input
      */
    def matchWith(input: InputState): Option[List[GroupRes]] = re.findPrefixMatchOf(input.chars) match {
      case None => None
      case Some(m) => {
        val startPos = input.fromStart

        val positions = m.subgroups.scanLeft(startPos){
          case (lastPos, str) => lastPos + str
        }

        val groupRes = m.subgroups.zip(positions.init zip positions.tail).map {
          case (str, (start, end)) => GroupRes(str, start, end)
        }

        Some(groupRes)
      }
    }

    /**
      * Associated the expression with a transition to a state with the same set of rules to create a rule
      *
      * @param transform transformation function
      * @return resulting rule
      */
    def |>[Value, Token](transform: (Value, List[GroupRes]) => (Value, List[Positioned[Token]])) = Rule(this, transform)
  }

  case class GroupUnit(group: Group) extends Expr {
    override def build() = Seq(group.build())
  }

  /**
    * A regular expression composed of at least one group
    *
    * @param transform function that takes a sequence of strings (matched groups) as input and produces a value
    * @param build function that defines how the scala standard library regex should be built
    * @param groupCount total number of groups in the expression
    */
  case class GroupSeq(left: Expr, right: Group) extends Expr {
    override def build(): Seq[String] = left.build() :+ right.build()
  }
}