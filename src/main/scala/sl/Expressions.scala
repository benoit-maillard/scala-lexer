package sl

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
  implicit def toExpr(g: Group): Expr[String] = Expr(results => results.head, () => Seq(g.build()), 1)



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
  implicit def groupUnit(re: String): Expr[String] = toExpr(unit(re))

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
  def oneOfRe(res: Regex*): Expr[String] =
    oneOf(res.map(r => r.pattern.pattern):_*)

  /**
    * Represents the remaining input.
    *
    * @param fromStart position from the start of the original input
    * @param chars sequence of remaining characters
    */
  case class InputState(val fromStart: Position, val chars: CharSequence)

  /**
    * A regular expression composed of at least one group
    *
    * @param transform function that takes a sequence of strings (matched groups) as input and produces a value
    * @param build function that defines how the scala standard library regex should be built
    * @param groupCount total number of groups in the expression
    */
  class Expr[A](val transform: Transform[A], val build: BuildExpr, val groupCount: Int) {
    /**
      * Creates a new expression that matches if the second expression matches directly after the first one.
      * Groups of each expression are preserved.
      *
      * @param right second expression
      * @return resulting expression
      */
    def ~/~[B](right: Expr[B]): Expr[A ~ B] = Expr(
      results => Expressions.~(
        this.transform(results.take(groupCount)),
        right.transform(results.takeRight(right.groupCount))
      ),
      () => build() ++ right.build(),
      groupCount + right.groupCount
    )

    /**
      * Transforms the resulting match value of the expression.
      * 
      * For example, an expression can be matched to produce a numeric value instead of a string
      * {{{
      * unit("""\d""").map(_.toInt)
      * }}}
      *
      * @param tr transformation function
      * @return resulting expression
      */
    def map[B](tr: A => B) = Expr(transform andThen tr, build, groupCount)
  }

  /**
    * Companion object for Expr
    */
  object Expr {
    /**
      * Creates an expression
      *
      * @param transform function that takes a sequence of strings (matched groups) as input and produces a value
      * @param build function that defines how the scala standard library regex should be built
      * @param groupCount total number of groups in the expression
      * @return resulting expression
      */
    def apply[A](transform: Transform[A], build: BuildExpr, groupCount: Int) =
      new Expr(transform, build, groupCount)
  }

  /**
    * A value and its position
    *
    * @param value value
    * @param pos position of the value
    */
  case class Positioned[+A](val value: A, val pos: Position)

  /**
    * Position in a multi-line string
    *
    * @param index index from the start
    * @param line line in the string
    * @param column index from the first character of the line
    */
  case class Position(index: Int, line: Int, column: Int, offset: Int = 0) {
    /**
      * Advances the position to after the given character
      *
      * @param char character to add to the position
      * @return updated position
      */
    def +(char: Char): Position =
      if (char == '\n') Position(index + 1, line + 1, 1)
      else Position(index + 1, line, column + 1)

    /**
      * Advances the position to after the given string
      *
      * @param seq string to att to the position
      * @return updated position
      */
    def +(seq: String): Position = seq.foldLeft(this)((acc, c) => acc + c)
  }

  /**
    * Companion object for Position class
    */
  object Position {
    /**
      * Initial position in input
      *
      * @return position
      */
    def initial = Position(0, 1, 1)
  }
}