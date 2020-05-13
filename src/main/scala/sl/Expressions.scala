package sl

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
    * Represents the remaining input.
    *
    * @param fromStart position from the start of the original input
    * @param chars sequence of remaining characters
    */
  case class InputState(val fromStart: Position, val chars: CharSequence)

  /**
    * A regular expression that supports groups and transformations.
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
    def ~[B](right: Expr[B]): Expr[A ~ B] = Expr(
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

      /**
        * Creates a basic regular expression from a scala standard library regex string
        *
        * @param re regex string
        * @return resulting expression
        */
    def unit(re: String): Expr[String] = Expr(results => results.head, () => Seq(re), 1)
  }

  /**
    * A value and its position
    *
    * @param value value
    * @param pos position of the value
    */
  case class Positioned[A](val value: A, val pos: Position)

  /**
    * Position in a multi-line string
    *
    * @param index index from the start
    * @param line line in the string
    * @param column index from the first character of the line
    */
  case class Position(val index: Int, val line: Int, val column: Int) {
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
}