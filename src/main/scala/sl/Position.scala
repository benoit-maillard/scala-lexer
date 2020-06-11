package sl

object Positions {
  trait Positioned[T]
  case class WithPosition[T](value: T, start: Position, end: Position) extends Positioned[T]
  case class NoPosition[T](value: T) extends Positioned[T] {
    def setPos(start: Position, end: Position) = WithPosition(value, start, end)
    def setPosOf(source: WithPosition[T]) = setPos(source.start, source.end)
  }

  /**
  * A value and its position
  *
  * @param value value
  * @param pos position of the value
  */
  //case class Positioned[+A](val value: A, val pos: Position)

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