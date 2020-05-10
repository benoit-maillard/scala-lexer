package sl

object Expressions {
  case class ~[A, B](left: A, right: B)

  type Transform[A] = Seq[String] => A
  type BuildExpr = () => Seq[String]

  case class InputState(val fromStart: Position, val chars: CharSequence)

  class Expr[A](val transform: Transform[A], val build: BuildExpr, val groupCount: Int) {
    def ~[B](right: Expr[B]): Expr[A ~ B] = Expr(
      results => Expressions.~(
        this.transform(results.take(groupCount)),
        right.transform(results.takeRight(right.groupCount))
      ),
      () => build() ++ right.build(),
      groupCount + right.groupCount
    )

    def map[B](tr: A => B) = Expr(transform andThen tr, build, groupCount)
  }

  object Expr {
    def apply[A](transform: Transform[A], build: BuildExpr, groupCount: Int) =
      new Expr(transform, build, groupCount)

    def unit(re: String): Expr[String] = Expr(results => results.head, () => Seq(Part(re).content), 1)
  }

  case class Part(val content: String)

  case class Positioned[A](val value: A, val pos: Position)

  case class Position(val index: Int, val line: Int, val column: Int) {
    def +(char: Char): Position =
      if (char == '\n') Position(index + 1, line + 1, 0)
      else Position(index + 1, line, column + 1)

    def +(seq: String): Position = seq.foldLeft(this)((acc, c) => acc + c)
  }
}