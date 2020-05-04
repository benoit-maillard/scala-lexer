package sl

import Lexers._

object Expressions {
  case class ~[A, B](left: A, right: B)

  class CompiledExpr[A](expr: Expr[A]) {
    val re = expr.build().map(s => "(" + s + ")").reduce(_ + _).r

    def get() = re

    def matchWith(seq: CharSequence): Option[(A, Int)] = re.findPrefixMatchOf(seq) match {
      case None => None
      case Some(m) => {
        Some(expr.transform(m.subgroups), m.end - m.start)
      }
    }

    def |>[T, C](transform: (C, A) => (State[T, C], Seq[T])) = new Rule(this, transform)
  }

  type Transform[A] = Seq[String] => A
  type BuildExpr = () => Seq[String]
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
}