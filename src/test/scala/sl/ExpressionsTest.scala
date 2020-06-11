package sl

import org.scalatest._
import Expressions._
import scala.util.matching.Regex

class ExpressionsTest extends FlatSpec {
    def build[A](e: Expr) = e.build().map(s => "(" + s + ")").reduce(_ + _).r

    def matches[A](s: String, e: Expr) = {
        val r = build(e)
        s.matches(r.pattern.pattern)
    }

    "expressions" should "match repetitions with many" in {
        val e: Expr = ("""\d""" ~ many("""\w""") ~ """\.""") ~/~ "[1-9]"
        assert(matches("123.1", e))
        assert(matches("1bcde1.1", e))
        assert(matches("1.4", e))
        assert(!matches(".3", e))
    }

    it should "match repetitions with many1" in {
        val e: Expr = ("""\d""" ~ many1("""\w""") ~ """\.""") ~/~ "[1-9]"
        assert(matches("123.1", e))
        assert(matches("1bcde1.1", e))
        assert(!matches("1.4", e))
        assert(!matches(".3", e))
    }

    it should "match optional part" in {
        val e: Expr = opt("""ab""" ~ opt("""\.""") ~ "c")
        assert(matches("ab.c", e))
        assert(matches("abc", e))
        assert(!matches("ab..c", e))
    }

    it should "match disjunctions" in {
        val e: Expr = "[1-9][a-z]" | "[A-Z]"
        assert(matches("1a", e))
        assert(matches("A", e))
        assert(!matches("AB", e))
    }

    it should "match with composition of primitives" in {
        val e: Expr = (opt("[a-z]" | "[1-9]") ~ many1("""\W""")) | opt("%")
        assert(matches("", e))
        assert(matches("1\t  \t\n", e))
        assert(matches("\t", e))
        assert(matches("%", e))
    }
}