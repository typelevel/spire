package spire.math

import org.scalatest.FunSuite
import spire.implicits.{eqOps => _, _}
import spire.random.{Uniform, Dist}

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class IntervalTest extends FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)
  def oc(n1: Double, n2: Double) = Interval.openBelow(n1, n2)

  val a = cc(0.0, 4.0)
  test("a.contains(0.0) is true") { assert(a.contains(0.0) === true) }
  test("a.crosses(0.0) is false") { assert(a.crosses(0.0) === false) }
  test("a.contains(3.334) is true") { assert(a.contains(3.334) === true) }
  test("a.contains(8.334) is false") { assert(a.contains(8.334) === false) }

  val b = cc(-8.0, 2.0)
  test("b.contains(0.0) is true") { assert(b.contains(0.0) === true) }
  test("b.crosses(0.0) is true") { assert(b.crosses(0.0) === true) }

  val c = oc(0.0, 1.0)
  test("c.contains(0.0) is false") { assert(c.contains(0.0) === false) }
  test("c.crosses(0.0) is false") { assert(c.crosses(0.0) === false) }
}

class RingIntervalTest extends FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)

  val a = cc(0.0, 4.0)
  test("a + a") { assert(a + a === cc(0.0, 8.0)) }
  test("a - a") { assert(a - a === cc(-4.0, 4.0)) }
  test("a * a") { assert(a * a === cc(0.0, 16.0)) }

  val b = cc(-8.0, 2.0)
  test("b + b") { assert(b + b === cc(-16.0, 4.0)) }
  test("b - b") { assert(b - b === cc(-10.0, 10.0)) }
  test("b * b") { assert(b * b === cc(-16.0, 64.0)) }
}

// TODO: this is just the tip of the iceberg... we also need to worry about
// unbounded intervals, closed vs open bounds, etc.
class ContinuousIntervalTest extends FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)

  val a = 2.0
  val b = 5.0
  val c = 1.0
  val d = 4.0

  // numerator interval crosses zero
  test("[-a,b] / [c,d]") { assert(cc(-a, b) / cc(c, d) === cc(-a / c, b / c)) }
  test("[-a,b] / [-d,-c]") { assert(cc(-a, b) / cc(-d, -c) === cc(b / -c, -a / -c)) }

  // numerator interval is positive
  test("[a,b] / [-d,-c]") { assert(cc(a, b) / cc(-d, -c) === cc(b / -c, a / -d)) }
  test("[a,b] / [c,d]") { assert(cc(a, b) / cc(c, d) === cc(a / d, b / c)) }
  
  // numerator interval is negative
  test("[-b,-a] / [-d,-c]") { assert(cc(-b, -a) / cc(-d, -c) === cc(-a / -d, -b / -c)) }
  test("[-b,-a] / [c,d]") { assert(cc(-b, -a) / cc(c, d) === cc(-b / c, -a / d)) }
}

class IntervalReciprocalTest extends FunSuite {

  def t(a: Interval[Rational], b: Interval[Rational]): Unit =
    test(s"[1]/$a = $b") { assert(a.reciprocal === b) }

  def error(a: Interval[Rational]): Unit =
    test(s"[1]/$a = error") {
      intercept[ArithmeticException] { a.reciprocal }
    }

  // point(x)
  t(Interval.point(r"1/5"), Interval.point(r"5"))
  t(Interval.point(r"-99"), Interval.point(r"-1/99"))
  error(Interval.point(r"0"))

  // above(x)
  t(Interval.above(r"3"), Interval.open(r"0", r"1/3"))
  t(Interval.above(r"0"), Interval.above(r"0")) //fixme
  error(Interval.above(r"-1"))

  // atOrAbove(x)
  t(Interval.atOrAbove(r"1/9"), Interval.openBelow(r"0", r"9"))
  error(Interval.atOrAbove(r"0"))
  error(Interval.atOrAbove(r"-2"))

  // closed(x, y)
  t(Interval.closed(r"1/2", r"4"), Interval.closed(r"1/4", r"2"))
  error(Interval.closed(r"0", r"6"))
  error(Interval.closed(r"-2", r"1/5"))
  error(Interval.closed(r"-1/9", r"0"))
  t(Interval.closed(r"-70", r"-14"), Interval.closed(r"-1/14", r"-1/70"))

  // openBelow(x, y)
  t(Interval.openBelow(r"1/2", r"4"), Interval.openAbove(r"1/4", r"2"))
  t(Interval.openBelow(r"0", r"6"), Interval.atOrAbove(r"1/6")) //fixme
  error(Interval.openBelow(r"-2", r"1/5"))
  error(Interval.openBelow(r"-1/9", r"0"))
  t(Interval.openBelow(r"-70", r"-14"), Interval.openAbove(r"-1/14", r"-1/70"))

  // openAbove(x, y)
  t(Interval.openAbove(r"1/2", r"4"), Interval.openBelow(r"1/4", r"2"))
  error(Interval.openAbove(r"0", r"6"))
  error(Interval.openAbove(r"-2", r"1/5"))
  t(Interval.openAbove(r"-1/9", r"0"), Interval.atOrBelow(r"-9")) //fixme
  t(Interval.openAbove(r"-70", r"-14"), Interval.openBelow(r"-1/14", r"-1/70"))

  // open
  t(Interval.open(r"1/2", r"4"), Interval.open(r"1/4", r"2"))
  t(Interval.open(r"0", r"6"), Interval.above(r"1/6")) //fixme
  error(Interval.open(r"-2", r"1/5"))
  t(Interval.open(r"-1/9", r"0"), Interval.below(r"-9")) //fixme
  t(Interval.open(r"-70", r"-14"), Interval.open(r"-1/14", r"-1/70"))

  // below(x)
  error(Interval.below(r"3"))
  t(Interval.below(r"0"), Interval.below(r"0")) //fixme
  t(Interval.below(r"-1"), Interval.open(r"-1", r"0")) //fixme

  // atOrBelow(x)
  error(Interval.atOrBelow(r"1/9"))
  error(Interval.atOrBelow(r"0"))
  t(Interval.atOrBelow(r"-2"), Interval.openAbove(r"-1/2", r"0")) //fixme
}

class IntervalCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import ArbitrarySupport.{interval, rational}

  val rng = spire.random.mutable.GlobalRng

  def sample(int: Interval[Rational], n: Int): Array[Rational] =
    if (int.isEmpty) {
       Array.empty[Rational]
    } else {
      val underlyingf: () => Rational = (int.lowerPair, int.upperPair) match {
        case (None, None) => () => Rational(rng.nextGaussian) * Long.MaxValue
        case (Some((x, _)), None) => () => x + (Rational(rng.nextGaussian).abs * Long.MaxValue)
        case (None, Some((y, _))) => () => y - (Rational(rng.nextGaussian).abs * Long.MaxValue)
        case (Some((x, _)) ,Some((y, _))) => () => x + Rational(rng.nextDouble) * (y - x)
      }

      def nextf(): Rational = {
        val r = underlyingf()
        if (int.contains(r)) r else nextf()
      }

      Array.fill(n)(nextf())
    }

  val tries = 100

  def testUnop(f: Interval[Rational] => Interval[Rational])(g: Rational => Rational) {
    forAll { (a: Interval[Rational]) =>
      val c: Interval[Rational] = f(a)
      sample(a, tries).foreach { x =>
        val ok = c.contains(g(x))
        if (!ok) println("%s failed on %s" format (a, x.toString))
        ok shouldBe true
      }
    }
  }

  def testBinop(f: (Interval[Rational], Interval[Rational]) => Interval[Rational])(g: (Rational, Rational) => Rational) {
    forAll { (a: Interval[Rational], b: Interval[Rational]) =>
      val c: Interval[Rational] = f(a, b)
      sample(a, tries).zip(sample(b, tries)).foreach { case (x, y) =>
        val ok = c.contains(g(x, y))
        if (!ok) println("(%s, %s) failed on (%s, %s)" format (a, b, x.toString, y.toString))
        ok shouldBe true
      }
    }
  }

  property("sampled unop abs") { testUnop(_.abs)(_.abs) }
  property("sampled unop -") { testUnop(-_)(-_) }
  property("sampled unop pow(2)") { testUnop(_.pow(2))(_.pow(2)) }
  property("sampled unop pow(3)") { testUnop(_.pow(3))(_.pow(3)) }

  property("sampled binop +") { testBinop(_ + _)(_ + _) }
  property("sampled binop -") { testBinop(_ - _)(_ - _) }
  property("sampled binop *") { testBinop(_ * _)(_ * _) }
  property("sampled binop min") { testBinop(_ min _)(_ min _) }
  property("sampled binop max") { testBinop(_ max _)(_ max _) }
}
