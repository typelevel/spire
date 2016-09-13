package spire
package math

import spire.math.ArbitrarySupport.{Positive, NonNegative}

import scala.util.Try

import org.scalatest.FunSuite
import spire.implicits.{eqOps => _, _}
import spire.laws.arb.{interval => interval_, rational}
import spire.random.{Uniform, Dist}

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._
import interval._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class IntervalTest extends FunSuite {
  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)
  def co(n1: Double, n2: Double) = Interval.openUpper(n1, n2)
  def oc(n1: Double, n2: Double) = Interval.openLower(n1, n2)
  def oo(n1: Double, n2: Double) = Interval.open(n1, n2)

  val e = Interval.empty[Double]
  val all = Interval.all[Double]

  test("[2, inf] is a superset of empty") { assert(Interval.atOrAbove(2).isSupersetOf(Interval.empty[Int])) }
  test("empty is empty") { assert(e.isEmpty) }
  test("point is point") { assert(Interval.point(2).isPoint) }
  test("[2,2] is point") { assert(Interval.closed(2, 2).isPoint) }
  test("[3,2] is empty") { assert(Interval.closed(3, 2).isEmpty) }
  test("empty interval is not above -1") { assert(!Interval.empty[Int].hasAbove(-1)) }
  test("empty interval is not below 1") { assert(!Interval.empty[Int].hasBelow(1)) }
  test("[2] has above 0") { assert(Interval.point(2).hasAbove(0)) }
  test("[-2] has below 0") { assert(Interval.point(-2).hasBelow(0)) }
  test("[0, 1] has at or above 1") { assert(Interval.closed(0, 1).hasAtOrAbove(1)) }
  test("[1, 2] has at or above 1") { assert(Interval.closed(1, 2).hasAtOrAbove(1)) }
  test("[1, 2] has above 1") { assert(Interval.closed(1, 2).hasAtOrAbove(1)) }
  test("(1, 2] has above 1") { assert(Interval.openLower(1, 2).hasAtOrAbove(1)) }

  test("Interval.point(2).toString == [2]") { assert(Interval.point(2).toString === "[2]") }
  test("Interval.empty.toString == (Ø)") { assert(Interval.empty[Int].toString === "(Ø)") }

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

  test("[3, 6] -- [3, 6] = nil") { assert(cc(3D, 6D) -- cc(3D, 6D) === Nil) }
  test("[3, 6] -- empty = [3, 6]") { assert(cc(3D, 6D) -- e === List(cc(3D, 6D))) }
  test("[3, 6] -- all = nil") { assert(cc(3D, 6D) -- all === Nil) }
  test("[3, 6] -- [4, 6] = [3, 4)") { assert(cc(3D, 6D) -- cc(4D, 6D) === List(co(3D, 4D))) }
  test("[3, 6] -- [4, 5] = [3, 4), (5, 6]") { assert(cc(3D, 6D) -- cc(4D, 5D) === List(co(3D, 4D), oc(5D, 6D))) }
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

  import interval.{Open, Unbound, Closed}
  val c = 4.0
  test("-(c, ∞) =  (-∞, -c)") {
    assert( -Interval.fromBounds(Open(c), Unbound()) ===
      Interval.fromBounds(Unbound(), Open(-c)) )
  }
  test("-(-∞, c] =  [-c, ∞)") {
    assert( -Interval.fromBounds(Unbound(), Closed(c)) ===
      Interval.fromBounds(Closed(-c), Unbound()) )
  }
  test("(c, ∞) * (-c) =  (-∞, -c * c), c > 0") {
    assert( Interval.fromBounds(Open(c), Unbound()) * (-c) ===
      Interval.fromBounds(Unbound(), Open(-c*c)) )
  }
  test("(-∞, c] * (-c) =  [-c * c, ∞), c > 0") {
    assert( Interval.fromBounds(Unbound(), Closed(c)) * (-c) ===
      Interval.fromBounds(Closed(-c*c), Unbound()) )
  }
  test("Interval multiplication bug #372")   {
    val a = Interval(-1, 1)
    val b = Interval.above(1)
    val x = -1
    val y = 10
    assert(a.contains(x))
    assert(b.contains(y))
    assert((a*b).contains(x*y))
  }
  test("Interval multiplication bug 1")   {
    val a = Interval(-3, -2)
    val b = Interval.above(-10)
    val x = -3
    val y = -9
    assert(a.contains(x))
    assert(b.contains(y))
    assert((a*b).contains(x*y))
  }
  test("Interval multiplication bug 2") {
    val a = Interval.atOrBelow(0)
    val b = Interval.below(-1)
    assert((a*b).contains(0))
  }
  test("Interval multiplication bug 3") {
    val a = Interval.atOrBelow(0)
    val b = Interval.open(-2, -1)
    assert((a*b).contains(0))
  }
  test("Interval multiplication bug 4") {
    val a = Interval.above(2)
    val b = Interval.closed(0, 1)
    assert((a*b).contains(0))
  }
}

class IntervalGeometricPartialOrderTest extends FunSuite {
  import spire.optional.intervalGeometricPartialOrder._

  import Interval.{openUpper, openLower, closed, open, point}
  test("[2, 3) === [2, 3)") { assert(openUpper(2, 3).partialCompare(openUpper(2, 3)) == 0.0) }
  test("[2, 3) < [3, 4]") { assert(openUpper(2, 3) < closed(3, 4)) }
  test("[2, 3] < (3, 4]") { assert(closed(2, 3) < openLower(3, 4)) }
  test("[2, 3] cannot be compared to [3, 4]") { assert(closed(2, 3).partialCompare(closed(3, 4)).isNaN) }
  test("[3, 4] > [2, 3)") { assert(closed(3, 4) > openUpper(2, 3)) }
  test("[2, 3) <= [3, 4]") { assert(openUpper(2, 3) <= closed(3, 4)) }
  test("[3, 4] >= [2, 3)") { assert(closed(3, 4) >= openUpper(2, 3)) }
  test("not [2, 3] < [3, 4]") { assert(!(closed(2, 3) < closed(3, 4))) }
  test("not [2, 3] <= [3, 4]") { assert(!(closed(2, 3) <= closed(3, 4))) }
  test("not [3, 4] > [3, 4]") { assert(!(closed(2, 3) > closed(3, 4))) }
  test("not [3, 4] >= [3, 4]") { assert(!(closed(2, 3) >= closed(3, 4))) }
  test("empty.partialCompare(empty) == 0.0") { assert(open(2, 2).partialCompare(open(3, 3)) == 0.0) }
  test("empty cannot be compared to [2, 3]") { assert(open(2, 2).partialCompare(closed(2, 3)).isNaN) }
  test("[2, 3] cannot be compared to empty") { assert(closed(2, 3).partialCompare(open(2, 2)).isNaN) }
  test("Minimal and maximal elements of {[1], [2, 3], [2, 4]}") {
    val intervals = Seq(point(1), closed(2, 3), closed(2, 4))
    assert(intervals.pmin.toSet == Set(point(1)))
    assert(intervals.pmax.toSet == Set(closed(2, 3), closed(2, 4)))
  }
}

class IntervalSubsetPartialOrderTest extends FunSuite {
  import spire.optional.intervalSubsetPartialOrder._

  import Interval.{openUpper, openLower, closed, open, point}

  test("Minimal and maximal elements of {[1, 3], [3], [2], [1]} by subset partial order") {
    val intervals = Seq(closed(1, 3), point(3), point(2), point(1))
    assert(intervals.pmin.toSet == Set(point(1), point(2), point(3)))
    assert(intervals.pmax.toSet == Set(closed(1, 3)))
  }
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
  t(Interval.atOrAbove(r"1/9"), Interval.openLower(r"0", r"9"))
  error(Interval.atOrAbove(r"0"))
  error(Interval.atOrAbove(r"-2"))

  // closed(x, y)
  t(Interval.closed(r"1/2", r"4"), Interval.closed(r"1/4", r"2"))
  error(Interval.closed(r"0", r"6"))
  error(Interval.closed(r"-2", r"1/5"))
  error(Interval.closed(r"-1/9", r"0"))
  t(Interval.closed(r"-70", r"-14"), Interval.closed(r"-1/14", r"-1/70"))

  // openLower(x, y)
  t(Interval.openLower(r"1/2", r"4"), Interval.openUpper(r"1/4", r"2"))
  t(Interval.openLower(r"0", r"6"), Interval.atOrAbove(r"1/6")) //fixme
  error(Interval.openLower(r"-2", r"1/5"))
  error(Interval.openLower(r"-1/9", r"0"))
  t(Interval.openLower(r"-70", r"-14"), Interval.openUpper(r"-1/14", r"-1/70"))

  // openUpper(x, y)
  t(Interval.openUpper(r"1/2", r"4"), Interval.openLower(r"1/4", r"2"))
  error(Interval.openUpper(r"0", r"6"))
  error(Interval.openUpper(r"-2", r"1/5"))
  t(Interval.openUpper(r"-1/9", r"0"), Interval.atOrBelow(r"-9")) //fixme
  t(Interval.openUpper(r"-70", r"-14"), Interval.openLower(r"-1/14", r"-1/70"))

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
  t(Interval.atOrBelow(r"-2"), Interval.openUpper(r"-1/2", r"0")) //fixme
}

class IntervalCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {


  property("x ⊆ x") {
    forAll { (x: Interval[Rational]) => (x isSupersetOf x) shouldBe true }
  }

  property("x ⊆ (x | y) && y ⊆ (x | y)") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      val z = x | y
      (z isSupersetOf x) shouldBe true
      (z isSupersetOf y) shouldBe true
    }
  }

  property("(x & y) ⊆ x && (x & y) ⊆ y") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      val z = x & y
      (x isSupersetOf z) shouldBe true
      (y isSupersetOf z) shouldBe true
    }
  }

  val rng = spire.random.GlobalRng

  property("(x -- y) ⊆ x && (x -- y) & y = Ø") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      (x -- y).foreach { zi =>
        (zi isSubsetOf x) shouldBe true
        (zi intersects y) shouldBe false
      }
    }
  }

  property("(x -- Ø) = x") {
    forAll { (x: Interval[Rational]) =>
      if (x.nonEmpty) {
        (x -- Interval.empty[Rational]) shouldBe List(x)
      }
    }
  }

  property("(x -- x) = Ø") {
    forAll { (x: Interval[Rational]) =>
      (x -- x) shouldBe Nil
    }
  }

  property("(x -- (-∞, ∞)) = Ø") {
    forAll { (x: Interval[Rational]) =>
      (x -- Interval.all[Rational]) shouldBe Nil
    }
  }

  def sample(int: Interval[Rational], n: Int): Array[Rational] =
    if (int.isEmpty) {
       Array.empty[Rational]
    } else {
      import spire.math.interval.ValueBound
      val underlyingf: () => Rational = (int.lowerBound, int.upperBound) match {
        case (ValueBound(x) , ValueBound(y)) => () => rng.nextInt(10) match {
          case 0 => x
          case 9 => y
          case _ => x + Rational(rng.nextDouble) * (y - x)
        }
        case (ValueBound(x) , _) => () => rng.nextInt(5) match {
          case 0 => x
          case _ => x + (Rational(rng.nextGaussian).abs * Long.MaxValue)
        }
        case (_, ValueBound(y)) => () => rng.nextInt(5) match {
          case 4 => y
          case _ => y - (Rational(rng.nextGaussian).abs * Long.MaxValue)
        }
        case (_ , _) => () => Rational(rng.nextGaussian) * Long.MaxValue
      }

      def nextf(): Rational = {
        val r = underlyingf()
        if (int.contains(r)) r else nextf()
      }

      Array.fill(n)(nextf())
    }

  val tries = 100

  def testUnop(f: Interval[Rational] => Interval[Rational])(g: Rational => Rational): Unit = {
    forAll { (a: Interval[Rational]) =>
      val c: Interval[Rational] = f(a)
      sample(a, tries).foreach { x =>
        val ok = c.contains(g(x))
        if (!ok) println("%s failed on %s" format (a, x.toString))
        ok shouldBe true
      }
    }
  }

  def testBinop(f: (Interval[Rational], Interval[Rational]) => Interval[Rational])(g: (Rational, Rational) => Rational): Unit = {
    forAll { (a: Interval[Rational], b: Interval[Rational]) =>
      val c: Interval[Rational] = f(a, b)
      sample(a, tries).zip(sample(b, tries)).foreach { case (x, y) =>
        if (!a.contains(x)) println("%s does not contain %s" format (a, x))
        if (!b.contains(y)) println("%s does not contain %s" format (b, y))
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
  property("sampled binop vmin") { testBinop(_ vmin _)(_ min _) }
  property("sampled binop vmax") { testBinop(_ vmax _)(_ max _) }

  property("toString/apply") {
    forAll { (x: Interval[Rational]) =>
      Interval(x.toString) shouldBe x
    }
  }

  property("points compare as scalars") {
    import spire.optional.intervalGeometricPartialOrder._

    import spire.algebra.{Order, PartialOrder}
    forAll { (x: Rational, y: Rational) =>
      val a = Interval.point(x)
      val b = Interval.point(y)
      PartialOrder[Interval[Rational]].tryCompare(a, b).get shouldBe Order[Rational].compare(x, y)
      val Some(Point(vmin)) = a.pmin(b)
      vmin shouldBe x.min(y)
      val Some(Point(vmax)) = a.pmax(b)
      vmax shouldBe x.max(y)
    }
  }

  property("(-inf, a] < [b, inf) if a < b") {
    import spire.optional.intervalGeometricPartialOrder._

    import spire.algebra.{Order, PartialOrder}
    forAll { (a: Rational, w: Positive[Rational]) =>
      val b = a + w.num
      // a < b
      val i = Interval.atOrBelow(a)
      val j = Interval.atOrAbove(b)
      (i < j) shouldBe true
      (i >= j) shouldBe false
      (j > i) shouldBe true
      (j <= i) shouldBe false
    }
  }

  property("(-inf, a] does not compare to [b, inf) if a >= b") {
    import spire.optional.intervalGeometricPartialOrder._
    import spire.algebra.{Order, PartialOrder}
    forAll { (a: Rational, w: NonNegative[Rational]) =>
      val b = a - w.num
      // a >= b
      val i = Interval.atOrBelow(a)
      val j = Interval.atOrAbove(b)
      i.partialCompare(j).isNaN shouldBe true
      j.partialCompare(i).isNaN shouldBe true
    }
  }

  property("(-inf, inf) does not compare with [a, b]") {
    import spire.optional.intervalGeometricPartialOrder._
    import spire.algebra.{Order, PartialOrder}
    forAll { (a: Rational, b: Rational) =>
      val i = Interval.all[Rational]
      val j = Interval.closed(a, b)
      i.partialCompare(j).isNaN shouldBe true
      j.partialCompare(i).isNaN shouldBe true
    }
  }

  property("empty intervals are equal") {
    forAll { (x: Rational, y: Rational) =>
      import spire.algebra.Eq
      val a = Interval.open(x, x)
      val b = Interval.open(y, y)
      val c = Interval.openUpper(x, x)
      val d = Interval.openLower(x, x)
      val e = Interval.empty[Rational]

      a shouldBe e
      a shouldBe b
      b shouldBe e
      c shouldBe e
      d shouldBe e
      e shouldBe e
      Eq[Interval[Rational]].eqv(a, e) shouldBe true
      Eq[Interval[Rational]].eqv(a, b) shouldBe true
      Eq[Interval[Rational]].eqv(b, e) shouldBe true
      Eq[Interval[Rational]].eqv(c, e) shouldBe true
      Eq[Interval[Rational]].eqv(d, e) shouldBe true
      Eq[Interval[Rational]].eqv(e, e) shouldBe true
    }
  }
}

class IntervalIteratorCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  property("bounded intervals are ok") {
    forAll { (n1: Rational, n2: Rational, num0: Byte) =>
      val (x, y) = if (n1 <= n2) (n1, n2) else (n2, n1)

      val num = ((num0 & 255) % 13) + 1

      def testEndpoints(interval: Interval[Rational], step: Rational, hasLower: Boolean, hasUpper: Boolean): Unit = {
        val ns = interval.iterator(step).toSet
        ns(x) shouldBe hasLower
        ns(y) shouldBe hasUpper
        val extra = if (hasLower && hasUpper) 2 else if (hasLower || hasUpper) 1 else 0
        ns.size shouldBe (num - 1 + extra)
      }

      val cc = Interval.closed(x, y)     // [x, y]
      val oo = Interval.open(x, y)       // (x, y)
      val oc = Interval.openLower(x, y)  // (x, y]
      val co = Interval.openUpper(x, y)  // [x, y)

      val step = (y - x) / num

      if (step.isZero) {
        List(cc, oo, oc, co).foreach { xs =>
          Try(xs.iterator(0)).isFailure shouldBe true
        }
      } else {
        val triples = List((cc, true, true), (oo, false, false), (oc, false, true), (co, true, false))
        triples.foreach { case (interval, hasLower, hasUpper) =>
          testEndpoints(interval, step, hasLower, hasUpper)
          testEndpoints(interval, -step, hasLower, hasUpper)
        }
      }
    }
  }

  property("half-unbound intervals are ok") {
    forAll { (n: Rational, s: Rational) =>

      val step0 = s.abs

      val cu = Interval.atOrAbove(n) // [n, ∞)
      val ou = Interval.above(n)     // (n, ∞)
      val uc = Interval.atOrBelow(n) // (-∞, n]
      val uo = Interval.below(n)     // (-∞, n)

      if (step0.isZero) {
        List(cu, ou, uc, uo).foreach { xs =>
          Try(xs.iterator(0)).isFailure shouldBe true
        }
      } else {
        val triples = List((cu, true, 1), (ou, false, 1), (uc, true, -1), (uo, false, -1))
        triples.foreach { case (interval, hasN, mult) =>
          val step = step0 * mult
          val it = interval.iterator(step)
          val expected = if (hasN) n else n + step
          it.next() shouldBe expected
          Try(interval.iterator(-step)).isFailure shouldBe true
        }
      }
    }
  }

  property("unbound intervals are not supported") {
    forAll { (step: Rational) =>
      Try(Interval.all[Rational].iterator(step)).isFailure shouldBe true
    }
  }
}

class IntervalOverlapCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  property("(x overlap y) = (y overlap x)") {
    forAll() { (x: Interval[Rational], y: Interval[Rational]) =>
      x.overlap(y) shouldBe y.overlap(x)
    }
  }

  property("x overlap x = Equals(x, x)") {
    forAll() { x: Interval[Rational] =>
      x.overlap(x) shouldBe Equals(x, x)
    }
  }

  property("(x overlap Ø) = Subset(Ø, x) id x != Ø") {
    forAll() { x: Interval[Rational] =>
      whenever(x.nonEmpty) {
        val empty = Interval.empty[Rational]
        x.overlap(empty) shouldBe Subset(empty, x)
      }
    }
  }

  property("consistency with Interval#isSubset") {
    forAll() { (x: Interval[Rational], y: Interval[Rational]) =>
      x.overlap(y).isSubset shouldBe (x.isSubsetOf(y) || y.isSubsetOf(x))
    }
  }

  property("(-inf, a] overlap [a, +inf) = LessAndOverlaps") {
    forAll() { (x: Rational) =>
      Interval.atOrBelow(x).overlap(Interval.atOrAbove(x)) shouldBe a[LessAndOverlaps[_]]
    }
  }

  property("[a, b) overlap (c, d] = LessAndOverlaps if a <= c < b <= d") {
    forAll() { (x: Rational, y: Rational, m: Rational, n: Rational) =>

      import spire.algebra.Order.ordering

      val sorted = List(x, y, m, n).sorted
      whenever(sorted(1) < sorted(2)) {
        Interval.openUpper(sorted(0), sorted(2)).overlap(Interval.openLower(sorted(1), sorted(3))).isLessAndOverlaps shouldBe true
      }
    }
  }

  property("[a, b] overlap [c, d] = LessAndOverlaps if a <= c <= b <= d") {
    forAll() { (x: Rational, y: Rational, m: Rational, n: Rational) =>

      import spire.algebra.Order.ordering

      val sorted = List(x, y, m, n).sorted
      Interval.closed(sorted(0), sorted(2)).overlap(Interval.closed(sorted(1), sorted(3))).isLessAndOverlaps shouldBe true
    }
  }

  property("(-inf, a) overlap (b, +inf) = LessAndOverlaps if a > b") {
    forAll() { (x: Rational, y: Rational) =>
      whenever(x != y) {
        Interval.below(max(x, y)).overlap(Interval.above(min(x, y))).isLessAndOverlaps shouldBe true
      }
    }
  }

  property("(-inf, a) overlap (b, +inf) = StrictlyLess if a <= b") {
    forAll() { (x: Rational, y: Rational) =>
      Interval.below(min(x, y)).overlap(Interval.above(max(x, y))).isStrictlyLess shouldBe true
    }
  }

  property("StrictlyLess((-inf, a), (b, +inf)).join = [a, b]") {
    forAll() { (x: Rational, y: Rational) =>
      val l = min(x, y)
      val u = max(x, y)
      StrictlyLess(Interval.below(l), Interval.above(u)).join shouldBe Interval.closed(l, u)
    }
  }

  property("[a, b) overlap (c, d] = StrictlyLess if a <= b <= c <= d") {
    forAll() { (x: Rational, y: Rational, m: Rational, n: Rational) =>

      import spire.algebra.Order.ordering

      val sorted = List(x, y, m, n).sorted
      val overlap = Interval.openUpper(sorted(0), sorted(1)).overlap(Interval.openLower(sorted(2), sorted(3)))
      overlap.isStrictlyLess shouldBe true
      overlap.asInstanceOf[StrictlyLess[Rational]].join shouldBe Interval.closed(sorted(1), sorted(2))
    }
  }

  property("[a, b] overlap [c, d] = StrictlyLess if a <= b < c <= d") {
    forAll() { (x: Rational, y: Rational, m: Rational, n: Rational) =>

      import spire.algebra.Order.ordering

      val sorted = List(x, y, m, n).sorted
      whenever(sorted(1) < sorted(2)) {
        val overlap = Interval.closed(sorted(0), sorted(1)).overlap(Interval.closed(sorted(2), sorted(3)))
        overlap.isStrictlyLess shouldBe true
        overlap.asInstanceOf[StrictlyLess[Rational]].join shouldBe Interval.open(sorted(1), sorted(2))
      }
    }
  }

  property("x overlap [a] is never a LessAndOverlaps") {
    forAll() { (x: Interval[Rational], b: Rational) =>
      x.overlap(Interval.point(b)).isLessAndOverlaps shouldNot be(true)
    }
  }
}
