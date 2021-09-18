package spire
package math

import spire.math.ArbitrarySupport.{NonNegative, Positive}

import spire.implicits._
import spire.laws.arb.{interval => interval_, rational}

import org.scalacheck.Prop._

class IntervalScalaCheckSuite extends munit.ScalaCheckSuite {

  property("x ⊆ x") {
    forAll { (x: Interval[Rational]) => x.isSupersetOf(x) }
  }

  property("x ⊆ (x | y) && y ⊆ (x | y)") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      val z = x | y
      (z.isSupersetOf(x)) &&
      (z.isSupersetOf(y))
    }
  }

  property("(x & y) ⊆ x && (x & y) ⊆ y") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      val z = x & y
      (x.isSupersetOf(z)) &&
      (y.isSupersetOf(z))
    }
  }

  val rng = spire.random.GlobalRng

  property("(x -- y) ⊆ x && (x -- y) & y = Ø") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      (x -- y).foreach { zi =>
        (zi.isSubsetOf(x)) &&
        (zi.intersects(y))
      }
    }
  }

  property("(x -- Ø) = x") {
    forAll { (x: Interval[Rational]) =>
      if (x.nonEmpty) {
        (x -- Interval.empty[Rational]) == List(x)
      } else true
    }
  }

  property("(x -- x) = Ø") {
    forAll { (x: Interval[Rational]) =>
      (x -- x) == Nil
    }
  }

  property("(x -- (-∞, ∞)) = Ø") {
    forAll { (x: Interval[Rational]) =>
      (x -- Interval.all[Rational]) == Nil
    }
  }

  def sample(int: Interval[Rational], n: Int): Array[Rational] =
    if (int.isEmpty) {
      Array.empty[Rational]
    } else {
      import spire.math.interval.ValueBound
      val underlyingf: () => Rational = (int.lowerBound, int.upperBound) match {
        case (ValueBound(x), ValueBound(y)) =>
          () =>
            rng.nextInt(10) match {
              case 0 => x
              case 9 => y
              case _ => x + Rational(rng.nextDouble) * (y - x)
            }
        case (ValueBound(x), _) =>
          () =>
            rng.nextInt(5) match {
              case 0 => x
              case _ => x + (Rational(rng.nextGaussian).abs * Long.MaxValue)
            }
        case (_, ValueBound(y)) =>
          () =>
            rng.nextInt(5) match {
              case 4 => y
              case _ => y - (Rational(rng.nextGaussian).abs * Long.MaxValue)
            }
        case (_, _) => () => Rational(rng.nextGaussian) * Long.MaxValue
      }

      def nextf(): Rational = {
        val r = underlyingf()
        if (int.contains(r)) r else nextf()
      }

      Array.fill(n)(nextf())
    }

  val tries = 100

  def testUnop(f: Interval[Rational] => Interval[Rational])(g: Rational => Rational) = {
    forAll { (a: Interval[Rational]) =>
      val c: Interval[Rational] = f(a)
      sample(a, tries).foreach { x =>
        val ok = c.contains(g(x))
        if (!ok) println("%s failed on %s".format(a, x.toString))
        ok
      }
    }
  }

  def testBinop(
    f: (Interval[Rational], Interval[Rational]) => Interval[Rational]
  )(g: (Rational, Rational) => Rational) = {
    forAll { (a: Interval[Rational], b: Interval[Rational]) =>
      val c: Interval[Rational] = f(a, b)
      sample(a, tries).zip(sample(b, tries)).foreach { case (x, y) =>
        if (!a.contains(x)) println("%s does not contain %s".format(a, x))
        if (!b.contains(y)) println("%s does not contain %s".format(b, y))
        val ok = c.contains(g(x, y))
        if (!ok) println("(%s, %s) failed on (%s, %s)".format(a, b, x.toString, y.toString))
        ok
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
      Interval(x.toString) == x
    }
  }

  property("points compare as scalars") {
    import spire.optional.intervalGeometricPartialOrder._

    import spire.algebra.{Order, PartialOrder}
    forAll { (x: Rational, y: Rational) =>
      val a = Interval.point(x)
      val b = Interval.point(y)
      val order = PartialOrder[Interval[Rational]].tryCompare(a, b).get == Order[Rational].compare(x, y)
      val min = a.pmin(b) match {
        case Some(Point(vmin)) => vmin == x.min(y)
        case _                 => false
      }
      val max = a.pmax(b) match {
        case Some(Point(vmax)) => vmax == x.max(y)
        case _                 => false
      }
      order && min && max
    }
  }

  property("(-inf, a] < [b, inf) if a < b") {
    import spire.optional.intervalGeometricPartialOrder._

    forAll { (a: Rational, w: Positive[Rational]) =>
      val b = a + w.num
      // a < b
      val i = Interval.atOrBelow(a)
      val j = Interval.atOrAbove(b)
      (i < j) &&
      !(i >= j) &&
      (j > i) &&
      !(j <= i)
    }
  }

  property("(-inf, a] does not compare to [b, inf) if a >= b") {
    import spire.optional.intervalGeometricPartialOrder._
    forAll { (a: Rational, w: NonNegative[Rational]) =>
      val b = a - w.num
      // a >= b
      val i = Interval.atOrBelow(a)
      val j = Interval.atOrAbove(b)
      i.partialCompare(j).isNaN &&
      j.partialCompare(i).isNaN
    }
  }

  property("(-inf, inf) does not compare with [a, b]") {
    import spire.optional.intervalGeometricPartialOrder._
    forAll { (a: Rational, b: Rational) =>
      val i = Interval.all[Rational]
      val j = Interval.closed(a, b)
      i.partialCompare(j).isNaN &&
      j.partialCompare(i).isNaN
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

      a == e &&
      a == b &&
      b == e &&
      c == e &&
      d == e &&
      e == e &&
      Eq[Interval[Rational]].eqv(a, e) &&
      Eq[Interval[Rational]].eqv(a, b) &&
      Eq[Interval[Rational]].eqv(b, e) &&
      Eq[Interval[Rational]].eqv(c, e) &&
      Eq[Interval[Rational]].eqv(d, e) &&
      Eq[Interval[Rational]].eqv(e, e)
    }
  }
}
