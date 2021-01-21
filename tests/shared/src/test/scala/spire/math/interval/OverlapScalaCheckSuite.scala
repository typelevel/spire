package spire.math.interval

import spire.laws.arb._
import spire.math.{Interval, Rational}
import spire.math.interval.Overlap._
import spire.syntax.eq._
import org.scalacheck.Prop._

class OverlapScalaCheckSuite extends munit.ScalaCheckSuite {

  property("Overlap equality") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      (x =!= y) ==> {
        import spire.algebra.Eq

        val eq = Eq[Overlap[Rational]]

        eq.eqv(Equal(), Equal()) &&
        eq.eqv(Disjoint(x, y), Disjoint(x, y)) &&
        !eq.eqv(Disjoint(x, y), Disjoint(y, x)) &&
        eq.eqv(PartialOverlap(x, y), PartialOverlap(x, y)) &&
        !eq.eqv(PartialOverlap(x, y), PartialOverlap(y, x)) &&
        eq.eqv(Subset(x, y), Subset(x, y)) &&
        !eq.eqv(Subset(x, y), Subset(y, x))
      }
    }
  }
}
