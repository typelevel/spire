package spire.math.interval

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import spire.laws.arb._
import spire.math.{Interval, Rational}
import spire.syntax.eq._

class OverlapTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  property("Overlap equality") {
    forAll { (x: Interval[Rational], y: Interval[Rational]) =>
      whenever(x =!= y) {
        import spire.algebra.Eq

        val eq = Eq[Overlap[Rational]]

        eq.eqv(Equals(x, y), Equals(x, y)) shouldBe true
        eq.eqv(Equals(x, y), Equals(y, x)) shouldBe false

        eq.eqv(StrictlyLess(x, y), StrictlyLess(x, y)) shouldBe true
        eq.eqv(StrictlyLess(x, y), StrictlyLess(y, x)) shouldBe false

        eq.eqv(LessAndOverlaps(x, y), LessAndOverlaps(x, y)) shouldBe true
        eq.eqv(LessAndOverlaps(x, y), LessAndOverlaps(y, x)) shouldBe false

        eq.eqv(Subset(x, y), Subset(x, y)) shouldBe true
        eq.eqv(Subset(x, y), Subset(y, x)) shouldBe false
      }
    }
  }
}
