package spire.math.interval

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import spire.laws.arb._
import spire.syntax.eq._
import spire.math.Rational
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class BoundTest extends AnyPropSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  property("Bound equality") {
    forAll { (x: Rational, y: Rational) =>
      whenever(x =!= y) {
        import spire.algebra.Eq

        val eq = Eq[Bound[Rational]]

        eq.eqv(EmptyBound(), EmptyBound()) shouldBe true
        eq.eqv(Unbound(), Unbound()) shouldBe true

        eq.eqv(Open(x), Open(x)) shouldBe true
        eq.eqv(Open(x), Open(y)) shouldBe false

        eq.eqv(Closed(x), Closed(x)) shouldBe true
        eq.eqv(Closed(x), Closed(y)) shouldBe false
      }
    }
  }
}
