package spire.math.interval

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import spire.laws.arb._
import spire.syntax.eq._
import spire.math.Rational

class BoundTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

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
