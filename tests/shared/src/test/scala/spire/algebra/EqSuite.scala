package spire
package algebra

import spire.std.int._

import org.scalacheck.Prop.forAll

class EqSuite extends munit.ScalaCheckSuite {

  property("Eq.by") {
    final case class A(asInt: Int)
    val eqA: Eq[A] = Eq.by(_.asInt)
    forAll { (x: Int, y: Int) =>
      eqA.eqv(A(x), A(y)) == (x == y)
    }
  }
}
