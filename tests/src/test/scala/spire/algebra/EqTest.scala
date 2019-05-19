package spire
package algebra

import spire.std.int._

import org.scalacheck.Prop.forAll

import org.scalatest.check.Checkers
import org.scalatest.funsuite.AnyFunSuite

class EqTests extends AnyFunSuite with Checkers {

  test("Eq.by")(check {
    final case class A(asInt: Int)
    val eqA: Eq[A] = Eq.by(_.asInt)
    forAll { (x: Int, y: Int) =>
      eqA.eqv(A(x), A(y)) === (x == y)
    }
  })
}
