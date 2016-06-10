package spire
package algebra

import spire.std.int._

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._

class EqTests extends FunSuite with Checkers {

  test("Eq.by")(check {
    final case class A(asInt: Int)
    val eqA: Eq[A] = Eq.by(_.asInt)
    forAll { (x: Int, y: Int) =>
      eqA.eqv(A(x), A(y)) === (x == y)
    }
  })
}
