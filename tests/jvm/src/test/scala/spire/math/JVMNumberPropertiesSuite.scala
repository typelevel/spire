package spire
package math

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class JVMNumberPropertiesSuite extends munit.ScalaCheckSuite {
  def bothEq[A, B](a: A, b: B) = {
    a == b && b == a
  }

  property("RationalNumber == Double") {
    forAll { (n: Double) => bothEq(Number(Rational(n)), n) }
  }

}
