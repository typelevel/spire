package spire
package math

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

import spire.implicits._

class BitStringScalaCheckSuite extends munit.ScalaCheckSuite {
  property("operator mappings") {
    def byOp[A: BitString](n: A, i: Int): List[A] =
      List(n << i, n >>> i, n >> i)

    def byName[A](n: A, i: Int)(implicit bs: BitString[A]): List[A] =
      List(bs.leftShift(n, i), bs.rightShift(n, i), bs.signedRightShift(n, i))

    forAll { (n: Byte, i: Int) =>
      byOp(n, i) == byName(n, i)
    }
  }
}
