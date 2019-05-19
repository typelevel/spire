package spire.math

import spire.algebra.DivisionRing
import org.scalatest.funsuite.AnyFunSuite

class QuaternionTest extends AnyFunSuite {

  test("Quaternion[Double].fromDouble") {
    assert(DivisionRing[Quaternion[Rational]].fromDouble(0).isZero)
    assert((-DivisionRing[Quaternion[Rational]].fromDouble(-1)).isValidInt)
    assert(DivisionRing[Quaternion[Rational]].fromDouble(1) === DivisionRing[Quaternion[Rational]].one)
  }

}
