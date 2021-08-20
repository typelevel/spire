package spire
package math

import spire.implicits._

class CooperativeEqualitySuite extends munit.FunSuite {

  def testEquals[A, B](a: A, aname: String, b: B, bname: String) =
    if (aname == bname) {
      test(s"$aname == $aname") { assert(a == b) }
    } else {
      test(s"$aname == $bname") { assert(a == b) }
      test(s"$bname == $aname") { assert(b == a) }
    }

  // test Natural
  testEquals(3.toByte(), "Byte", Natural(3), "Natural")
  testEquals(3.toShort(), "Short", Natural(3), "Natural")
  testEquals(3, "Int", Natural(3), "Natural")
  testEquals(3L, "Long", Natural(3), "Natural")
  testEquals(3f, "Float", Natural(3), "Natural")
  testEquals(3d, "Double", Natural(3), "Natural")
  // testEquals(BigInt(3), "BigInt", Natural(3), "Natural")
  testEquals(Natural(3), "Natural", Natural(3), "Natural")

  // test SafeLong
  testEquals(3.toByte(), "Byte", SafeLong(3), "SafeLong")
  testEquals(3.toShort(), "Short", SafeLong(3), "SafeLong")
  testEquals(3, "Int", SafeLong(3), "SafeLong")
  testEquals(3L, "Long", SafeLong(3), "SafeLong")
  testEquals(3f, "Float", SafeLong(3), "SafeLong")
  testEquals(3d, "Double", SafeLong(3), "SafeLong")
  testEquals(Natural(3), "Natural", SafeLong(3), "SafeLong")
  // testEquals(BigInt(3), "BigInt", SafeLong(3), "SafeLong")
  testEquals(SafeLong(3), "SafeLong", SafeLong(3), "SafeLong")

  // test Rational
  testEquals(3.toByte(), "Byte", Rational(3), "Rational")
  testEquals(3.toShort(), "Short", Rational(3), "Rational")
  testEquals(3, "Int", Rational(3), "Rational")
  testEquals(3L, "Long", Rational(3), "Rational")
  testEquals(3f, "Float", Rational(3), "Rational")
  testEquals(3d, "Double", Rational(3), "Rational")
  testEquals(Natural(3), "Natural", Rational(3), "Rational")
  // testEquals(BigInt(3), "BigInt", Rational(3), "Rational")
  testEquals(SafeLong(3), "SafeLong", Rational(3), "Rational")
  // testEquals(BigDecimal(3), "BigDecimal", Rational(3), "Rational")
  testEquals(Rational(3), "Rational", Rational(3), "Rational")

  // test Number
  testEquals(3.toByte(), "Byte", Number(3), "Number")
  testEquals(3.toShort(), "Short", Number(3), "Number")
  testEquals(3, "Int", Number(3), "Number")
  testEquals(3L, "Long", Number(3), "Number")
  testEquals(3f, "Float", Number(3), "Number")
  testEquals(3d, "Double", Number(3), "Number")
  testEquals(Natural(3), "Natural", Number(3), "Number")
  // testEquals(BigInt(3), "BigInt", Number(3), "Number")
  testEquals(SafeLong(3), "SafeLong", Number(3), "Number")
  // testEquals(BigDecimal(3), "BigDecimal", Number(3), "Number")
  testEquals(Rational(3), "Rational", Number(3), "Number")
  testEquals(Number(3), "Number", Number(3), "Number")

  // test Algebraic
  testEquals(3.toByte(), "Byte", Algebraic(3), "Algebraic")
  testEquals(3.toShort(), "Short", Algebraic(3), "Algebraic")
  testEquals(3, "Int", Algebraic(3), "Algebraic")
  testEquals(3L, "Long", Algebraic(3), "Algebraic")
  testEquals(3f, "Float", Algebraic(3), "Algebraic")
  testEquals(3d, "Double", Algebraic(3), "Algebraic")
  testEquals(Natural(3), "Natural", Algebraic(3), "Algebraic")
  // testEquals(BigInt(3), "BigInt", Algebraic(3), "Algebraic")
  testEquals(SafeLong(3), "SafeLong", Algebraic(3), "Algebraic")
  // testEquals(BigDecimal(3), "BigDecimal", Algebraic(3), "Algebraic")
  testEquals(Rational(3), "Rational", Algebraic(3), "Algebraic")
  testEquals(Number(3), "Number", Algebraic(3), "Algebraic")
  testEquals(Algebraic(3), "Algebraic", Algebraic(3), "Algebraic")

  // test Real
  testEquals(3.toByte(), "Byte", Real(3), "Real")
  testEquals(3.toShort(), "Short", Real(3), "Real")
  testEquals(3, "Int", Real(3), "Real")
  testEquals(3L, "Long", Real(3), "Real")
  testEquals(3f, "Float", Real(3), "Real")
  testEquals(3d, "Double", Real(3), "Real")
  testEquals(Natural(3), "Natural", Real(3), "Real")
  // testEquals(BigInt(3), "BigInt", Real(3), "Real")
  testEquals(SafeLong(3), "SafeLong", Real(3), "Real")
  // testEquals(BigDecimal(3), "BigDecimal", Real(3), "Real")
  testEquals(Rational(3), "Rational", Real(3), "Real")
  testEquals(Number(3), "Number", Real(3), "Real")
  testEquals(Algebraic(3), "Algebraic", Real(3), "Real")
  testEquals(Real(3), "Real", Real(3), "Real")

  def testComplex[A: ConvertableFrom](a: A, name: String): Unit = {
    testEquals(a, name, Complex(a.toFloat()), "Complex[Float]")
    testEquals(a, name, Complex(a.toDouble()), "Complex[Double]")
    // testEquals(a, name, Complex(a.toBigDecimal), "Complex[BigDecimal]")
    testEquals(a, name, Complex(Real(a.toRational())), "Complex[Real]")
  }

  testComplex(3.toByte(), "Byte")
  testComplex(3.toShort(), "Short")
  testComplex(3, "Int")
  testComplex(3L, "Long")
  testComplex(3f, "Float")
  testComplex(3d, "Double")
  testComplex(Natural(3), "Natural")
  testComplex(SafeLong(3), "SafeLong")
  testComplex(Rational(3), "Rational")
  testComplex(Number(3), "Number")
  testComplex(Algebraic(3), "Algebraic")
  testComplex(Real(3), "Real")

  def testQuaternion[A: ConvertableFrom](a: A, name: String): Unit = {
    testEquals(a, name, Quaternion(a.toFloat()), "Quaternion[Float]")
    testEquals(a, name, Quaternion(a.toDouble()), "Quaternion[Double]")
    // testEquals(a, name, Quaternion(a.toBigDecimal), "Quaternion[BigDecimal]")
    testEquals(a, name, Quaternion(Real(a.toRational())), "Quaternion[Real]")
  }

  testQuaternion(3.toByte(), "Byte")
  testQuaternion(3.toShort(), "Short")
  testQuaternion(3, "Int")
  testQuaternion(3L, "Long")
  testQuaternion(3f, "Float")
  testQuaternion(3d, "Double")
  testQuaternion(Natural(3), "Natural")
  testQuaternion(SafeLong(3), "SafeLong")
  testQuaternion(Rational(3), "Rational")
  testQuaternion(Number(3), "Number")
  testQuaternion(Algebraic(3), "Algebraic")
  testQuaternion(Real(3), "Real")
}
