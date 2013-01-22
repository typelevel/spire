package test.scala.spire.math

import org.scalatest.FunSuite

import spire.math._
import spire.implicits.{eqOps => _, _}

class ConversionTest extends FunSuite {
  test("byte can be widened") {
    import WideningConversion._
    assert((1: Byte).widen[Byte] === (1: Byte))
    assert((1: Byte).widen[Short] === (1: Short))
    assert((-128: Byte).widen[Short] === (-128: Short))
    assert((123: Byte).widen[Int] === 123)
    assert((123: Byte).widen[Long] === 123L)
    assert((11: Byte).widen[Float] === 11f)
    assert((11: Byte).widen[Double] === 11d)
    assert((11: Byte).widen[BigInt] === BigInt(11))
    assert((11: Byte).widen[BigDecimal] === BigDecimal(11))
    assert((11: Byte).widen[Rational] === Rational(11))
    assert((11: Byte).widen[Real] === Real(11))
  }

  test("short can be widened") {
    assert((-1234: Short).widen[Short] === (-1234: Short))
    assert((829: Short).widen[Int] === 829)
    assert((18234: Short).widen[Long] === 18234L)
    assert((11: Short).widen[Float] === 11f)
    assert((11: Short).widen[Double] === 11d)
    assert((11: Short).widen[BigInt] === BigInt(11))
    assert((11: Short).widen[BigDecimal] === BigDecimal(11))
    assert((11: Short).widen[Rational] === Rational(11))
    assert((11: Short).widen[Real] === Real(11))
  }

  test("int can be widened") {
    assert(1.widen[Int] === 1)
    assert((-19234).widen[Long] === -19234L)
    assert(Int.MaxValue.widen[Long] === Int.MaxValue.toLong)
    assert(Int.MinValue.widen[Long] === Int.MinValue.toLong)
    // compile error I hope: 12.widen[Float]
    assert(12.widen[Double] === 12d)
    assert(Int.MaxValue.widen[Double] === Int.MaxValue.toDouble)
    assert(0.widen[BigInt] === BigInt(0))
    assert(Int.MinValue.widen[BigInt] === BigInt(Int.MinValue))
    assert(Int.MinValue.widen[BigDecimal] === BigDecimal(Int.MinValue))
    assert(Int.MaxValue.widen[Rational] === Rational(Int.MaxValue))
    assert(Int.MaxValue.widen[Real] === Real(Int.MaxValue))
  }

  test("long can be widened") {
    assert((-19234L).widen[Long] === -19234L)
    // Compile error: assert(12.widen[Double] === 12d)
    assert(0.widen[BigInt] === BigInt(0))
    assert(Long.MinValue.widen[BigInt] === BigInt(Long.MinValue))
    assert(Long.MinValue.widen[BigDecimal] === BigDecimal(Long.MinValue))
    assert(Long.MaxValue.widen[Rational] === Rational(Long.MaxValue))
    assert(Long.MaxValue.widen[Real] === Real(Long.MaxValue))
  }

  test("BigInt can be widened") {
    assert(BigInt(0).widen[BigDecimal] === BigDecimal(0))
    assert(BigInt(Long.MaxValue).widen[BigDecimal] === BigDecimal(Long.MaxValue))
    assert(BigInt(-123456).widen[Rational] === Rational(-123456))
    assert(BigInt(123456).widen[Real] === Real(123456))
  }

  test("BigDecimal can be widened") {
    assert(BigDecimal("0.5").widen[Rational] === Rational(1, 2))
    assert(BigDecimal("123.235").widen[Rational] === Rational(BigDecimal("123.235")))
    assert(BigDecimal(0).widen[Real] === Real(0))
  }

  test("Rational can be widened") {
    assert(Rational(3, 4).widen[Rational] === Rational(3, 4))
    assert(Rational(1, 2).widen[Real] === Real(0.5))
  }

  test("Real can widen to itself") {
    assert(Real(3).widen[Real] === Real(3))
  }

  test("Can narrow if widen available") {
    def narrowFromWiden[A, B](a: A)(implicit c: WideningConversion[A, B]): B = {
      a.narrow[B]
    }

    assert(narrowFromWiden[Int, Long](1) === 1L)
  }

  test("Real can be narrowed") {
    implicit val mc = java.math.MathContext.DECIMAL128

    assert(Real(0.5).narrow[Rational] === Rational(1, 2))
    assert(Real(-0.5).narrow[BigDecimal] === BigDecimal(-0.5))
    assert(Real(42.5).narrow[BigInt] === BigInt(42))
    assert(Real(12.25).narrow[Double] === 12.25)
  }

  test("Rational can be narrowed") {
    assert(Rational(1, 2).narrow[Double] === 0.5)
    assert(Rational(11, 2).narrow[Int] === 5)
  }

  test("BigDecimal can be narrowed") {
    assert(BigDecimal(2.3).narrow[Double] === 2.3)
  }

  test("BigInt can be narrowed") {
    assert(BigInt("192938188192928828391919923941").narrow[Int] === Int.MaxValue)
    assert(BigInt(1).narrow[Long] === 1L)
  }

  test("Long can be narrowed") {
    assert(Long.MaxValue.narrow[Double] === 9.223372036854776E18)
    assert((Long.MinValue).narrow[Int] === Int.MinValue)
  }

  test("Int can be narrowed") {
    assert(Int.MaxValue.narrow[Float] === 2.14748365E9f)
    assert(Int.MaxValue.narrow[Short] === Short.MaxValue)
    assert(Int.MinValue.narrow[Short] === Short.MinValue)
    assert(128.narrow[Byte] === (127: Byte))
  }

  test("Short can be narrowed") {
    assert((1: Short).narrow[Byte] === (1: Byte))
  }
}

