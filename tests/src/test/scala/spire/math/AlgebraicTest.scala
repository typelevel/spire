package spire.math

import spire.algebra.Sign

import org.scalatest.FunSuite
import java.math.MathContext


class AlgebraicTest extends FunSuite {
  def trickyZero: Algebraic = Algebraic(18).sqrt - Algebraic(8).sqrt - Algebraic(2).sqrt

  test("Sign of tricky zero is Zero") {
    assert(trickyZero.sign === Sign.Zero)
  }

  test("Relative approximation of zero is zero") {
    assert((Algebraic(0) approximateTo MathContext.DECIMAL128) === BigDecimal(0))
    assert(trickyZero.toDouble === 0.0)
  }

  test("Absolute approximation of addition is correct") {
    val sqrt2x100 = Iterator.fill(100)(Algebraic(2).sqrt) reduce (_ + _)
    val dblSqrt2x100 = math.sqrt(2) * 100

    val err = BigDecimal(0.0001)
  
    val approx = sqrt2x100 +/- err

    assert(approx - err <= dblSqrt2x100 && dblSqrt2x100 <= approx + err)
  }

  test("Relative approximation of addition is correct") {
    val sum = Iterator.fill(29)(Algebraic(1) / 29) reduce (_ + _)
    assert(sum.toDouble === 1.0)
    assert(sum.toBigDecimal === BigDecimal(1))
  }

  test("Absolute approximation of subtraction is correct") {
    val negSqrt2x98 = Iterator.fill(100)(Algebraic(2).sqrt) reduce (_ - _)
    val dblNegSqrt2x98 = -math.sqrt(2) * 98

    val err = BigDecimal(0.0001)
    val approx = negSqrt2x98 +/- err
    assert(approx - err <= dblNegSqrt2x98 && dblNegSqrt2x98 <= approx + err)
  }

  test("Absolute approximation of multiplication is correct") {
    val prod = Iterator.fill(32)(Algebraic(2).sqrt) reduce (_ * _)
    val err = BigDecimal(0.0001)

    val approx = prod +/- err
    val actual = BigDecimal(1 << 16)

    assert(actual - err <= approx && approx <= actual + err)
  }

  test("Relative approximation of multiplication is correct") {
    val prod = Iterator.fill(32)(Algebraic(2).sqrt) reduce (_ * _)

    val approx = prod approximateTo MathContext.DECIMAL64
    val actual = BigDecimal(1 << 16)

    assert(approx === actual)
  }

  test("Absolute approximation of division is correct") {
    val quot = Algebraic(2).sqrt / 2
    val actual = 0.7071067811865476
    val err = BigDecimal(0.0001)
    val approx = quot +/- err
    assert(actual - err <= approx && approx <= actual + err)
  }

  test("Relative approximation of division is correct") {
    
    // The bubble-up div transform will actually just transform this to
    // 65536 / 65536... so not sure how good of a test it is.

    val quot = Iterator.fill(16)(Algebraic(2)).foldLeft(Algebraic(1 << 16))(_ / _)
    assert(quot.toDouble === 1.0)

    val aThird = Algebraic(-1) / 3
    val actual = BigDecimal(-1, MathContext.DECIMAL128) / 3
    assert(aThird.toBigDecimal(MathContext.DECIMAL128) === actual)

    val aThird2 = Algebraic(1) / -3
    assert(aThird2.toBigDecimal(MathContext.DECIMAL128) === actual)
  }

  test("Absolute approximation of roots is correct") {
    val a = Algebraic(2).sqrt
    val err = BigDecimal(0.00001)
    val actual = BigDecimal(1.4142135623730951)
    val approx = a +/- err
    assert(actual - err <= approx && approx <= actual + err)

    val b = Algebraic(-4) nroot 3
    val bctual = BigDecimal(-1.5874010519681994) // give or take
    val bpprox = b +/- err
    assert(bctual - err <= bpprox && bpprox <= bctual + err)
  }

  test("Associativity with large and small numbers") {
    val x = Algebraic(1e308)
    val y = Algebraic(-1e308)
    val z = Algebraic(1)
    assert((x + (y + z)) === (x + y + z))
  }
}

