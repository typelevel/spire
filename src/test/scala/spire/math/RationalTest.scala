package spire.math

import org.scalatest.FunSuite

import scala.util.Random


class RationalTest extends FunSuite {

  test("rational canonical construction") {
    val r = Rational(5,6)
    assert(r.numerator === BigInt(5))
    assert(r.denominator === BigInt(6))
  }
  test("rational degenerate construction") {
    val r = Rational(30, 345)
    assert(r.numerator === BigInt(2))
    assert(r.denominator === BigInt(23))
  }

  test("RationalIsFractional implicit exists") {
    import Implicits._
    def doStuff[NT](a: NT, b: NT)(implicit n: Fractional[NT]) = {
      a / b    // Note: a & b are of unknown type NT, not Rational
    }

    expect(Rational(1, 2)) {
      doStuff(Rational(1), Rational(2))
    }
  }

  test("equality of equivalent canonical and degenerate rationals") {
    val a = Rational(1, 2)
    val b = Rational(8, 16)
    assert(a === b)
  }

  test("non-equivalent rationals are not equal") {
    val a = Rational(1, 2)
    val b = Rational(1, 3)
    val c = Rational(2, 1)
    
    expect(false)(a == b)
    expect(false)(a == c)
  }
  
  test("comparisons") {
    val a = Rational(1, 2)
    val b = Rational(3, 4)
    val c = Rational(-1, 2)
    val d = Rational(1, 2)
    assert(a < b)
    assert(b > a)
    assert(a > c)
    assert(c < a)
    assert(a <= d)
    assert(a >= d)
  }
     
  test("primitive comparisons") {
    val a = Rational("5000000000")
    val b = Rational(-123456)
    val c = Rational(1, 8)
    assert(a === 5000000000L)
    assert(5000000000L === a)
    assert(b === -123456)
    assert(-123456 === b)
    assert(c === 0.125)
    assert(0.125 === c)
    assert(c === 0.125f)
    assert(0.125f === c)
  }

  test("addition") {
    val a = Rational(3, 10)
    val b = Rational(4, 19)

    // This will go through the coprime denominator path.
    // Since, 97 and 190 are coprime, 97/190 is canonical too.
    expect(Rational(97, 190)) {
      a + b
    }

    val c = Rational(1, 2)
    val d = Rational(1, 6)

    // This will go through the non-coprime denominator path. Since the
    // GCD of 2 and 6 is 2, the numerator 1 * 3 + 1 * 1 = 4 is tried first.
    // The GCD of 4 and 2 is 2, so the numerator will need to be reduced.
    expect(Rational(1 * 6 + 1 * 2, 2 * 6)) {
      c + d
    }

    val e = Rational(1, 2)
    val f = Rational(3, 4)
    
    // This will go through the non-coprime denominator path. Since the
    // GCD of 2 and 4 is 2, the numerator 5 is tried first, which is
    // coprime with 2, so the numerator need not be reduced.
    expect(Rational(1 * 4 + 3 * 2, 2 * 4)) {
      e + f
    }
  }

  test("subtraction") {
    // Just ripped from addition
    val a = Rational(3, 10)
    val b = Rational(4, 19)
    expect(Rational(3 * 19 - 4 * 10, 10 * 19)) {
      a - b
    }

    val c = Rational(1, 2)
    val d = Rational(1, 6)
    expect(Rational(1 * 6 - 1 * 2, 2 * 6)) {
      c - d
    }

    val e = Rational(1, 2)
    val f = Rational(3, 4)
    expect(Rational(1 * 4 - 3 * 2, 2 * 4)) {
      e - f
    }
  }

  test("multiplication") {
    val a = Rational(2, 3)
    val b = Rational(1, 2)
    expect(Rational(1, 3)) {
      a * b
    }

    val c = Rational(-321, 23)
    val d = Rational(23, 13)
    expect(Rational(-321 * 23, 23 * 13)) {
      c * d
    }

    val e = Rational(-1, 2)
    expect(Rational(1, 4)) {
      e * e
    }
  }

  test("division") {
    val a = Rational(2, 3)
    val b = Rational(1, 2)
    expect(Rational(4, 3)) {
      a / b
    }

    val c = Rational(-21, 5)
    val d = Rational(7, 18)
    expect(Rational(-54, 5)) {
      c / d
    }

    val e = Rational(-23, 19)
    expect(Rational.one) {
      e / e
    }
  }

  test("division by 0") {
    // Should this be an ArithmeticException?
    intercept[IllegalArgumentException] {
      Rational.one / 0
    }
  }

    test("pow") {
        val a = Rational(1, 2)
    expect(Rational(1, BigInt("4294967296"))) {
      a pow 32
    }

        val b = Rational(-3, 1)
    expect(Rational(9, 1)) {
      b pow 2
    }
    expect(Rational(-27, 1)) {
      b pow 3
    }
    }

    test("longValue") { assert(Rational("5000000000").toLong === 5000000000L) }
    test("intValue") {
        assert(Rational(3).toInt === 3)
        assert(Rational(-5, 2).toInt === -2)
    }
    test("shortValue") {
        assert(Rational(65535).toShort === -1)
        assert(Rational(65536).toShort === 0)
        assert(Rational(-5).toShort === -5)
    }
    test("byteValue") {
        assert(Rational(-1).toByte === -1)
        assert(Rational(256).toByte === 0)
    }
    test("toDouble and tFloat") {
        assert(Rational(1, 2).toFloat === 0.5f)
        val a = Rational("10000000000000002/10000000000000000")
        assert(a.toDouble === 1.0000000000000002)
        assert(a.toFloat === 1.0f)
    assert(Rational(2, 3).toDouble === 2 / 3.0)
    }

  test("toString") {
    assert(Rational(1, 2).toString === "1/2")
    assert(Rational(1, -2).toString === "-1/2")
    assert(Rational(2, 4).toString === "1/2")
  }

  test("hashCode is the same for equivalent rats") {
    assert(Rational(1, 2).hashCode === Rational(2, 4).hashCode)
    assert(Rational(0).hashCode === Rational(0, 5).hashCode)
    assert(Rational(-1, 2).hashCode === Rational(1, -2).hashCode)
  }

  test("reverse primitive equality") {
    assert(1 == Rational.one)
    assert(-23L == Rational(-23L, 1L))
  }

  test("limiting 0 to any number returns 0") {
    assert(Rational.zero.limitDenominatorTo(1234) === Rational.zero)
    assert(Rational.zero.limitDenominatorTo(1) === Rational.zero)
    assert(Rational.zero.limitTo(23) === Rational.zero)
  }

  test("limiting to non-positive number throws exception") {
    intercept[IllegalArgumentException] {
      val a = Rational(123, 456).limitDenominatorTo(-1)
    }
    
    intercept[IllegalArgumentException] {
      val a = Rational(123, 456).limitTo(-1)
    }
  }

  /**
   * Finds the closest `Rational` to `a` whose denominator is no greater than
   * `limit` by brute-force. This is used to compare with the version used by
   * `Rational` which is a little harder to reason about. This just literally
   * tries every denominator between 1 and `limit` and returns the `Rational`
   * that was closest to `a`.
   */
  def bruteForceLimitDen(a: Rational, limit: Int): Rational =
    (1 to limit) map (BigInt(_)) flatMap { d =>
      val ln = (a * d).toBigInt
      List(Rational(ln - 1, d), Rational(ln, d), Rational(ln + 1, d))
    } minBy (b => (b - a).abs)

  test("limitDenominatorTo valid number returns correct result") {
    assert(Rational(6, 5) === Rational(23, 19).limitDenominatorTo(10))
    assert(Rational(-6, 5) === Rational(-23, 19).limitDenominatorTo(10))

    val rng = new Random(9281)
    val rationals = List.fill(100)(Rational(rng.nextInt, rng.nextInt))
    rationals foreach { a =>
      assert(a.limitDenominatorTo(255) === bruteForceLimitDen(a, 255), {
          "%s != %s (original: %s)" format (
            a.limitDenominatorTo(255),
            bruteForceLimitDen(a, 255),
            a
          )
        })
    }
  }

  test("limit large number to small number returns small number") {
    assert(Rational(1231, 2).limitTo(12) === Rational(12))
    assert(Rational(-321, 3).limitTo(7) === Rational(-7))
  }

  test("limitToInt makes rationals fit in Ints") {
    val rng = new Random(2919234)
    val rationals = List.fill(100)(Rational(BigInt(128, rng), BigInt(128, rng)))
    rationals foreach { a =>
      val b = a.limitToInt
      assert(b.numerator.isValidInt && b.denominator.isValidInt, 
        "%s (from %s) doesn't fit in Ints" format (b.toString, a.toString))
    }
  }
}
