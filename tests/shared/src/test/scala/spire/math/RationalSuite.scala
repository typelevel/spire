package spire
package math

import scala.util.Random

class RationalSuite extends munit.FunSuite {

  test("rational canonical construction") {
    val r = Rational(5, 6)
    assert(r.numerator == BigInt(5))
    assert(r.denominator == BigInt(6))
    intercept[IllegalArgumentException] {
      Rational(1, 0)
    }
    intercept[IllegalArgumentException] {
      Rational(BigInt(1), 0)
    }
  }
  test("rational degenerate construction") {
    val r = Rational(30, 345)
    assert(r.numerator == BigInt(2))
    assert(r.denominator == BigInt(23))
  }
  test("rational parse") {
    intercept[NumberFormatException] {
      Rational("x")
    }
  }

  test("RationalIsFractional implicit exists") {
    import spire.implicits._
    def doStuff[NT: Fractional](a: NT, b: NT): NT = a / b

    assertEquals(Rational(1, 2), {
                   doStuff(Rational(1), Rational(2))
                 }
    )
  }

  test("equality of equivalent canonical and degenerate rationals") {
    val a = Rational(1, 2)
    val b = Rational(8, 16)
    assertEquals(a, b)
  }

  test("non-equivalent rationals are not equal") {
    val a = Rational(1, 2)
    val b = Rational(1, 3)
    val c = Rational(2, 1)

    assert(!(a == b))
    assert(!(a == c))
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
    assert(a == 5000000000L)
    assert(5000000000L == a)
    assert(b == -123456)
    assert(-123456 == b)
    assert(c == 0.125)
    assert(0.125 == c)
    assert(c == 0.125f)
    assert(0.125f == c)
  }

  test("addition") {
    val a = Rational(3, 10)
    val b = Rational(4, 19)

    // This will go through the coprime denominator path.
    // Since, 97 and 190 are coprime, 97/190 is canonical too.
    assertEquals(Rational(97, 190), a + b)

    val c = Rational(1, 2)
    val d = Rational(1, 6)

    // This will go through the non-coprime denominator path. Since the
    // GCD of 2 and 6 is 2, the numerator 1 * 3 + 1 * 1 = 4 is tried first.
    // The GCD of 4 and 2 is 2, so the numerator will need to be reduced.
    assertEquals(Rational(1 * 6 + 1 * 2, 2 * 6), c + d)

    val e = Rational(1, 2)
    val f = Rational(3, 4)

    // This will go through the non-coprime denominator path. Since the
    // GCD of 2 and 4 is 2, the numerator 5 is tried first, which is
    // coprime with 2, so the numerator need not be reduced.
    assertEquals(Rational(1 * 4 + 3 * 2, 2 * 4), e + f)
  }

  test("subtraction") {
    // Just ripped from addition
    val a = Rational(3, 10)
    val b = Rational(4, 19)
    assertEquals(Rational(3 * 19 - 4 * 10, 10 * 19), a - b)

    val c = Rational(1, 2)
    val d = Rational(1, 6)
    assertEquals(Rational(1 * 6 - 1 * 2, 2 * 6), c - d)

    val e = Rational(1, 2)
    val f = Rational(3, 4)
    assertEquals(Rational(1 * 4 - 3 * 2, 2 * 4), e - f)
  }

  test("multiplication") {
    val a = Rational(2, 3)
    val b = Rational(1, 2)
    assertEquals(Rational(1, 3), a * b)

    val c = Rational(-321, 23)
    val d = Rational(23, 13)
    assertEquals(Rational(-321 * 23, 23 * 13), c * d)

    val e = Rational(-1, 2)
    assertEquals(Rational(1, 4), e * e)
  }

  test("division") {
    val a = Rational(2, 3)
    val b = Rational(1, 2)
    assertEquals(Rational(4, 3), a / b)

    val c = Rational(-21, 5)
    val d = Rational(7, 18)
    assertEquals(Rational(-54, 5), c / d)

    val e = Rational(-23, 19)
    assertEquals(Rational.one, e / e)
  }

  test("division by 0") {
    intercept[ArithmeticException] {
      Rational.one / 0
    }
    intercept[ArithmeticException] {
      Rational.zero.reciprocal
    }
  }

  test("pow") {
    val a = Rational(1, 2)
    assertEquals(Rational(1, BigInt("4294967296")), a.pow(32))
    assertEquals(Rational(2, 1), a.pow(-1))
    val b = Rational(-3, 1)
    assertEquals(Rational.one, b.pow(0))
    assertEquals(Rational(9, 1), b.pow(2))
    assertEquals(Rational(-27, 1), b.pow(3))
    val l = Rational(Long.MaxValue) * 2
    assertEquals(Rational.one, l.pow(0))
    assertEquals(l.reciprocal, l.pow(-1))
  }

  test("longValue") { assertEquals(Rational("5000000000").toLong, 5000000000L) }
  test("intValue") {
    assertEquals(Rational(3).toInt, 3)
    assertEquals(Rational(-5, 2).toInt, -2)
  }
  test("shortValue") {
    assertEquals(Rational(65535).toShort, -1.toShort)
    assertEquals(Rational(65536).toShort, 0.toShort)
    assertEquals(Rational(-5).toShort, -5.toShort)
  }
  test("byteValue") {
    assertEquals(Rational(-1).toByte, -1.toByte)
    assertEquals(Rational(256).toByte, 0.toByte)
  }
  test("toDouble and tFloat") {
    assertEquals(Rational(1, 2).toFloat, 0.5f)
    val a = Rational("10000000000000002/10000000000000000")
    assertEquals(a.toDouble, 1.0000000000000002)
    assertEquals(a.toFloat, 1.0f)
    assertEquals(Rational(2, 3).toDouble, 2 / 3.0)
  }

  test("toString") {
    assertEquals(Rational(1, 2).toString, "1/2")
    assertEquals(Rational(1, -2).toString, "-1/2")
    assertEquals(Rational(2, 4).toString, "1/2")
  }

  test("hashCode is the same for equivalent rats") {
    assertEquals(Rational(1, 2).hashCode, Rational(2, 4).hashCode)
    assertEquals(Rational(0).hashCode, Rational(0, 5).hashCode)
    assertEquals(Rational(-1, 2).hashCode, Rational(1, -2).hashCode)
  }

  test("reverse primitive equality") {
    assert(1 == Rational.one)
    //assertEquals(-23L, Rational(-23L, 1L))
  }

  test("limiting 0 to any number returns 0") {
    assertEquals(Rational.zero.limitDenominatorTo(1234), Rational.zero)
    assertEquals(Rational.zero.limitDenominatorTo(1), Rational.zero)
    assertEquals(Rational.zero.limitTo(23), Rational.zero)
  }

  test("limiting to non-positive number throws exception") {
    intercept[IllegalArgumentException] {
      Rational(123, 456).limitDenominatorTo(-1)
    }

    intercept[IllegalArgumentException] {
      Rational(123, 456).limitTo(-1)
    }
  }

  /**
   * Finds the closest `Rational` to `a` whose denominator is no greater than `limit` by brute-force. This is used to
   * compare with the version used by `Rational` which is a little harder to reason about. This just literally tries
   * every denominator between 1 and `limit` and returns the `Rational` that was closest to `a`.
   */
  def bruteForceLimitDen(a: Rational, limit: Int): Rational =
    (1 to limit)
      .map(BigInt(_))
      .flatMap { d =>
        val ln = (a * d).toBigInt
        List(Rational(ln - 1, d), Rational(ln, d), Rational(ln + 1, d))
      }
      .minBy(b => (b - a).abs)

  // FIXME: for some reason the commented files seem to throw SBT/scalac into
  // some kind of continuous compilcation loop... YMMV :/
  test("limitDenominatorTo valid number returns correct result") {
    assertEquals(Rational(6, 5), Rational(23, 19).limitDenominatorTo(10))
    //assertEquals(Rational(-6, 5), Rational(-23, 19).limitDenominatorTo(10))

    val rng = new Random(9281)
    val rationals = List.fill(100)(Rational(rng.nextInt(), rng.nextInt().abs + 1))
    rationals.foreach { a =>
      //assertEquals(a.limitDenominatorTo(255), bruteForceLimitDen(a, 255), {
      //    "%s != %s (original: %s)" format (
      //      a.limitDenominatorTo(255),
      //      bruteForceLimitDen(a, 255),
      //      a
      //    )
      //})
    }
  }

  test("limit large number to small number returns small number") {
    assertEquals(Rational(1231, 2).limitTo(12), Rational(12))
    assertEquals(Rational(-321, 3).limitTo(7), Rational(-7))
  }

  test("limitToInt makes rationals fit in Ints") {
    val rng = new Random(2919234)
    val rationals = List.fill(100)(Rational(BigInt(128, rng), BigInt(128, rng).abs + 1))
    rationals.foreach { a =>
      val b = a.limitToInt
      assert(b.numerator.isValidInt && b.denominator.isValidInt,
             "%s (from %s) doesn't fit in Ints".format(b.toString, a.toString)
      )
    }
  }

  test("Rational(1).limitToInt returns 1") {
    assertEquals(Rational(1).limitToInt, Rational(1))
  }

  /* TODO: have a formal definition of Rational GCD and test it
  test("gcd returns the correct rational GCD") {
    assertEquals(Rational(1, 2).gcd(Rational(1, 3)), Rational(1, 6))
    assertEquals(Rational(11, 12).gcd(Rational(43, 22)), Rational(1, 132))
    assertEquals(Rational(-1, 2).gcd(Rational(1, 3)), Rational(1, 6))
    assertEquals(Rational(11, 12).gcd(Rational(-43, 22)), Rational(1, 132))
    assertEquals(Rational(11, 12).gcd(Rational(-43, 22)), Rational(1, 132))
    val x = Rational("1234123412341234/87658765876587658764")
    val y = Rational("1919191919191919191919/373737373737373737")
    val z = Rational("1/287380324068203382157064120376241062")
    assertEquals(x.gcd(y), z) // As confirmed by Wolfram Alpha
    // test gcd special cases (0 and 1)
    for(w <- Seq(Rational(Int.MaxValue), Rational(BigInt(2).pow(100)))) {
      val n = -w
      assertEquals(Rational.zero.gcd(w), w)
      assertEquals(w.gcd(Rational.zero), w)
      assertEquals(Rational.zero.gcd(n), w)
      assertEquals(n.gcd(Rational.zero), w)
      assertEquals(Rational.one.gcd(w), Rational.one)
      assertEquals(w.gcd(Rational.one), Rational.one)
    }
  }*/

  test("Rational(0D) is Zero") {
    assertEquals(Rational(0d), Rational.zero)
  }

  test("compareToOne") {
    val d = Rational(1, Long.MaxValue)
    assertEquals(Rational.one.compareToOne, 0)
    assertEquals((Rational.one + d).compareToOne, 1)
    assertEquals((Rational.one - d).compareToOne, -1)
  }
  test("limitToLong".ignore) {
    val d = Rational(1, Long.MaxValue)
    // re-enable once #393 is fixed
    assertEquals((Rational.one + d).limitToLong, Rational.one)
  }
  test("numeratorAndDenominatorAsLong") {
    assertEquals(Rational(2, 3).numeratorAsLong, 2L)
    assertEquals(Rational(2, 3).denominatorAsLong, 3L)

    assertEquals((Rational(1, Long.MaxValue) / 2).numeratorAsLong, 1L)
    assertEquals((Rational(Long.MaxValue) * 2).denominatorAsLong, 1L)
  }
  test("quotMod") {
    val a = Rational(31, 4)
    val b = Rational(7, 9)
    assertEquals(a, {
                   val (q, m) = Rational.RationalAlgebra.equotmod(a, b)
                   q * b + m
                 }
    )
  }
  test("isValidFlags") {
    def check(x: Rational,
              whole: Boolean,
              char: Boolean,
              byte: Boolean,
              short: Boolean,
              int: Boolean,
              long: Boolean
    ): Unit = {
      assertEquals(x.isWhole, whole)
      assertEquals(x.isValidChar, char)
      assertEquals(x.isValidByte, byte)
      assertEquals(x.isValidShort, short)
      assertEquals(x.isValidInt, int)
      assertEquals(x.isValidLong, long)
    }

    check(Rational.one, true, true, true, true, true, true)
    check(Rational(1, 2), false, false, false, false, false, false)

    check(Rational(Byte.MaxValue), true, true, true, true, true, true)
    check(Rational(Byte.MaxValue) + 1, true, true, false, true, true, true)
    check(Rational(Byte.MinValue), true, false, true, true, true, true)
    check(Rational(Byte.MinValue) - 1, true, false, false, true, true, true)

    check(Rational(Short.MaxValue), true, true, false, true, true, true)
    check(Rational(Short.MaxValue) + 1, true, true, false, false, true, true)
    check(Rational(Short.MinValue), true, false, false, true, true, true)
    check(Rational(Short.MinValue) - 1, true, false, false, false, true, true)

    check(Rational(Char.MaxValue), true, true, false, false, true, true)
    check(Rational(Char.MaxValue) + 1, true, false, false, false, true, true)
    check(Rational(Char.MinValue), true, true, true, true, true, true)
    check(Rational(Char.MinValue) - 1, true, false, true, true, true, true)

    check(Rational(Int.MaxValue), true, false, false, false, true, true)
    check(Rational(Int.MaxValue) + 1, true, false, false, false, false, true)
    check(Rational(Int.MinValue), true, false, false, false, true, true)
    check(Rational(Int.MinValue) - 1, true, false, false, false, false, true)

    check(Rational(Long.MaxValue), true, false, false, false, false, true)
    check(Rational(Long.MaxValue) + 1, true, false, false, false, false, false)
    check(Rational(Long.MinValue), true, false, false, false, false, true)
    check(Rational(Long.MinValue) - 1, true, false, false, false, false, false)
  }
  test("applyNumber") {
    def rationalFromNumber(x: Number) = Rational(x)
    assert(rationalFromNumber(1) == 1)
    assert(rationalFromNumber(1.0) == 1)
    assert(rationalFromNumber(Rational.one) == 1)
    assert(rationalFromNumber(1: BigDecimal) == 1)
  }

  test("Commutativity of gcd") {
    val a = Rational(-1, SafeLong("7552476006398892199"))
    val b = Rational.one
    assertEquals(a.gcd(b), b.gcd(a))
  }

  test("Commutativity of gcd 2") {
    val a = Rational(SafeLong("-9223372036854775808"), SafeLong("9223372036854775807"))
    val b = Rational.zero
    assertEquals(a.gcd(b), b.gcd(a))
  }
}
