package numerics.math

import org.scalatest.FunSuite

class RatTest extends FunSuite {

  test("rational canonical construction") {
    val r = Rat(5,6)
    assert(r.numerator === BigInt(5))
    assert(r.denominator === BigInt(6))
  }
  test("rational degenerate construction") {
    val r = Rat(30, 345)
    assert(r.numerator === BigInt(2))
    assert(r.denominator === BigInt(23))
  }

  test("RatIsFractional implicit exists") {
    import Implicits._
    def doStuff[NT](a: NT, b: NT)(implicit n: Fractional[NT]) = {
      a / b    // Note: a & b are of unknown type NT, not Rat
    }

    expect(Rat(1, 2)) {
      doStuff(Rat(1), Rat(2))
    }
  }

  test("equality of equivalent canonical and degenerate rationals") {
    val a = Rat(1, 2)
    val b = Rat(8, 16)
    assert(a === b)
  }

  test("non-equivalent rationals are not equal") {
    val a = Rat(1, 2)
    val b = Rat(1, 3)
    val c = Rat(2, 1)
    
    expect(false)(a == b)
    expect(false)(a == c)
  }
  
  test("comparisons") {
    val a = Rat(1, 2)
    val b = Rat(3, 4)
    val c = Rat(-1, 2)
    val d = Rat(1, 2)
    assert(a < b)
    assert(b > a)
    assert(a > c)
    assert(c < a)
    assert(a <= d)
    assert(a >= d)
  }
     
  test("primitive comparisons") {
    val a = Rat("5000000000")
    val b = Rat(-123456)
    val c = Rat(1, 8)
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
    val a = Rat(3, 10)
    val b = Rat(4, 19)

    // This will go through the coprime denominator path.
    // Since, 97 and 190 are coprime, 97/190 is canonical too.
    expect(Rat(97, 190)) {
      a + b
    }

    val c = Rat(1, 2)
    val d = Rat(1, 6)

    // This will go through the non-coprime denominator path. Since the
    // GCD of 2 and 6 is 2, the numerator 1 * 3 + 1 * 1 = 4 is tried first.
    // The GCD of 4 and 2 is 2, so the numerator will need to be reduced.
    expect(Rat(1 * 6 + 1 * 2, 2 * 6)) {
      c + d
    }

    val e = Rat(1, 2)
    val f = Rat(3, 4)
    
    // This will go through the non-coprime denominator path. Since the
    // GCD of 2 and 4 is 2, the numerator 5 is tried first, which is
    // coprime with 2, so the numerator need not be reduced.
    expect(Rat(1 * 4 + 3 * 2, 2 * 4)) {
      e + f
    }
  }

  test("subtraction") {
    // Just ripped from addition
    val a = Rat(3, 10)
    val b = Rat(4, 19)
    expect(Rat(3 * 19 - 4 * 10, 10 * 19)) {
      a - b
    }

    val c = Rat(1, 2)
    val d = Rat(1, 6)
    expect(Rat(1 * 6 - 1 * 2, 2 * 6)) {
      c - d
    }

    val e = Rat(1, 2)
    val f = Rat(3, 4)
    expect(Rat(1 * 4 - 3 * 2, 2 * 4)) {
      e - f
    }
  }

  test("multiplication") {
    val a = Rat(2, 3)
    val b = Rat(1, 2)
    expect(Rat(1, 3)) {
      a * b
    }

    val c = Rat(-321, 23)
    val d = Rat(23, 13)
    expect(Rat(-321 * 23, 23 * 13)) {
      c * d
    }

    val e = Rat(-1, 2)
    expect(Rat(1, 4)) {
      e * e
    }
  }

  test("division") {
    val a = Rat(2, 3)
    val b = Rat(1, 2)
    expect(Rat(4, 3)) {
      a / b
    }

    val c = Rat(-21, 5)
    val d = Rat(7, 18)
    expect(Rat(-54, 5)) {
      c / d
    }

    val e = Rat(-23, 19)
    expect(Rat.one) {
      e / e
    }
  }

  test("division by 0") {
    // Should this be an ArithmeticException?
    intercept[IllegalArgumentException] {
      Rat.one / 0
    }
  }

    test("pow") {
        val a = Rat(1, 2)
    expect(Rat(1, BigInt("4294967296"))) {
      a pow 32
    }

        val b = Rat(-3, 1)
    expect(Rat(9, 1)) {
      b pow 2
    }
    expect(Rat(-27, 1)) {
      b pow 3
    }
    }

    test("longValue") { assert(Rat("5000000000").toLong === 5000000000L) }
    test("intValue") {
        assert(Rat(3).toInt === 3)
        assert(Rat(-5, 2).toInt === -2)
    }
    test("shortValue") {
        assert(Rat(65535).toShort === -1)
        assert(Rat(65536).toShort === 0)
        assert(Rat(-5).toShort === -5)
    }
    test("byteValue") {
        assert(Rat(-1).toByte === -1)
        assert(Rat(256).toByte === 0)
    }
    test("toDoubleAndFloat") {
        assert(Rat(1, 2).toFloat === 0.5f)
        val a = Rat("10000000000000002/10000000000000000")
        assert(a.toDouble === 1.0000000000000002)
        assert(a.toFloat === 1.0f)
    assert(Rat(2, 3).toDouble === 2 / 3.0)
    }

  test("toString") {
    assert(Rat(1, 2).toString === "1/2")
    assert(Rat(1, -2).toString === "-1/2")
    assert(Rat(2, 4).toString === "1/2")
  }

  test("hashCodeIsSameForEquivalentRats") {
    assert(Rat(1, 2).hashCode === Rat(2, 4).hashCode)
    assert(Rat(0).hashCode === Rat(0, 5).hashCode)
    assert(Rat(-1, 2).hashCode === Rat(1, -2).hashCode)
  }
}
