/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import java.math.BigInteger

import spire.util.Opt
import org.scalacheck.Prop._

class SafeLongScalaCheckSuite extends munit.ScalaCheckSuite {

  import SafeLong.zero

  val smin = SafeLong(Long.MinValue)
  val smax = SafeLong(Long.MaxValue)

  def invariant(z: SafeLong): SafeLong = {
    z match {
      case SafeLongLong(_)       => ()
      case SafeLongBigInteger(n) => assert(!BigInt(n).isValidLong)
    }
    z
  }

  property("x + y") {
    forAll { (x: BigInt, y: BigInt) =>
      (invariant(SafeLong(x) + SafeLong(y)) === x + y) &&
      (invariant(SafeLong(x) + y) === x + y) &&
      (invariant(SafeLong(x) + y.toLong) === x + y.toLong)
    }
  }

  property("x - y") {
    forAll { (x: BigInt, y: BigInt) =>
      (invariant(SafeLong(x) - SafeLong(y)) === x - y) &&
      (invariant(SafeLong(x) - y) === x - y) &&
      (invariant(SafeLong(x) - y.toLong) === x - y.toLong)
    }
  }

  property("x * y") {
    forAll { (x: BigInt, y: BigInt) =>
      (invariant(SafeLong(x) * SafeLong(y)) === x * y) &&
      (invariant(SafeLong(x) * y) === x * y) &&
      (invariant(SafeLong(x) * y.toLong) === x * y.toLong)
    }
  }

  property("x / y") {
    forAll { (x: BigInt, y: BigInt) =>
      (y != 0) ==> {
        (invariant(SafeLong(x) /~ SafeLong(y)) === x / y) &&
        (invariant(SafeLong(x) / SafeLong(y)) === x / y) &&
        (invariant(SafeLong(x) / y) === x / y)
      }
    }
  }

  test("x / y") {
    assertEquals(invariant(smin / SafeLong(-1)), -smin)
    assertEquals(invariant(smin / -1L), -smin)
    assertEquals(invariant(smin / BigInt(-1)), -smin)
  }

  property("x % y") {
    forAll { (x: BigInt, y: BigInt) =>
      (y != 0) ==> {
        (invariant(SafeLong(x) % SafeLong(y)) === x % y) &&
        (invariant(SafeLong(x) % y) === x % y)
      }
    }
  }

  test("x % y") {
    assertEquals(invariant(smin % SafeLong(-1)), zero)
    assertEquals(invariant(smin % -1L), zero)
    assertEquals(invariant(smin % BigInt(-1)), zero)
  }

  property("x /% y") {
    forAll { (x: BigInt, y: BigInt) =>
      (y != 0) ==> {
        val sx = SafeLong(x)
        val sy = SafeLong(y)
        (sx /% sy == x /% y) &&
        ((sx /% y) == (x /% y)) &&
        ((sx /% sy) == ((invariant(sx / sy), invariant(sx % sy))))
      }
    }
  }

  test("x /% y") {
    assertEquals((smin /% SafeLong(-1)), (-smin, zero))
    assertEquals((smin /% -1L), (-smin, zero))
    assertEquals((smin /% BigInt(-1)), (-smin, zero))
  }

  property("x ** y") {
    forAll { (x: BigInt, k: Byte) =>
      val sx = SafeLong(x)
      if (k < 0) {
        intercept[RuntimeException] { sx.pow(k) }
        true
      } else {
        (invariant(sx ** k) == x.pow(k)) &&
        (invariant(sx.pow(k)) == x.pow(k))
      }
    }
  }

  property("x.modPow(y, m) == (x ** y) % m") {
    forAll { (x: BigInt, k: Byte, m: BigInt) =>
      val sx = SafeLong(x)
      val sm = SafeLong(m)
      (!sm.isZero) ==> {
        if (k < 0) {
          intercept[RuntimeException] { sx.modPow(k, sm) }
          true
        } else {
          invariant(sx.modPow(k, sm)) == sx.pow(k) % m
        }
      }
    }
  }

  property("comparisons") {
    forAll { (x: BigInt, y: BigInt) =>
      val sx = SafeLong(x)
      val sy = SafeLong(y)
      (invariant(sx.min(sy)) == x.min(y)) &&
      (invariant(sx.max(sy)) == x.max(y)) &&
      (sx.compare(sy) == x.compare(y)) &&
      (sx.signum == sx.compare(zero)) &&
      (sx.isZero == (sx == zero))
    }
  }

  property("x << k") {
    forAll { (x: BigInt, k: Byte) =>
      invariant(SafeLong(x) << k) == SafeLong(x << k)
    }
  }

  if (!sys.props.get("java.vm.name").contains("Scala Native"))
    property("x >> k") {
      forAll { (x: BigInt, k: Byte) =>
        intercept[RuntimeException] { SafeLong(x) >> Int.MinValue }
        invariant(SafeLong(x) >> k) == SafeLong(x >> k)
      }
    }

  property("long safelongs") {
    forAll { (x: Long) =>
      val sx = SafeLong(x)

      intercept[RuntimeException] { sx.pow(-1) }

      (sx.toLong == x) &&
      (sx.getLong == Opt(x)) &&
      sx.isWhole &&
      sx.isValidInt == x.isValidInt &&
      sx.isValidLong &&
      (x == Long.MinValue || (-sx).isValidLong)
    }
  }

  property("conversions, etc.") {
    forAll { (x: BigInt) =>
      val sx = SafeLong(x)
      assertEquals(sx.toString, x.toString)
      assertEquals(sx.toByte, x.toByte)
      assertEquals(sx.toShort, x.toShort)
      assertEquals(sx.toInt, x.toInt)
      assertEquals(sx.toLong, x.toLong)
      assertEquals(sx.toFloat, x.toFloat)
      assertEquals(sx.toDouble, x.toDouble)
      assertEquals(sx.isWhole, true)
    }
  }

  property("mixed size tests") {
    forAll { (ex: Either[Long, BigInt], ey: Either[Long, BigInt]) =>
      val x = ex.fold(BigInt(_), identity)
      val y = ey.fold(BigInt(_), identity)
      val sx = ex.fold(SafeLong(_), SafeLong(_))
      val sy = ey.fold(SafeLong(_), SafeLong(_))

      (sx > sy == x > y) &&
      (sx >= sy == x >= y) &&
      (sx == sy) == (x == y) &&
      (sx <= sy) == (x <= y) &&
      (sx < sy == x < y)
    }
  }

  test("special cases") {
    val firstBig = smax + 1

    // equality
    assert(SafeLong(0) != (BigInt(1) << 64))

    // quotient
    assertEquals(smin / (-smin), SafeLong.minusOne)

    // mod
    assertEquals(smin % (-smin), zero)

    // quotmod
    assertEquals(smin /% (-smin), (SafeLong.minusOne, zero))

    // gcd
    assertEquals(smin.gcd(smin), firstBig)
    assertEquals(smin.gcd(zero), firstBig)
    assertEquals(zero.gcd(smin), firstBig)
    assertEquals(SafeLong(2).gcd(smin), SafeLong(2))
    assertEquals(smin.gcd(smin), firstBig)
    assertEquals(SafeLong(13).gcd(SafeLongBigInteger(BigInteger.ZERO)), SafeLong(13))
    assertEquals(smin.gcd(SafeLongBigInteger(BigInteger.ZERO)), firstBig)
    assertEquals(SafeLong.minusOne.gcd(SafeLongBigInteger(BigInteger.ZERO)), SafeLong.one)

    assertEquals((SafeLong(0).gcd(SafeLong(-13))), SafeLong(13))
    assertEquals((SafeLong(0).gcd(smin)), firstBig)

    assertEquals((SafeLong(-13).gcd(SafeLong(0))), SafeLong(13))
    assertEquals((smin.gcd(SafeLong(0))), firstBig)
  }

  test("regressions") {
    val bx = BigInt(8796093022208L)
    val sx = SafeLong(8796093022208L)
    assert(sx << 23 == bx << 23)
    assert(sx >> -23 == sx << 23)
    assert(sx >> -23 == bx >> -23)
  }

  property("isOdd") {
    forAll { (b: BigInt) =>
      !SafeLong(b * 2).isOdd &&
      SafeLong(b * 2 + 1).isOdd
    }
  }

  property("isEven") {
    forAll { (b: BigInt) =>
      SafeLong(b * 2).isEven &&
      !SafeLong(b * 2 + 1).isEven
    }
  }

}
