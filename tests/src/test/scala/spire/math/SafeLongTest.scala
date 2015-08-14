package spire.math

import java.math.BigInteger

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class SafeLongTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import SafeLong.zero

  val smin = SafeLong(Long.MinValue)
  val smax = SafeLong(Long.MaxValue)

  def invariant(z: SafeLong): SafeLong = {
    z match {
      case SafeLongLong(_) => ()
      case SafeLongBigInteger(n) => BigInt(n).isValidLong shouldBe false
    }
    z
  }

  property("x + y") {
    forAll { (x: BigInt, y: BigInt) =>
      invariant(SafeLong(x) + SafeLong(y)) shouldBe x + y
      invariant(SafeLong(x) + y) shouldBe x + y
      invariant(SafeLong(x) + y.toLong) shouldBe x + y.toLong
    }
  }

  property("x - y") {
    forAll { (x: BigInt, y: BigInt) =>
      invariant(SafeLong(x) - SafeLong(y)) shouldBe x - y
      invariant(SafeLong(x) - y) shouldBe x - y
      invariant(SafeLong(x) - y.toLong) shouldBe x - y.toLong
    }
  }

  property("x * y") {
    forAll { (x: BigInt, y: BigInt) =>
      invariant(SafeLong(x) * SafeLong(y)) shouldBe x * y
      invariant(SafeLong(x) * y) shouldBe x * y
      invariant(SafeLong(x) * y.toLong) shouldBe x * y.toLong
    }
  }

  property("x / y") {
    forAll { (x: BigInt, y: BigInt) =>
      if (y != 0) {
        val expected = x / y
        invariant(SafeLong(x) /~ SafeLong(y)) shouldBe x / y
        invariant(SafeLong(x) / SafeLong(y)) shouldBe x / y
        invariant(SafeLong(x) / y) shouldBe x / y
      }
    }

    invariant(smin / SafeLong(-1)) shouldBe -smin
    invariant(smin / -1L) shouldBe -smin
    invariant(smin / BigInt(-1)) shouldBe -smin
  }

  property("x % y") {
    forAll { (x: BigInt, y: BigInt) =>
      if (y != 0) {
        invariant(SafeLong(x) % SafeLong(y)) shouldBe x % y
        invariant(SafeLong(x) % y) shouldBe x % y
      }
    }

    invariant(smin % SafeLong(-1)) shouldBe zero
    invariant(smin % -1L) shouldBe zero
    invariant(smin % BigInt(-1)) shouldBe zero
  }

  property("x /% y") {
    forAll { (x: BigInt, y: BigInt) =>
      if (y != 0) {
        val sx = SafeLong(x)
        val sy = SafeLong(y)
        sx /% sy shouldBe x /% y
        sx /% y shouldBe x /% y
        sx /% sy shouldBe ((invariant(sx / sy), invariant(sx % sy)))
      }
    }

    (smin /% SafeLong(-1)) shouldBe ((-smin, zero))
    (smin /% -1L) shouldBe ((-smin, zero))
    (smin /% BigInt(-1)) shouldBe ((-smin, zero))
  }

  property("x ** y") {
    forAll { (x: BigInt, k: Byte) =>
      val sx = SafeLong(x)
      if (k < 0) {
        intercept[IllegalArgumentException] { sx pow k }
        true shouldBe true
      } else {
        invariant(sx ** k) shouldBe (x pow k)
        invariant(sx pow k) shouldBe (x pow k)
      }
    }
  }

  property("x.modPow(y, m) == (x ** y) % m") {
    forAll { (x: BigInt, k: Byte, m: BigInt) =>
      val sx = SafeLong(x)
      val sm = SafeLong(m)
      if (!sm.isZero) {
        if (k < 0) {
          intercept[IllegalArgumentException] { sx.modPow(k, sm) }
        } else {
          invariant(sx.modPow(k, sm)) shouldBe (sx pow k) % m
        }
      }
      true shouldBe true
    }
  }

  property("comparisons") {
    forAll { (x: BigInt, y: BigInt) =>
      val sx = SafeLong(x)
      val sy = SafeLong(y)
      invariant(sx min sy) shouldBe (x min y)
      invariant(sx max sy) shouldBe (x max y)
      sx compare sy shouldBe (x compare y)
      sx.signum shouldBe (sx compare zero)
      sx.isZero shouldBe (sx == zero)
    }
  }

  property("x & y") {
    forAll { (x: BigInt, y: BigInt) =>
      invariant(SafeLong(x) & SafeLong(y)) shouldBe SafeLong(x & y)
      invariant(SafeLong(x) & y) shouldBe SafeLong(x & y)
      invariant(SafeLong(x) & y.toLong) shouldBe SafeLong(x & y.toLong)
    }
  }

  property("x | y") {
    forAll { (x: BigInt, y: BigInt) =>
      invariant(SafeLong(x) | SafeLong(y)) shouldBe SafeLong(x | y)
      invariant(SafeLong(x) & y) shouldBe SafeLong(x & y)
      invariant(SafeLong(x) & y.toLong) shouldBe SafeLong(x & y.toLong)
    }
  }

  property("x ^ y") {
    forAll { (x: BigInt, y: BigInt) =>
      invariant(SafeLong(x) ^ SafeLong(y)) shouldBe SafeLong(x ^ y)
    }
  }

  property("x << k") {
    forAll { (x: BigInt, k: Byte) =>
      invariant(SafeLong(x) << k) shouldBe SafeLong(x << k)
    }
  }

  property("x >> k") {
    forAll { (x: BigInt, k: Byte) =>
      intercept[ArithmeticException] { SafeLong(x) >> Int.MinValue }
      invariant(SafeLong(x) >> k) shouldBe SafeLong(x >> k)
    }
  }

  property("long safelongs") {
    forAll { (x: Long) =>
      val sx = SafeLong(x)

      intercept[IllegalArgumentException] { sx pow -1 }

      sx.toLong shouldBe x
      sx.getLong shouldBe Some(x)
      sx.isWhole shouldBe true
      sx.isValidInt shouldBe x.isValidInt
      sx.isValidLong shouldBe true

      (x == Long.MinValue || (-sx).isValidLong) shouldBe true
    }
  }

  property("conversions, etc.") {
    forAll { (x: BigInt) =>
      val sx = SafeLong(x)
      sx.toString shouldBe x.toString
      sx.toByte shouldBe x.toByte
      sx.toShort shouldBe x.toShort
      sx.toInt shouldBe x.toInt
      sx.toLong shouldBe x.toLong
      sx.toFloat shouldBe x.toFloat
      sx.toDouble shouldBe x.toDouble
      sx.isWhole shouldBe true
    }
  }

  property("mixed size tests") {
    forAll { (ex: Either[Long, BigInt], ey: Either[Long, BigInt]) =>
      val x = ex.fold(BigInt(_), identity)
      val y = ey.fold(BigInt(_), identity)
      val sx = ex.fold(SafeLong(_), SafeLong(_))
      val sy = ey.fold(SafeLong(_), SafeLong(_))

      sx > sy shouldBe x > y
      sx >= sy shouldBe x >= y
      sx == sy shouldBe x == y
      sx <= sy shouldBe x <= y
      sx < sy shouldBe x < y
    }
  }

  property("special cases") {
    val firstBig = smax + 1

    // equality
    SafeLong(0) != (BigInt(1) << 64)

    // quotient
    smin / (-smin) shouldBe SafeLong.minusOne

    // mod
    smin % (-smin) shouldBe zero

    // quotmod
    smin /% (-smin) shouldBe ((SafeLong.minusOne, zero))

    // gcd
    smin gcd smin shouldBe firstBig
    smin gcd zero shouldBe firstBig
    zero gcd smin shouldBe firstBig
    SafeLong(2) gcd smin shouldBe SafeLong(2)
    smin gcd smin shouldBe firstBig
    SafeLong(13) gcd SafeLongBigInteger(BigInteger.ZERO) shouldBe SafeLong(13)
    smin gcd SafeLongBigInteger(BigInteger.ZERO) shouldBe firstBig
    SafeLong.minusOne gcd SafeLongBigInteger(BigInteger.ZERO) shouldBe SafeLong.one

    (SafeLong(0) gcd SafeLong(-13)) shouldBe SafeLong(13)
    (SafeLong(0) gcd smin) shouldBe firstBig

    (SafeLong(-13) gcd SafeLong(0)) shouldBe SafeLong(13)
    (smin gcd SafeLong(0)) shouldBe firstBig
  }

  property("regressions") {
    val bx = BigInt(8796093022208L)
    val sx = SafeLong(8796093022208L)
    sx << 23 shouldBe bx << 23
    sx >> -23 shouldBe sx << 23
    sx >> -23 shouldBe bx >> -23
  }
}
