package spire.math

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class ULongTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = ULong(0L)
  val one = ULong(1L)

  property("n >= 0") {
    forAll { (n: Long) =>
      ULong(n) >= zero should be === true
    }
  }

  property("a + b == b + a") {
    forAll { (a: Long, b: Long) =>
      ULong(a) + ULong(b) should be === ULong(b) + ULong(a)
    }
  }

  property("a * b == b * a") {
    forAll { (a: Long, b: Long) =>
      ULong(a) * ULong(b) should be === ULong(b) * ULong(a)
    }
  }

  property("(a + b) - b == a") {
    forAll { (a: Long, b: Long) =>
      (ULong(a) + ULong(b)) - ULong(b) should be === ULong(a)
    }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: Long) =>
      val error = try {
        ULong(n) / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: Long) =>
      ULong(n) / one should be === ULong(n)
      ULong(n) % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: Long, d: Long) =>
      whenever (d != 0L) {
        val q = ULong(n) / ULong(d)
        val r = ULong(n) % ULong(d)
        q * ULong(d) + r should be === ULong(n)
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: Long, d: Long) =>
      whenever (d != 0L) {
        ULong(n) / ULong(d) <= ULong(n) should be === true
      }
    }
  }

  property("n % d < d") {
    forAll { (n: Long, d: Long) =>
      whenever (d != 0L) {
        ULong(n) % ULong(d) < ULong(d) should be === true
      }
    }
  }

  property("n + 1 > n") {
    forAll { (n: Long) =>
      whenever (n != -1L) {
        n + 1 > n should be === true
      }
    }
  }
}

class UIntTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = UInt(0)
  val one = UInt(1)

  property("n >= 0") {
    forAll { (n: Int) =>
      UInt(n) >= zero should be === true
    }
  }

  property("a + b == b + a") {
    forAll { (a: Int, b: Int) =>
      UInt(a) + UInt(b) should be === UInt(b) + UInt(a)
    }
  }

  property("a * b == b * a") {
    forAll { (a: Int, b: Int) =>
      UInt(a) * UInt(b) should be === UInt(b) * UInt(a)
    }
  }

  property("(a + b) - b == a") {
    forAll { (a: Int, b: Int) =>
      (UInt(a) + UInt(b)) - UInt(b) should be === UInt(a)
    }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: Int) =>
      val error = try {
        UInt(n) / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: Int) =>
      UInt(n) / one should be === UInt(n)
      UInt(n) % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: Int, d: Int) =>
      whenever (d != 0) {
        val q = UInt(n) / UInt(d)
        val r = UInt(n) % UInt(d)
        q * UInt(d) + r should be === UInt(n)
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: Int, d: Int) =>
      whenever (d != 0) {
        UInt(n) / UInt(d) <= UInt(n) should be === true
      }
    }
  }

  property("n % d < d") {
    forAll { (n: Int, d: Int) =>
      whenever (d != 0) {
        UInt(n) % UInt(d) < UInt(d) should be === true
      }
    }
  }

  property("n + 1 > n") {
    forAll { (n: Int) =>
      whenever (n != -1) {
        UInt(n) + UInt(1) > UInt(n) should be === true
      }
    }
  }
}

class UShortTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = UShort(0)
  val one = UShort(1)

  property("n >= 0") {
    forAll { (n: Char) =>
      UShort(n) >= zero should be === true
    }
  }

  property("a + b == b + a") {
    forAll { (a: Char, b: Char) =>
      UShort(a) + UShort(b) should be === UShort(b) + UShort(a)
    }
  }

  property("a * b == b * a") {
    forAll { (a: Char, b: Char) =>
      UShort(a) * UShort(b) should be === UShort(b) * UShort(a)
    }
  }

  property("(a + b) - b == a") {
    forAll { (a: Char, b: Char) =>
      (UShort(a) + UShort(b)) - UShort(b) should be === UShort(a)
    }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: Char) =>
      val error = try {
        UShort(n) / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: Char) =>
      UShort(n) / one should be === UShort(n)
      UShort(n) % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: Char, d: Char) =>
      whenever (d != 0) {
        val q = UShort(n) / UShort(d)
        val r = UShort(n) % UShort(d)
        q * UShort(d) + r should be === UShort(n)
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: Char, d: Char) =>
      whenever (d != 0) {
        UShort(n) / UShort(d) <= UShort(n) should be === true
      }
    }
  }

  property("n % d < d") {
    forAll { (n: Char, d: Char) =>
      whenever (d != 0) {
        UShort(n) % UShort(d) < UShort(d) should be === true
      }
    }
  }

  property("n + 1 > n") {
    forAll { (n: Char) =>
      whenever (n != -1) {
        UShort(n) + UShort(1) > UShort(n) should be === true
      }
    }
  }
}

class UByteTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val zero = UByte(0)
  val one = UByte(1)

  property("UByte(-128) >> 1 == UByte(64)") {
    (UByte(-128) >> 1) should be === UByte(64)
  }

  property("n >= 0") {
    forAll { (n: Byte) =>
      UByte(n) >= zero should be === true
    }
  }

  property("a + b == b + a") {
    forAll { (a: Byte, b: Byte) =>
      UByte(a) + UByte(b) should be === UByte(b) + UByte(a)
    }
  }

  property("a * b == b * a") {
    forAll { (a: Byte, b: Byte) =>
      UByte(a) * UByte(b) should be === UByte(b) * UByte(a)
    }
  }

  property("(a + b) - b == a") {
    forAll { (a: Byte, b: Byte) =>
      (UByte(a) + UByte(b)) - UByte(b) should be === UByte(a)
    }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: Byte) =>
      val error = try {
        UByte(n) / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error should be === true
    }
  }

  property("n / 1 == n") {
    forAll { (n: Byte) =>
      UByte(n) / one should be === UByte(n)
      UByte(n) % one should be === zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: Byte, d: Byte) =>
      whenever (d != 0) {
        val q = UByte(n) / UByte(d)
        val r = UByte(n) % UByte(d)
        q * UByte(d) + r should be === UByte(n)
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: Byte, d: Byte) =>
      whenever (d != 0) {
        UByte(n) / UByte(d) <= UByte(n) should be === true
      }
    }
  }

  property("n % d < d") {
    forAll { (n: Byte, d: Byte) =>
      whenever (d != 0) {
        UByte(n) % UByte(d) < UByte(d) should be === true
      }
    }
  }

  property("n + 1 > n") {
    forAll { (n: Byte) =>
      whenever (n != -1) {
        UByte(n) + UByte(1) > UByte(n) should be === true
      }
    }
  }

  property("n << x == n << (x + 32 * y)") {
    forAll { (n: Byte, x: Int, y: Int) =>
      UByte(n) << x should be === UByte(n) << (x + 32 * y)
    }
  }

  property("n >> x == n >> (x % 32)") {
    forAll { (n: Byte, x: Int, y: Int) =>
      UByte(n) >> x should be === UByte(n) >> (x % 32)
    }
  }

  property("n >> x == n >>> x") {
    forAll { (n: Byte, x: Byte) =>
      UByte(n) >> x should be === UByte(n) >>> x
    }
  }
}
