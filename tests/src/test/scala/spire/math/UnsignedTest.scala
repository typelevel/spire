package spire.math

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import ArbitrarySupport.{ubyte, ushort, uint, ulong}

class ULongTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  val zero = ULong(0L)
  val one = ULong(1L)

  property("n >= 0") {
    forAll { (n: ULong) => n >= zero shouldBe true }
  }

  property("a + b == b + a") {
    forAll { (a: ULong, b: ULong) => a + b shouldBe b + a }
  }

  property("a * b == b * a") {
    forAll { (a: ULong, b: ULong) => a * b shouldBe b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: ULong, b: ULong) => (a + b) - b shouldBe a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: ULong) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error shouldBe true
    }
  }

  property("n / 1 == n") {
    forAll { (n: ULong) =>
      n / one shouldBe n
      n % one shouldBe zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: ULong, d: ULong) =>
      whenever (d != zero) {
        val q = n / d
        val r = n % d
        q * d + r shouldBe n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: ULong, d: ULong) =>
      whenever (d != zero) {
        n / d <= n shouldBe true
      }
    }
  }

  property("n % d < d") {
    forAll { (n: ULong, d: ULong) =>
      whenever (d != zero) {
        n % d < d shouldBe true
      }
    }
  }

  property("n + 1 > n") {
    forAll { (n: ULong) =>
      whenever (n != ULong.MaxValue) {
        n + one > n shouldBe true
      }
    }
  }

  property("n + (-n) == 0") {
    forAll { (n: ULong) => n + (-n) shouldBe zero }
  }

  property("a < b") {
    forAll { (a: ULong, b: ULong) => a < b shouldBe a.toBigInt < b.toBigInt }
  }

  property("a <= b") {
    forAll { (a: ULong, b: ULong) => a <= b shouldBe a.toBigInt <= b.toBigInt }
  }

  property("a > b") {
    forAll { (a: ULong, b: ULong) => a > b shouldBe a.toBigInt > b.toBigInt }
  }

  property("a >= b") {
    forAll { (a: ULong, b: ULong) => a >= b shouldBe a.toBigInt >= b.toBigInt }
  }

  property("a.toString = a.toBigInt.toString") {
    forAll { (n: ULong) =>
      n.toString shouldBe n.toBigInt.toString
    }
  }

  property("toFloat") {
    forAll { (n: ULong) =>
      n.toFloat shouldBe n.toBigInt.toFloat
    }
  }

  property("toDouble") {
    forAll { (n: ULong) =>
      val d = new java.math.BigDecimal(n.toDouble).toPlainString
      println(s"$n versus $d")
      n.toDouble shouldBe n.toBigInt.toDouble
    }
  }
}

class UIntTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  val zero = UInt(0)
  val one = UInt(1)

  property("n >= 0") {
    forAll { (n: UInt) =>
      n >= zero shouldBe true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UInt, b: UInt) => a + b shouldBe b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Int, b: Int) => a * b shouldBe b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UInt, b: UInt) => (a + b) - b shouldBe a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UInt) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error shouldBe true
    }
  }

  property("n / 1 == n") {
    forAll { (n: UInt) =>
      n / one shouldBe n
      n % one shouldBe zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UInt, d: UInt) =>
      whenever (d != zero) {
        val q = n / d
        val r = n % d
        q * d + r shouldBe n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UInt, d: UInt) =>
      whenever (d != zero) { n / d <= n shouldBe true }
    }
  }

  property("n % d < d") {
    forAll { (n: UInt, d: UInt) =>
      whenever (d != zero) { n % d < d shouldBe true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UInt) =>
      whenever (n != UInt.MaxValue) { n + one > n shouldBe true }
    }
  }

  property("n + (-n) == 0") {
    forAll { (n: UInt) => n + (-n) shouldBe zero }
  }

  property("a < b") {
    forAll { (a: UInt, b: UInt) => a < b shouldBe a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UInt, b: UInt) => a <= b shouldBe a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UInt, b: UInt) => a > b shouldBe a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UInt, b: UInt) => a >= b shouldBe a.toLong >= b.toLong }
  }
}

class UShortTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  val zero = UShort(0)
  val one = UShort(1)

  property("n >= 0") {
    forAll { (n: UShort) =>
      n >= zero shouldBe true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UShort, b: UShort) => a + b shouldBe b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Short, b: Short) => a * b shouldBe b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UShort, b: UShort) => (a + b) - b shouldBe a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UShort) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error shouldBe true
    }
  }

  property("n / 1 == n") {
    forAll { (n: UShort) =>
      n / one shouldBe n
      n % one shouldBe zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UShort, d: UShort) =>
      whenever (d != zero) {
        val q = n / d
        val r = n % d
        q * d + r shouldBe n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UShort, d: UShort) =>
      whenever (d != zero) { n / d <= n shouldBe true }
    }
  }

  property("n % d < d") {
    forAll { (n: UShort, d: UShort) =>
      whenever (d != zero) { n % d < d shouldBe true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UShort) =>
      whenever (n != UShort.MaxValue) { n + one > n shouldBe true }
    }
  }

  property("n + (-n) == 0") {
    forAll { (n: UShort) => n + (-n) shouldBe zero }
  }

  property("a < b") {
    forAll { (a: UShort, b: UShort) => a < b shouldBe a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UShort, b: UShort) => a <= b shouldBe a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UShort, b: UShort) => a > b shouldBe a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UShort, b: UShort) => a >= b shouldBe a.toLong >= b.toLong }
  }
}

class UByteTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  val zero = UByte(0)
  val one = UByte(1)

  property("n >= 0") {
    forAll { (n: UByte) =>
      n >= zero shouldBe true
    }
  }

  property("a + b == b + a") {
    forAll { (a: UByte, b: UByte) => a + b shouldBe b + a }
  }

  property("a * b == b * a") {
    forAll { (a: Byte, b: Byte) => a * b shouldBe b * a }
  }

  property("(a + b) - b == a") {
    forAll { (a: UByte, b: UByte) => (a + b) - b shouldBe a }
  }

  property("n / 0 -> ArithmeticException") {
    forAll { (n: UByte) =>
      val error = try {
        n / zero
        false
      } catch {
        case _: ArithmeticException => true
        case _: Exception => false
      }
      error shouldBe true
    }
  }

  property("n / 1 == n") {
    forAll { (n: UByte) =>
      n / one shouldBe n
      n % one shouldBe zero
    }
  }

  property("(n / d) * d + (n % d) == n") {
    forAll { (n: UByte, d: UByte) =>
      whenever (d != zero) {
        val q = n / d
        val r = n % d
        q * d + r shouldBe n
      }
    }
  }

  property("n / d <= n") {
    forAll { (n: UByte, d: UByte) =>
      whenever (d != zero) { n / d <= n shouldBe true }
    }
  }

  property("n % d < d") {
    forAll { (n: UByte, d: UByte) =>
      whenever (d != zero) { n % d < d shouldBe true }
    }
  }

  property("n + 1 > n") {
    forAll { (n: UByte) =>
      whenever (n != UByte.MaxValue) { n + one > n shouldBe true }
    }
  }

  property("n + (-n) == 0") {
    forAll { (n: UByte) => n + (-n) shouldBe zero }
  }

  property("a < b") {
    forAll { (a: UByte, b: UByte) => a < b shouldBe a.toLong < b.toLong }
  }

  property("a <= b") {
    forAll { (a: UByte, b: UByte) => a <= b shouldBe a.toLong <= b.toLong }
  }

  property("a > b") {
    forAll { (a: UByte, b: UByte) => a > b shouldBe a.toLong > b.toLong }
  }

  property("a >= b") {
    forAll { (a: UByte, b: UByte) => a >= b shouldBe a.toLong >= b.toLong }
  }
}
