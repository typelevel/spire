package spire.math

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class NaturalTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  property("x + y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      Natural(x) + Natural(y) shouldBe Natural(x + y)
    }
  }

  property("x - y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (x >= y) {
        Natural(x) - Natural(y) shouldBe Natural(x - y)
      } else {
        val error = try {
          Natural(x) - Natural(y); false
        } catch {
          case _: ArithmeticException => true
          case _: Exception => false
        }
        error shouldBe true
      }
    }
  }
  
  property("x * y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      Natural(x) * Natural(y) shouldBe Natural(x * y)
    }
  }
  
  property("x / y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (y != 0) {
        val result = Natural(x) / Natural(y)
        val expected = Natural(x / y)
        result shouldBe expected
      }
    }
  }

  property("x % y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (y != 0) {
        val result = Natural(x) % Natural(y)
        val expected = Natural(x % y)
        result shouldBe expected
      }
    }
  }

  property("x /% y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (y != 0) {
        val result = Natural(x) /% Natural(y)
        val expected = (Natural(x / y), Natural(x % y))
        result shouldBe expected
      }
    }
  }

  property("x compare y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      val result = Natural(x) compare Natural(y)
      val expected = x compare y
      result shouldBe expected
    }
  }

  property("x.toString") {
    forAll { _x: BigInt =>
      val x = _x.abs
      Natural(x).toString shouldBe x.toString
    }
  }

  property("x.toBigInt") {
    forAll { _x: BigInt =>
      val x = _x.abs
      Natural(x).toBigInt shouldBe x
    }
  }

  property("x.toLong") {
    forAll { _x: BigInt =>
      val x = _x.abs
      Natural(x).toLong shouldBe x.toLong
    }
  }
}
