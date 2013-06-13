package spire.math

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class NaturalTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {
  property("x + y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      Natural(x) + Natural(y) should be === Natural(x + y)
    }
  }

  property("x - y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (x >= y) {
        Natural(x) - Natural(y) should be === Natural(x - y)
      } else {
        val error = try {
          Natural(x) - Natural(y); false
        } catch {
          case _: ArithmeticException => true
          case _: Exception => false
        }
        error should be === true
      }
    }
  }
  
  property("x * y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      Natural(x) * Natural(y) should be === Natural(x * y)
    }
  }
  
  property("x / y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (y != 0) {
        val result = Natural(x) / Natural(y)
        val expected = Natural(x / y)
        result should be === expected
      }
    }
  }

  property("x % y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (y != 0) {
        val result = Natural(x) % Natural(y)
        val expected = Natural(x % y)
        result should be === expected
      }
    }
  }

  property("x /% y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      if (y != 0) {
        val result = Natural(x) /% Natural(y)
        val expected = (Natural(x / y), Natural(x % y))
        result should be === expected
      }
    }
  }

  property("x compare y") {
    forAll { (_x: BigInt, _y: BigInt) =>
      val (x, y) = (_x.abs, _y.abs)
      val result = Natural(x) compare Natural(y)
      val expected = x compare y
      result should be === expected
    }
  }

  property("x.toString") {
    forAll { _x: BigInt =>
      val x = _x.abs
      Natural(x).toString should be === x.toString
    }
  }

  property("x.toBigInt") {
    forAll { _x: BigInt =>
      val x = _x.abs
      Natural(x).toBigInt should be === x
    }
  }

  property("x.toLong") {
    forAll { _x: BigInt =>
      val x = _x.abs
      Natural(x).toLong should be === x.toLong
    }
  }
}
