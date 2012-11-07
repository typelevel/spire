package spire.math

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class UnsignedTest extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

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
        case _ => false
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
      whenever (n < ULong.MaxValue) {
        n + 1 > n should be === true
      }
    }
  }
}
