package spire.math

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class SafeLongTest extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {
  property("x + y") {
    forAll { (x: Long, y: Long) =>
      SafeLong(x) + SafeLong(y) shouldBe BigInt(x) + BigInt(y)
    }
  }

  property("x - y") {
    forAll { (x: Long, y: Long) =>
      SafeLong(x) - SafeLong(y) shouldBe BigInt(x) - BigInt(y)
    }
  }

  property("x * y") {
    forAll { (x: Long, y: Long) =>
      SafeLong(x) * SafeLong(y) shouldBe BigInt(x) * BigInt(y)
    }
  }

  property("x / y") {
    forAll { (x: Long, y: Long) =>
      if (y != 0)
        SafeLong(x) / SafeLong(y) shouldBe BigInt(x) / BigInt(y)
    }
  }

  property("x % y") {
    forAll { (x: Long, y: Long) =>
      if (y != 0)
        SafeLong(x) % SafeLong(y) shouldBe BigInt(x) % BigInt(y)
    }
  }

  property("x ** y") {
    forAll { (x: Long, y: Int) =>
      val exp: Int = y.abs % 64
      SafeLong(x) ** exp shouldBe BigInt(x).pow(exp)
    }
  }

  property("x.modPow(y, m) == (x ** y) % m") {
    forAll { (x: Long, y: Int, m: Long) =>
      val exp: Int = y.abs % 64
      if (m != 0)
        SafeLong(x).modPow(exp, SafeLong(m)) shouldBe BigInt(x).pow(exp) % BigInt(m)
    }
  }
}
