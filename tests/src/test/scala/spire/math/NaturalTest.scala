package spire
package math

import org.scalacheck.Arbitrary._
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.check.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class NaturalTest extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  import spire.std.bigInt._
  import ArbitrarySupport._
  type N = NonNegative[BigInt]

  property("x + y") {
    forAll { (x: N, y: N) =>
      Natural(x.num) + Natural(y.num) shouldBe Natural(x.num + y.num)
    }
  }

  property("x - y") {
    forAll { (x: N, y: N) =>
      val z = Try(Natural(x.num) - Natural(y.num))
      if (x.num >= y.num) {
        z shouldBe Try(Natural(x.num - y.num))
      } else {
        z.isFailure shouldBe true
      }
    }
  }

  property("x * y") {
    forAll { (x: N, y: N) =>
      Natural(x.num) * Natural(y.num) shouldBe Natural(x.num * y.num)
    }
  }

  property("x / y") {
    forAll { (x: N, y: Positive[BigInt]) =>
      Natural(x.num) / Natural(y.num) shouldBe Natural(x.num / y.num)
    }
  }

  property("x % y") {
    forAll { (x: N, y: Positive[BigInt]) =>
      Natural(x.num) % Natural(y.num) shouldBe Natural(x.num % y.num)
    }
  }

  property("x /% y") {
    forAll { (x: N, y: Positive[BigInt]) =>
      (Natural(x.num) /% Natural(y.num)) shouldBe ((Natural(x.num / y.num), Natural(x.num % y.num)))
    }
  }

  property("x compare y") {
    forAll { (x: N, y: N) =>
      (Natural(x.num) compare Natural(y.num)) shouldBe (x.num compare y.num)
    }
  }

  property("x.toString") {
    forAll { x: N =>
      Natural(x.num).toString shouldBe x.num.toString
    }
  }

  property("x.toBigInt") {
    forAll { x: N =>
      Natural(x.num).toBigInt shouldBe x.num
    }
  }

  property("x.toLong") {
    forAll { x: N =>
      Natural(x.num).toLong shouldBe x.num.toLong
    }
  }
}
