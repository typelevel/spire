package spire.math

import spire.implicits._

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class CRCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit val smallRational = Arbitrary(for {
    n <- arbitrary[Long]
    d <- arbitrary[Long].filter(_ != 0)
  } yield {
    Rational(n, d)
  })

  val biggerRational = Arbitrary(for {
    n <- arbitrary[BigInt]
    d <- arbitrary[BigInt].filter(_ != 0)
  } yield {
    Rational(n, d)
  })

  implicit val arbitraryCR =
    Arbitrary(for { r <- arbitrary[Rational] } yield CR(r))

  property("pi") {
    CR.pi.getString(200) shouldBe "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
  }

  property("e") {
    CR.e.getString(200) shouldBe "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901"
  }

  property("sqrt(2)") {
    CR(2).sqrt.getString(200) shouldBe "1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721264412149709993583141322266592750559275579995050115278206057147"
  }

  property("Rational(n) = CR(n).toRational") {
    forAll { (n: BigInt) =>
      Rational(n) shouldBe CR(n).toRational
    }
  }

  property("CR(n)/CR(d) = CR(n/d)") {
    forAll { (r: Rational) =>
      CR(r.numerator) / CR(r.denominator) shouldBe CR(r)
    }
  }

  property("x + 0 = x") {
    forAll { (x0: Rational) =>
      val x = CR(x0)
      x + CR.zero shouldBe x
    }
  }
  
  property("x * 0 = 0") {
    forAll { (x0: Rational) =>
      val x = CR(x0)
      x * CR.zero shouldBe CR.zero
    }
  }
  
  property("x * 1 = x") {
    forAll { (x0: Rational) =>
      val x = CR(x0)
      x + CR.zero shouldBe x
    }
  }
  
  property("x + y = y + x") {
    forAll { (x0: Rational, y0: Rational) =>
      val (x, y) = (CR(x0), CR(y0))
      x + y shouldBe y + x
    }
  }
  
  property("x + (-x) = 0") {
    forAll { (x0: Rational) =>
      val x = CR(x0)
      x + (-x) shouldBe CR.zero
    }
  }
  
  property("x / x = 1") {
    forAll { (x0: Rational) =>
      if (x0 != 0) {
        val x = CR(x0)
        x / x shouldBe CR.one
      }
    }
  }
  
  property("x * y = y * x") {
    forAll { (x0: Rational, y0: Rational) =>
      val (x, y) = (CR(x0), CR(y0))
      x * y shouldBe y * x
    }
  }
  
  property("x + x = 2x") {
    forAll { (x0: Rational) =>
      val x = CR(x0)
      x + x shouldBe x * CR(2)
    }
  }
  
  property("x * (y + z) = xy + xz") {
    forAll { (x0: Rational, y0: Rational, z0: Rational) =>
      val (x, y, z) = (CR(x0), CR(y0), CR(z0))
      x * (y + z) shouldBe x * y + x * z
    }
  }
  
  property("x.pow(k).nroot(k) = x") {
    forAll { (x0: Rational, k0: Byte) =>
      val x = CR(x0.abs)
      val k = (k0 & 0xff) % 10 + 1
      x.pow(k).nroot(k) shouldBe x
    }
  }
  
  property("x.nroot(k).pow(k) = x") {
    forAll { (x0: Rational, k0: Byte) =>
      val x = CR(x0.abs)
      val k = (k0 & 0xff) % 10 + 1
      x.nroot(k).pow(k) shouldBe x
    }
  }

  // // TODO: this doesn't really work due to the kind of rounding that
  // // even computable reals introduce when computing 1/3.
  // property("x.pow(j).nroot(k) = x.fpow(j/k)") {
  //   forAll { (x0: Int, j0: Byte, k0: Byte) =>
  //     if (x0 > 0) {
  //       val x = CR(x0)
  //       val j = (j0 & 0xff) % 10 + 1
  //       val k = (k0 & 0xff) % 10 + 1
  //       x.pow(j).nroot(k) shouldBe x.fpow(Rational(j, k))
  //     }
  //   }
  // }
}
