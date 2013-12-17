package spire.math

import spire.implicits._

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class RealCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

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

  implicit val arbitraryReal =
    Arbitrary(for { r <- arbitrary[Rational] } yield Real(r))

  property("pi") {
    Real.pi.getString(200) shouldBe "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196"
  }

  property("e") {
    Real.e.getString(200) shouldBe "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901"
  }

  property("sqrt(2)") {
    Real(2).sqrt.getString(200) shouldBe "1.41421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157273501384623091229702492483605585073721264412149709993583141322266592750559275579995050115278206057147"
  }

  property("Rational(n) = Real(n).toRational") {
    forAll { (n: BigInt) =>
      Rational(n) shouldBe Real(n).toRational
    }
  }

  property("Real(n)/Real(d) = Real(n/d)") {
    forAll { (r: Rational) =>
      Real(r.numerator) / Real(r.denominator) shouldBe Real(r)
    }
  }

  property("x + 0 = x") {
    forAll { (x0: Rational) =>
      val x = Real(x0)
      x + Real.zero shouldBe x
    }
  }
  
  property("x * 0 = 0") {
    forAll { (x0: Rational) =>
      val x = Real(x0)
      x * Real.zero shouldBe Real.zero
    }
  }
  
  property("x * 1 = x") {
    forAll { (x0: Rational) =>
      val x = Real(x0)
      x + Real.zero shouldBe x
    }
  }
  
  property("x + y = y + x") {
    forAll { (x0: Rational, y0: Rational) =>
      val (x, y) = (Real(x0), Real(y0))
      x + y shouldBe y + x
    }
  }
  
  property("x + (-x) = 0") {
    forAll { (x0: Rational) =>
      val x = Real(x0)
      x + (-x) shouldBe Real.zero
    }
  }
  
  property("x / x = 1") {
    forAll { (x0: Rational) =>
      if (x0 != 0) {
        val x = Real(x0)
        x / x shouldBe Real.one
      }
    }
  }
  
  property("x * y = y * x") {
    forAll { (x0: Rational, y0: Rational) =>
      val (x, y) = (Real(x0), Real(y0))
      x * y shouldBe y * x
    }
  }
  
  property("x + x = 2x") {
    forAll { (x0: Rational) =>
      val x = Real(x0)
      x + x shouldBe x * Real(2)
    }
  }
  
  property("x * (y + z) = xy + xz") {
    forAll { (x0: Rational, y0: Rational, z0: Rational) =>
      val (x, y, z) = (Real(x0), Real(y0), Real(z0))
      x * (y + z) shouldBe x * y + x * z
    }
  }
  
  property("x.pow(k).nroot(k) = x") {
    forAll { (x0: Rational, k0: Byte) =>
      val x = Real(x0.abs)
      val k = (k0 & 0xff) % 10 + 1
      x.pow(k).nroot(k) shouldBe x
    }
  }
  
  property("x.nroot(k).pow(k) = x") {
    forAll { (x0: Rational, k0: Byte) =>
      val x = Real(x0.abs)
      val k = (k0 & 0xff) % 10 + 1
      x.nroot(k).pow(k) shouldBe x
    }
  }

  // // useful for visually debugging atan/asin
  // property("atan view") {
  //   println((-8 to 8).map { i =>
  //     val x = Real(Rational(i, 2))
  //     if ((scala.math.atan(x.toDouble) - Real.atan(x).toDouble).abs < 0.00001) "." else "!"
  //   }.mkString)
  // }
  // 
  // property("asin view") {
  //   println((-8 to 8).map { i =>
  //     val x = Real(Rational(i, 8))
  //     if ((scala.math.asin(x.toDouble) - Real.asin(x).toDouble).abs < 0.00001) "." else "!"
  //   }.mkString)
  // }

  property("Real.acos") {
    forAll { (n: Rational) =>
      // (-1) to (1)
      val x = Real(if (n.abs > 1) n.reciprocal else n)
      if (x.toDouble.abs != 0.5) { // work around Rational#toDouble bug for now
        val a1 = scala.math.acos(x.toDouble)
        val a2 = Real.acos(x)
        (a1 - a2.toDouble).abs should be < 0.00001
      }
    }
  }
  
  property("Real.asin") {
    forAll { (n: Rational) =>
      // (-1) to (1)
      val x = Real(if (n.abs > 1) n.reciprocal else n)
      if (x.toDouble.abs != 0.5) { // work around Rational#toDouble bug for now
        val a1 = scala.math.asin(x.toDouble)
        val a2 = Real.asin(x)
        (a1 - a2.toDouble).abs should be < 0.00001
      }
    }
  }

  property("Real.atan") {
    forAll { (n: Int) =>
      // (-inf) to (inf)
      val x = Real(Rational(n, 1024 * 1024))
      val a1 = scala.math.atan(x.toDouble)
      val a2 = Real.atan(x)
      (a1 - a2.toDouble).abs should be < 0.00001
    }
  }

  // // TODO: this doesn't really work due to the kind of rounding that
  // // even computable reals introduce when computing 1/3.
  // property("x.pow(j).nroot(k) = x.fpow(j/k)") {
  //   forAll { (x0: Int, j0: Byte, k0: Byte) =>
  //     if (x0 > 0) {
  //       val x = Real(x0)
  //       val j = (j0 & 0xff) % 10 + 1
  //       val k = (k0 & 0xff) % 10 + 1
  //       x.pow(j).nroot(k) shouldBe x.fpow(Rational(j, k))
  //     }
  //   }
  // }
}
