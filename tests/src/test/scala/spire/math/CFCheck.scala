package spire.math

import spire.implicits._

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import ArbitrarySupport._
import Ordinal._

class CFCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import spire.math.{SafeLong => N}

  // property("Rational(n) = CF(n).toRational") {
  //   forAll { (n: BigInt) =>
  //     Rational(n) shouldBe CF(n).toRational
  //   }
  // }

  property("x + y") {
    forAll { (x: Int, y: Int) =>
      CF(x) + CF(y) shouldBe CF(N(x) + N(y))
    }
  }

  property("x - y") {
    forAll { (x: Int, y: Int) =>
      CF(x) - CF(y) shouldBe CF(N(x) - N(y))
    }
  }

  property("x * y") {
    forAll { (x: Int, y: Int) =>
      CF(x) * CF(y) shouldBe CF(N(x) * N(y))
    }
  }

  property("x / y") {
    forAll { (x: Int, y: Int) =>
      if (y != 0) CF(x) / CF(y) shouldBe CF(Rational(x, y))
    }
  }
  
  property("CF(n)/CF(d) = CF(n/d)") {
    forAll { (r: Rational) =>
      CF(r.numerator) / CF(r.denominator) shouldBe CF(r)
    }
  }
  
  property("x + 0 = x") {
    forAll { (x: CF) =>
      x + CF.zero shouldBe x
    }
  }
  
  property("x * 0 = 0") {
    forAll { (x: CF) =>
      x * CF.zero shouldBe CF.zero
    }
  }
  
  property("x * 1 = x") {
    forAll { (x: CF) =>
      x + CF.zero shouldBe x
    }
  }
  
  property("x + y = y + x") {
    forAll { (x: CF, y: CF) =>
      x + y shouldBe y + x
    }
  }
  
  property("x + (-x) = 0") {
    forAll { (x: CF) =>
      x + (-x) shouldBe CF.zero
    }
  }
  
  property("x / x = 1") {
    forAll { (x: CF) =>
      if (x != 0) x / x shouldBe CF.one
    }
  }
  
  property("x * y = y * x") {
    forAll { (x: CF, y: CF) =>
      x * y shouldBe y * x
    }
  }
  
  property("x + x = 2x") {
    forAll { (x: CF) =>
      x + x shouldBe x * CF(2)
    }
  }
  
  property("x * (y + z) = xy + xz") {
    forAll { (x: CF, y: CF, z: CF) =>
      x * (y + z) shouldBe x * y + x * z
    }
  }
  
  property("x.pow(2) = x * x") {
    forAll { (x: CF) =>
      x.pow(2) shouldBe x * x
    }
  }
  
  property("x.pow(3) = x * x * x") {
    forAll { (x: CF) =>
      x.pow(2) shouldBe x * x
    }
  }
  
  // property("x.pow(k).nroot(k) = x") {
  //   forAll { (x0: CF, k: Sized[Int, _1, _10]) =>
  //     val x = x0.abs
  //     x.pow(k.num).nroot(k.num) shouldBe x
  //   }
  // }
  // 
  // property("x.nroot(k).pow(k) = x") {
  //   forAll { (x0: CF, k: Sized[Int, _1, _10]) =>
  //     val x = x0.abs
  //     x.nroot(k.num).pow(k.num) shouldBe x
  //   }
  // }
  // 
  // property("pythagorean theorem") {
  //   forAll { (y: CF, x: CF) =>
  //     if (x.signum != 0 || y.signum != 0) {
  //       val mag = (x.pow(2) + y.pow(2)).sqrt
  //       val x0 = x / mag
  //       val y0 = y / mag
  //       x0.pow(2) + y0.pow(2) shouldBe CF(1)
  //     }
  //   }
  // }

  // property("cos(atan2(y, x)) = x/mag") {
  //   forAll { (y: CF, x: CF) =>
  //     if (x.signum != 0 || y.signum != 0) {
  //       val mag = (x.pow(2) + y.pow(2)).sqrt
  //       CF.cos(CF.atan2(y, x)) shouldBe (x / mag)
  //     }
  //   }
  // }
  // 
  // property("sin(atan2(y, x)) = y/mag") {
  //   forAll { (y: CF, x: CF) =>
  //     if (x.signum != 0 || y.signum != 0) {
  //       val mag = (x.pow(2) + y.pow(2)).sqrt
  //       CF.sin(CF.atan2(y, x)) shouldBe (y / mag)
  //     }
  //   }
  // }

  // property("complex multiplication") {
  //   forAll { (re: CF, im: CF) =>
  //     val ma = (re.pow(2) + im.pow(2)).sqrt
  //     val ph = CF.atan2(im, re)
  // 
  //     ma * CF.cos(ph) shouldBe re.pow(2) - im.pow(2)
  //     ma * CF.sin(ph) shouldBe re * im * CF(2)
  //   }
  // }

  // def sample1(name: String)(f: CF => CF) {
  //   property(name) {
  //     forAll { (x0: Rational, i0: Byte, j0: Byte) =>
  //       val x = f(CF(x0.abs))
  //       val i = (i0 & 0xff) % 250 + 1
  //       val j = (j0 & 0xff) % 250 + 1
  //       val (k1, k2) = if (i <= j) (i, j) else (j, i)
  //       val v1 = x(k1) 
  //       val v2 = x(k2)
  //       val v3 = CF.roundUp(Rational(v2, SafeLong(2).pow(k2 - k1)))
  //       v1 shouldBe v3
  //     }
  //   }
  // }
  //
  // sample1("sample1 id")(x => x)
  // sample1("sample1 negate")(x => -x)
  // sample1("sample1 +")(x => x + x)
  // sample1("sample1 *")(x => x * x)
  // sample1("sample1 sqrt")(_.sqrt)
  // sample1("sample1 pow(2)")(_.pow(2))

  // def arcSample(f: Rational => Rational)(g: Double => Double, h: CF => CF): String =
  //   (-8 to 8).map { i =>
  //     val x = CF(f(Rational(i)))
  //     if ((g(x.toDouble) - h(x).toDouble).abs < 0.00001) "." else "!"
  //   }.mkString
  // 
  // // useful for visually debugging atan/asin
  // property("atan sample") {
  //   arcSample(_ / 2)(scala.math.atan, CF.atan)
  // }
  // 
  // property("asin sample") {
  //   arcSample(_ / 8)(scala.math.asin, CF.asin)
  // }
  // 
  // property("acos sample") {
  //   arcSample(_ / 8)(scala.math.acos, CF.acos)
  // }

  // // TODO: this doesn't really work due to the kind of rounding that
  // // even computable reals introduce when computing 1/3.
  // property("x.pow(j).nroot(k) = x.fpow(j/k)") {
  //   forAll { (x0: Int, j0: Byte, k0: Byte) =>
  //     if (x0 > 0) {
  //       val x = CF(x0)
  //       val j = (j0 & 0xff) % 10 + 1
  //       val k = (k0 & 0xff) % 10 + 1
  //       x.pow(j).nroot(k) shouldBe x.fpow(Rational(j, k))
  //     }
  //   }
  // }
}
