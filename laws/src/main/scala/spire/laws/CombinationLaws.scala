package spire
package laws

import spire.algebra._
import spire.implicits._

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

object CombinationLaws {
  def apply[A : Eq : Arbitrary] = new CombinationLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

/** Contains laws that are obeying by combination of types, for example
  * various kinds of signed rings. 
  */
trait CombinationLaws[A] extends OrderLaws[A] {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  // copy of those in LimitedRangeLaws

  def signedAdditiveCMonoid(implicit signedA: Signed[A], additiveCMonoidA: AdditiveCMonoid[A]) = new DefaultRuleSet(
    name = "signedAdditiveCMonoid",
    parent = Some(signed),
    "ordered group" → forAll { (x: A, y: A, z: A) =>
      (x <= y) ==> (x + z <= y + z)
    },
    "triangle inequality" → forAll { (x: A, y: A) =>
      (x + y).abs <= x.abs + y.abs
    }
  )

  def signedAdditiveAbGroup(implicit signedA: Signed[A], additiveAbGroupA: AdditiveAbGroup[A]) = new DefaultRuleSet(
    name = "signedAdditiveAbGroup",
    parent = Some(signedAdditiveCMonoid),
    "abs(x) equals abs(-x)" → forAll { (x: A) =>
      x.abs === (-x).abs
    }
  )

  // more a convention: as GCD is defined up to a unit, so up to a sign,
  // on an ordered GCD ring we require gcd(x, y) >= 0, which is the common
  // behavior of computer algebra systems
  def signedGCDRing(implicit signedA: Signed[A], gcdRingA: GCDRing[A]) = new DefaultRuleSet(
    name = "signedGCDRing",
    parent = Some(signedAdditiveAbGroup),
    "gcd(x, y) >= 0" → forAll { (x: A, y: A) =>
      x.gcd(y).signum >= 0
    },
    "gcd(x, 1) === 1" → forAll { (x: A) =>
      x.gcd(Ring[A].one) === Ring[A].one
    },
    "gcd(x, 0) === abs(x)" → forAll { (x: A) =>
      x.gcd(Ring[A].zero) === Signed[A].abs(x)
    },
    "lcm(x, 1) === x" → forAll { (x: A) =>
      x.lcm(Ring[A].one) === x
    }
  )

  def truncatedDivision(implicit cRingA: CRing[A], truncatedDivisionA: TruncatedDivision[A]) = new DefaultRuleSet(
    name = "truncatedDivision",
    parent = Some(signedAdditiveAbGroup),
    "division rule (t_/%)" → forAll { (x: A, y: A) =>
      y.isZero || {
        val (q, r) = x t_/% y
        x === y * q + r
      }
    },
    "division rule (f_/%)" → forAll { (x: A, y: A) =>
      y.isZero || {
        val (q, r) = x f_/% y
        x === y * q + r
      }
    },
    "quotient is integer (t_/~)" → forAll { (x: A, y: A) =>
      y.isZero || (x t_/~ y).toBigIntOpt.nonEmpty
    },
    "quotient is integer (f_/~)" → forAll { (x: A, y: A) =>
      y.isZero || (x f_/~ y).toBigIntOpt.nonEmpty
    },
    "|r| < |y| (t_%)" → forAll { (x: A, y: A) =>
      y.isZero || {
        val r = x t_% y
        r.abs < y.abs
      }
    },
    "|r| < |y| (f_%)" → forAll { (x: A, y: A) =>
      y.isZero || {
        val r = x f_% y
        r.abs < y.abs
      }
    },
    "r = 0 or sign(r) = sign(x) (t_%)" → forAll { (x: A, y: A) =>
      y.isZero || {
        val r = x t_% y
        r.isZero || (r.sign === x.sign)
      }
    },
    "r = 0 or sign(r) = sign(y) (f_%)" → forAll { (x: A, y: A) =>
      y.isZero || {
        val r = x f_% y
        r.isZero || (r.sign === y.sign)
      }
    },
    "t_/~" → forAll { (x: A, y: A) =>
      y.isZero || {
        (x t_/% y)._1 === (x t_/~ y)
      }
    },
    "t_%" → forAll { (x: A, y: A) =>
      y.isZero || {
        (x t_/% y)._2 === (x t_% y)
      }
    },
    "f_/~" → forAll { (x: A, y: A) =>
      y.isZero || {
        (x f_/% y)._1 === (x f_/~ y)
      }
    },
    "f_%" → forAll { (x: A, y: A) =>
      y.isZero || {
        (x f_/% y)._2 === (x f_% y)
      }
    }
  )
}

// vim: expandtab:ts=2:sw=2
