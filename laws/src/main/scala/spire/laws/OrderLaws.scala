package spire
package laws

import spire.algebra._
import spire.math.NumberTag
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object OrderLaws {
  def apply[A : Eq : Arbitrary] = new OrderLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait OrderLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def partialOrder(implicit A: PartialOrder[A]) = new OrderProperties(
    name = "partialOrder",
    parent = None,
    "reflexitivity" → forAll((x: A) =>
      x <= x
    ),
    "antisymmetry" → forAll((x: A, y: A) =>
      (x <= y && y <= x) imp (x === y)
    ),
    "transitivity" → forAll((x: A, y: A, z: A) =>
      (x <= y && y <= z) imp (x <= z)
    ),
    "gteqv" → forAll((x: A, y: A) =>
      (x <= y) === (y >= x)
    ),
    "lt" → forAll((x: A, y: A) =>
      (x < y) === (x <= y && x =!= y)
    ),
    "gt" → forAll((x: A, y: A) =>
      (x < y) === (y > x)
    )
  )

  def order(implicit A: Order[A]) = new OrderProperties(
    name = "order",
    parent = Some(partialOrder),
    "totality" → forAll((x: A, y: A) =>
      x <= y || y <= x
    )
  )

  def signed(implicit A: Signed[A]) = new OrderProperties(
    name = "signed",
    parent = Some(order),
    "abs non-negative" → forAll((x: A) =>
      x.abs.sign != Sign.Negative
    ),
    "signum returns -1/0/1" → forAll((x: A) =>
      x.signum.abs <= 1
    ),
    "signum is sign.toInt" → forAll((x: A) =>
      x.signum == x.sign.toInt
    )
  )

  def signedAdditiveCMonoid(implicit signedA: Signed[A], additiveCMonoidA: AdditiveCMonoid[A]) =
    new OrderProperties(
      name = "signedAdditiveCMonoid",
      parent = Some(signed),
      "ordered group" → forAll { (x: A, y: A, z: A) =>
        (x <= y) ==> (x + z <= y + z)
      },
      "triangle inequality" → forAll { (x: A, y: A) =>
        (x + y).abs <= x.abs + y.abs
      }
    )

  def signedAdditiveAbGroup(implicit signedA: Signed[A], additiveAbGroupA: AdditiveAbGroup[A]) =
    new OrderProperties(
      name = "signedAdditiveAbGroup",
      parent = Some(signedAdditiveCMonoid),
      "abs(x) equals abs(-x)" → forAll { (x: A) =>
        x.abs === (-x).abs
      }
    )

  def truncatedDivision(implicit cRingA: CRing[A], truncatedDivisionA: TruncatedDivision[A]) =
    new OrderProperties(
      name = "truncatedDivision",
      parent = Some(signedAdditiveAbGroup),
      "division rule (tquotmod)" → forAll { (x: A, y: A) =>
        y.isZero || {
          val (q, r) = x tquotmod y
          x === y * q + r
        }
      },
      "division rule (fquotmod)" → forAll { (x: A, y: A) =>
        y.isZero || {
          val (q, r) = x fquotmod y
          x === y * q + r
        }
      },
      "quotient is integer (tquot)" → forAll { (x: A, y: A) =>
        y.isZero || (x tquot y).toBigIntOption.nonEmpty
      },
      "quotient is integer (fquot)" → forAll { (x: A, y: A) =>
        y.isZero || (x fquot y).toBigIntOption.nonEmpty
      },
      "|r| < |y| (tmod)" → forAll { (x: A, y: A) =>
        y.isZero || {
          val r = x tmod y
          r.abs < y.abs
        }
      },
      "|r| < |y| (fmod)" → forAll { (x: A, y: A) =>
        y.isZero || {
          val r = x fmod y
          r.abs < y.abs
        }
      },
      "r = 0 or sign(r) = sign(x) (tmod)" → forAll { (x: A, y: A) =>
        y.isZero || {
          val r = x tmod y
          r.isZero || (r.sign === x.sign)
        }
      },
      "r = 0 or sign(r) = sign(y) (fmod)" → forAll { (x: A, y: A) =>
        y.isZero || {
          val r = x fmod y
          r.isZero || (r.sign === y.sign)
        }
      },
      "tquot" → forAll { (x: A, y: A) =>
        y.isZero || {
          (x tquotmod y)._1 === (x tquot y)
        }
      },
      "tmod" → forAll { (x: A, y: A) =>
        y.isZero || {
          (x tquotmod y)._2 === (x tmod y)
        }
      },
      "fquot" → forAll { (x: A, y: A) =>
        y.isZero || {
          (x fquotmod y)._1 === (x fquot y)
        }
      },
      "fmod" → forAll { (x: A, y: A) =>
        y.isZero || {
          (x fquotmod y)._2 === (x fmod y)
        }
      }
    )

  def inRange(bi: BigInt)(implicit isIntegral: IsIntegral[A], numberTag: NumberTag[A]): Boolean = {
    val isTooLow = numberTag.hasMinValue.map(bi < isIntegral.toBigInt(_)).getOrElse(false)
    val isTooHigh = numberTag.hasMaxValue.map(bi > isIntegral.toBigInt(_)).getOrElse(false)
    (!isTooLow) && (!isTooHigh)
  }

  def tbi(a: A)(implicit isIntegral: IsIntegral[A]): BigInt = isIntegral.toBigInt(a)

  // TODO: in all the laws below, we do not discard evaluations by using `==>` because
  // it leads to too many evaluations discarded, to be fixed
  // i.e. instead of P ==> X, we write !P || X
  def signedLimitedRange(implicit signedA: Signed[A], isIntegral: IsIntegral[A], numberTag: NumberTag[A]) = new OrderProperties(
    name = "signed (limited range)",
    parent = Some(order),
    "abs non-negative" → forAll((x: A) =>
      !inRange(tbi(x).abs) || (x.abs.sign != Sign.Negative)
    ),
    "signum returns -1/0/1" → forAll((x: A) =>
      x.signum.abs <= 1
    ),
    "signum is sign.toInt" → forAll((x: A) =>
      x.signum == x.sign.toInt
    )
  )


  def signedAdditiveCMonoidLimitedRange(implicit signedA: Signed[A], additiveCMonoidA: AdditiveCMonoid[A], isIntegral: IsIntegral[A], numberTag: NumberTag[A]) =
    new OrderProperties(
      name = "signedAdditiveCMonoid (limited range)",
      parent = Some(signedLimitedRange),
      "ordered group" → forAll { (x: A, y: A, z: A) =>
        !(inRange(tbi(x) + tbi(z)) && inRange(tbi(y) + tbi(z))) || {
          (x <= y) ==> (x + z <= y + z)
        }
      },
      "triangle inequality" → forAll { (x: A, y: A) =>
        val lhsInRange = inRange(tbi(x) + tbi(y)) && inRange((tbi(x) + tbi(y)).abs)
        val rhsInRange = inRange(tbi(x).abs) && inRange(tbi(y).abs) && inRange(tbi(x).abs + tbi(y).abs)
        !(lhsInRange && rhsInRange) || ( (x + y).abs <= x.abs + y.abs )
      }
    )

  def signedAdditiveAbGroupLimitedRange(implicit signedA: Signed[A], additiveAbGroupA: AdditiveAbGroup[A], isIntegral: IsIntegral[A], numberTag: NumberTag[A]) =
    new OrderProperties(
      name = "signedAdditiveAbGroup (limited range)",
      parent = Some(signedAdditiveCMonoid),
      "abs(x) equals abs(-x)" → forAll { (x: A) =>
        !(inRange(tbi(x).abs) && inRange(-tbi(x))) || (x.abs === (-x).abs)
      }
    )

  // TODO: have finer-grained rejection of overflowing operations
  def wellDefinedQuotMods(x: A, y: A)(implicit isIntegral: IsIntegral[A], numberTag: NumberTag[A]): Boolean = {
    val xi = tbi(x)
    val yi = tbi(y)
    if (yi.isZero) false else {
      val (tq, tm) = xi tquotmod yi
      val (fq, fm) = xi fquotmod yi
      inRange(yi.abs) && // for the ... < |y| tests
      (inRange(tq) && inRange(tm) && inRange(tm.abs) &&
        inRange(fq) && inRange(fm) && inRange(fm.abs))
    }
  }

  def truncatedDivisionLimitedRange(implicit cRigA: CRig[A], truncatedDivisionA: TruncatedDivision[A], isIntegral: IsIntegral[A], numberTag: NumberTag[A]) =
    new OrderProperties(
      name = "truncatedDivision (limited range)",
      parent = Some(signedAdditiveCMonoidLimitedRange), // TODO: propose variant with abelian additive
      "division rule (tquotmod)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          val (q, r) = x tquotmod y
          x === y * q + r
        }
      },
      "division rule (fquotmod)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          val (q, r) = x fquotmod y
          x === y * q + r
        }
      },
      "quotient is integer (tquot)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || (x tquot y).toBigIntOption.nonEmpty
      },
      "quotient is integer (fquot)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || (x fquot y).toBigIntOption.nonEmpty
      },
      "|r| < |y| (tmod)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          val r = x tmod y
          r.abs < y.abs
        }
      },
      "|r| < |y| (fmod)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          val r = x fmod y
          r.abs < y.abs
        }
      },
      "r = 0 or sign(r) = sign(x) (tmod)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          val r = x tmod y
          r.isZero || (r.sign === x.sign)
        }
      },
      "r = 0 or sign(r) = sign(y) (fmod)" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          val r = x fmod y
          r.isZero || (r.sign === y.sign)
        }
      },
      "tquot" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          (x tquotmod y)._1 === (x tquot y)
        }
      },
      "tmod" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          (x tquotmod y)._2 === (x tmod y)
        }
      },
      "fquot" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          (x fquotmod y)._1 === (x fquot y)
        }
      },
      "fmod" → forAll { (x: A, y: A) =>
        !wellDefinedQuotMods(x, y) || {
          (x fquotmod y)._2 === (x fmod y)
        }
      }
    )

  class OrderProperties(
    name: String,
    parent: Option[OrderProperties],
    props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props: _*)

}

// vim: expandtab:ts=2:sw=2
