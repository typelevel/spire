package spire
package laws

import spire.algebra._
import spire.implicits._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import InvalidTestException._

object OrderLaws {
  def apply[A: Eq: Arbitrary] = new OrderLaws[A] {
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
    "reflexitivity" -> forAllSafe((x: A) => x <= x),
    "antisymmetry" -> forAllSafe((x: A, y: A) => (x <= y && y <= x).imp(x === y)),
    "transitivity" -> forAllSafe((x: A, y: A, z: A) => (x <= y && y <= z).imp(x <= z)),
    "gteqv" -> forAllSafe((x: A, y: A) => (x <= y) === (y >= x)),
    "lt" -> forAllSafe((x: A, y: A) => (x < y) === (x <= y && x =!= y)),
    "gt" -> forAllSafe((x: A, y: A) => (x < y) === (y > x))
  )

  def order(implicit A: Order[A]) = new OrderProperties(
    name = "order",
    parent = Some(partialOrder),
    "totality" -> forAllSafe((x: A, y: A) => x <= y || y <= x)
  )

  def signed(implicit A: Signed[A]) = new OrderProperties(
    name = "signed",
    parent = Some(order),
    "abs non-negative" -> forAllSafe((x: A) => x.abs.sign != Sign.Negative),
    "signum returns -1/0/1" -> forAllSafe((x: A) => x.signum.abs <= 1),
    "signum is sign.toInt" -> forAllSafe((x: A) => x.signum == x.sign.toInt)
  )

  def truncatedDivision(implicit cRigA: CRig[A], truncatedDivisionA: TruncatedDivision[A]) = new DefaultRuleSet(
    name = "truncatedDivision",
    parent = Some(signed),
    "division rule (tquotmod)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        val (q, r) = x.tquotmod(y)
        x === y * q + r
      }
    },
    "division rule (fquotmod)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        val (q, r) = x.fquotmod(y)
        x == y * q + r
      }
    },
    "quotient is integer (tquot)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || x.tquot(y).toBigIntOpt.nonEmpty
    },
    "quotient is integer (fquot)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || x.fquot(y).toBigIntOpt.nonEmpty
    },
    "|r| < |y| (tmod)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        val r = x.tmod(y)
        r.abs < y.abs
      }
    },
    "|r| < |y| (fmod)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        val r = x.fmod(y)
        r.abs < y.abs
      }
    },
    "r = 0 or sign(r) = sign(x) (tmod)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        val r = x.tmod(y)
        r.isZero || (r.sign === x.sign)
      }
    },
    "r = 0 or sign(r) = sign(y) (fmod)" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        val r = x.fmod(y)
        r.isZero || (r.sign === y.sign)
      }
    },
    "tquot" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        x.tquotmod(y)._1 === (x.tquot(y))
      }
    },
    "tmod" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        x.tquotmod(y)._2 === (x.tmod(y))
      }
    },
    "fquot" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        x.fquotmod(y)._1 === (x.fquot(y))
      }
    },
    "fmod" -> forAllSafe { (x: A, y: A) =>
      y.isZero || {
        x.fquotmod(y)._2 === (x.fmod(y))
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
