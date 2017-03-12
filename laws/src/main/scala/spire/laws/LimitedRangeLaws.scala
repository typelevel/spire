package spire
package laws

import spire.algebra._
import spire.math.{Integral, NumberTag}
import spire.implicits._

import org.typelevel.discipline.{Laws, Predicate}

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object LimitedRangeLaws {
  // Integral is 
  def apply[A:Eq:Arbitrary:Integral:NumberTag](implicit _pred: Predicate[A]) = new LimitedRangeLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def Tag = NumberTag[A]
    def Itg = Integral[A]
    def pred = _pred
  }
}



trait LimitedRangeSyntax[A] {

  implicit def Itg: Integral[A]
  implicit def Tag: NumberTag[A]

  trait SeqAdapter[S] extends Function1[S, Seq[BigInt]]

  object SeqAdapter {

    def apply[S](implicit ev: SeqAdapter[S]): SeqAdapter[S] = ev

    implicit object seq extends SeqAdapter[Seq[BigInt]] {
      def apply(s: Seq[BigInt]) = s
    }

    implicit object bigInt extends SeqAdapter[BigInt] {
      def apply(b: BigInt) = Seq(b)
    }

    implicit object tuple2 extends SeqAdapter[(BigInt, BigInt)] {
      def apply(b: (BigInt, BigInt)) = Seq(b._1, b._2)
    }

    implicit object tuple3 extends SeqAdapter[(BigInt, BigInt, BigInt)] {
      def apply(b: (BigInt, BigInt, BigInt)) = Seq(b._1, b._2, b._3)
    }

    implicit object tuple4 extends SeqAdapter[(BigInt, BigInt, BigInt, BigInt)] {
      def apply(b: (BigInt, BigInt, BigInt, BigInt)) = Seq(b._1, b._2, b._3, b._4)
    }

  }

  def isInRange[S:SeqAdapter](s: S) = SeqAdapter[S].apply(s).forall(isBigIntInRange)

  def isBigIntInRange(y: BigInt): Boolean = {
    Tag.hasMinValue.fold(true)(minVal => Itg.toBigInt(minVal) <= y) &&
    Tag.hasMaxValue.fold(true)(maxVal => y <= Itg.toBigInt(maxVal))
  }

  def inRange[S:SeqAdapter](x1: A)(f: BigInt => S): Boolean =
    isInRange(f(Itg.toBigInt(x1)))

  def inRange[S:SeqAdapter](x1: A, x2: A)(f: (BigInt, BigInt) => S): Boolean =
    isInRange(f(Itg.toBigInt(x1), Itg.toBigInt(x2)))

  def inRange[S:SeqAdapter](x1: A, x2: A, x3: A)(f: (BigInt, BigInt, BigInt) => S): Boolean =
    isInRange(f(Itg.toBigInt(x1), Itg.toBigInt(x2), Itg.toBigInt(x3)))

}

/** Shadows tests from other Laws traits by allowing the property test only when the
  * intermediate results are in the range of the tested type. 
  */
trait LimitedRangeLaws[A] extends LimitedRangeSyntax[A] with Laws {

  implicit override def Itg: Integral[A]
  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def pred: Predicate[A]

  // BaseLaws

  def signed(implicit A: Signed[A]) = new SimpleRuleSet(
    name = "signed",
    "abs non-negative" → forAll((x: A) =>
      inRange(x)(x1 => Seq(x1.abs)) ==>
        (x.abs.sign != Sign.Negative)
    ),
    "signum returns -1/0/1" → forAll((x: A) =>
      x.signum.abs <= 1
    ),
    "signum is sign.toInt" → forAll((x: A) =>
      x.signum == x.sign.toInt
    )
  )

  def signedAdditiveCMonoid(implicit signedA: Signed[A], additiveCMonoidA: AdditiveCMonoid[A]) = new DefaultRuleSet(
    name = "signedAdditiveCMonoid",
    parent = Some(signed),
    "ordered group" → forAll { (x: A, y: A, z: A) =>
      inRange(x, y, z) { (x1, y1, z1) => (x1 + z1, y1 + z1) } ==>
      ((x <= y) ==> (x + z <= y + z))
    },
    "triangle inequality" → forAll { (x: A, y: A) =>
      inRange(x, y) { (x1, y1) => (x1.abs + y1.abs) } ==>
      ((x + y).abs <= x.abs + y.abs)
    }
  )

  def signedAdditiveAbGroup(implicit signedA: Signed[A], additiveAbGroupA: AdditiveAbGroup[A]) = new DefaultRuleSet(
    name = "signedAdditiveAbGroup",
    parent = Some(signedAdditiveCMonoid),
    "abs(x) equals abs(-x)" → forAll { (x: A) =>
      inRange(x) { y => y.abs } ==>
      (x.abs === (-x).abs)
    }
  )

  def signedGCDRing(implicit signedA: Signed[A], gcdRingA: GCDRing[A]) = new DefaultRuleSet(
    name = "signedGCDRing",
    parent = Some(signedAdditiveAbGroup),
    "gcd(x, y) >= 0" → forAll { (x: A, y: A) =>
      inRange(x, y) { (x1, y1) => x1.gcd(y1) } ==>
      ((x gcd y).signum >= 0)
    },
    "gcd(x, 1) === 1" → forAll { (x: A) =>
      x.gcd(Ring[A].one) === Ring[A].one
    },
    "gcd(x, 0) === abs(x)" → forAll { (x: A) =>
      inRange(x) { y => y.abs } ==>
      (x.gcd(Ring[A].zero) === Signed[A].abs(x))
    },
    "lcm(x, 1) === x" → forAll { (x: A) =>
      inRange(x) { y => y.abs } ==>
      (x.lcm(Ring[A].one) === x)
    }
  )

  def additiveCSemigroup(implicit A: AdditiveSemigroup[A]) = new DefaultRuleSet(
    name = "additiveCSemigroup",
    parent = None,
    "associative +" → forAll((x: A, y: A, z: A) =>
      inRange(x, y, z) { (x1, y1, z1) => (x1 + y1, y1 + z1, x1 + y1 + z1) } ==>
        (((x + y) + z) === (x + (y + z)))
    ),
    "commutative +" → forAll((x: A, y: A) =>
      inRange(x, y) { (x1, y1) => x1 + y1 } ==>
        ((x + y) === (y + x))
    ),
    "sumN(a, 1) === a" → forAll((a: A) =>
      A.sumN(a, 1) === a
    ),
    "sumN(a, 2) === a + a" → forAll((a: A) =>
      inRange(a) { a1 => a1 + a1 } ==>
        (A.sumN(a, 2) === (a + a))
    )
  )

  def additiveCMonoid(implicit A: AdditiveCMonoid[A]) = new DefaultRuleSet(
    name = "additiveCMonoid",
    parent = Some(additiveCSemigroup),
    "left identity +" → forAll((x: A) =>
      (A.zero + x) === x
    ),
    "right identity +" → forAll((x: A) =>
      (x + A.zero) === x
    ),
    "sumN(a, 0) === 0" → forAll((a: A) =>
      A.sumN(a, 0) === A.zero
    ),
    "sum(Nil) === 0" → forAll((a: A) =>
      A.sum(Nil) === A.zero
    ),
    "isZero" → forAll((a: A) =>
      a.isZero === (a === A.zero)
    )
  )

  def additiveAbGroup(implicit A: AdditiveGroup[A]) = new DefaultRuleSet(
    name = "additiveAbGroup",
    parent = Some(additiveCMonoid),
    "minus consistent" → forAll((x: A, y: A) =>
      inRange(x, y) { (x1, y1) => (x1 - y1, -y1) } ==>
        ((x - y) === (x + (-y)))
    ),
    "left inverse +" → forAll((x: A) =>
      inRange(x) { x1 => -x1 } ==>
        (A.zero === ((-x) + x))
    ),
    "right inverse +" → forAll((x: A) =>
      inRange(x) { x1 => -x1 } ==>
        (A.zero === (x + (-x)))
    )
  )

  def multiplicativeCSemigroup(implicit A: MultiplicativeSemigroup[A]) = new DefaultRuleSet(
    name = "multiplicativeCSemigroup",
    parent = None,
    "associative *" → forAll((x: A, y: A, z: A) =>
      inRange(x, y, z) { (x1, y1, z1) => (x1 * y1, y1 * z1, x1 * y1 * z1) } ==>
        (((x * y) * z) === (x * (y * z)))
    ),
    "commutative *" → forAll((x: A, y: A) =>
      inRange(x, y) { (x1, y1) => x1 * y1 } ==>
        ((x * y) === (y * x))
    ),
    "pow(a, 1) === a" → forAll((a: A) =>
      A.pow(a, 1) === a
    ),
    "pow(a, 2) === a + a" → forAll((a: A) =>
      inRange(a) { a1 => a1 * a1 } ==>
        (A.pow(a, 2) === (a * a))
    )
  )

  def multiplicativeCMonoid(implicit A: MultiplicativeCMonoid[A]) = new DefaultRuleSet(
    name = "multiplicativeCMonoid",
    parent = Some(multiplicativeCSemigroup),
    "left identity *" → forAll((x: A) =>
      (A.one * x) === x
    ),
    "right identity *" → forAll((x: A) =>
      (x * A.one) === x
    ),
    "pow(a, 0) === 0" → forAll((a: A) =>
      A.pow(a, 0) === A.one
    ),
    "product(Nil) === 0" → forAll((a: A) =>
      A.product(Nil) === A.one
    ),
    "isOne" → forAll((a: A) =>
      a.isOne === (a === A.one)
    )
  )

  def cRig(implicit A: CRig[A]) = new RingProperties(
    name = "commutative rig",
    parents = Seq(additiveCMonoid, multiplicativeCMonoid),
    "distributive" → forAll((x: A, y: A, z: A) =>
      inRange(x, y, z) { (x1, y1, z1) => (x1 * (y1 + z1), x1 * y1, x1 * z1) } ==>
        ((x * (y + z) === (x * y + x * z)) && (((x + y) * z) === (x * z + y * z)))
    ),
    "pow" → forAll((x: A) =>
      inRange(x) { x1 => (x1 * x1, x1 * x1 * x1) } ==>
        (((x pow 1) === x) && ((x pow 2) === x * x) && ((x pow 3) === x * x * x))
    )
  )

  def cRing(implicit A: CRing[A]) = new RingProperties(
    name = "commutative ring",
    parents = Seq(cRig, additiveCMonoid, multiplicativeCMonoid)
  )

  def gcdRing(implicit A: GCDRing[A]) = new RingProperties(
    name = "gcd domain",
    parents = Seq(cRing),
    "gcd/lcm" → forAll { (x: A, y: A) =>
      inRange(x, y) { (x1, y1) => x1 * y1 } ==> {
        val d = x gcd y
        val m = x lcm y
        x * y === d * m
      }
    },
    "gcd is commutative" → forAll { (x: A, y: A) =>
      inRange(x, y) { (x1, y1) => (x1 gcd y1) } ==>
      ((x gcd y) === (y gcd x))
    },
    "lcm is commutative" → forAll { (x: A, y: A) =>
      inRange(x, y) { (x1, y1) => (x1 lcm y1) } ==>
      ((x lcm y) === (y lcm x))
    },
    "gcd(0, 0)" → ((A.zero gcd A.zero) === A.zero),
    "lcm(0, 0) === 0" → ((A.zero lcm A.zero) === A.zero),
    "lcm(x, 0) === 0" → forAll { (x: A) => (x lcm A.zero) === A.zero }
  )

  def euclideanRing(implicit A: EuclideanRing[A]) = new RingProperties(
    name = "euclidean ring",
    parents = Seq(gcdRing),
    "euclidean division rule" → forAll { (x: A, y: A) =>
      pred(y) ==> {
        val (q, r) = x /% y
        x === (y * q + r)
      }
    },
    "equot" → forAll { (x: A, y: A) =>
      pred(y) ==> {
        (x /% y)._1 === (x /~ y)
      }
    },
    "emod" → forAll { (x: A, y: A) =>
      pred(y) ==> {
        (x /% y)._2 === (x % y)
      }
    },
    "euclidean function" → forAll { (x: A, y: A) =>
      pred(y) ==> {
        val (q, r) = x /% y
        r.isZero || (r.euclideanFunction < y.euclideanFunction)
      }
    },
    "submultiplicative function" → forAll { (x: A, y: A) =>
      (pred(x) && pred(y) && inRange(x, y) { (x1, y1) => (x1 * y1) }) ==> {
        x.euclideanFunction <= (x * y).euclideanFunction
      }
    }
  )

  class RingProperties(
    val name: String,
    val parents: Seq[RuleSet],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Nil
  }

}
