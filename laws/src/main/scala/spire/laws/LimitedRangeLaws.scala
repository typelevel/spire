package spire
package laws

import spire.algebra._
import spire.math.{Integral, NumberTag}
import spire.implicits._

import org.typelevel.discipline.{Laws, Predicate}

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

object LimitedRangeLaws {

  def apply[A:Eq:Arbitrary:Integral:NumberTag](implicit _pred: Predicate[A]) = new LimitedRangeLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def Tag = NumberTag[A]
    def Itg = Integral[A]
    def pred = _pred
  }
}

/** Syntax support for limited range forAll; the law provides not only a property to check,
  * but also a list of intermediate results that should be contained in the range of the
  * tested primitive type.
  */
trait LimitedRangeSyntax[A] {

  implicit def Arb: Arbitrary[A]
  implicit def Itg: Integral[A]
  implicit def Tag: NumberTag[A]

  /** Checks that `y`, given as an arbitrary length integer, fits into the tested type `A`. */
  def inRange(y: BigInt): Boolean = {
    Tag.hasMinValue.fold(true)(minVal => Itg.toBigInt(minVal) <= y) &&
    Tag.hasMaxValue.fold(true)(maxVal => y <= Itg.toBigInt(maxVal))
  }

  /** Convenient syntax suger to support different kinds of return types for the size list:
    * simple scalar, small tuples (size = 2,3,4) and seqs.
    */
  trait RangeCheck[S] {
    def check[B](b: B)(f: B => S): Boolean
  }

  object RangeCheck {

    def apply[S](implicit ev: RangeCheck[S]): RangeCheck[S] = ev

    implicit object scalar extends RangeCheck[BigInt] {
      def check[B](b: B)(f: B => BigInt) = inRange(f(b))
    }

    implicit object seq extends RangeCheck[Seq[BigInt]] {
      def check[B](b: B)(f: B => Seq[BigInt]) = f(b).forall(inRange)
    }

    implicit object tuple2 extends RangeCheck[(BigInt, BigInt)] {
      def check[B](b: B)(f: B => (BigInt, BigInt)) = {
        val (b1, b2) = f(b)
        inRange(b1) && inRange(b2)
      }
    }

    implicit object tuple3 extends RangeCheck[(BigInt, BigInt, BigInt)] {
      def check[B](b: B)(f: B => (BigInt, BigInt, BigInt)) = {
        val (b1, b2, b3) = f(b)
        inRange(b1) && inRange(b2) && inRange(b3)
      }
    }

    implicit object tuple4 extends RangeCheck[(BigInt, BigInt, BigInt, BigInt)] {
      def check[B](b: B)(f: B => (BigInt, BigInt, BigInt, BigInt)) = {
        val (b1, b2, b3, b4) = f(b)
        inRange(b1) && inRange(b2) && inRange(b3) && inRange(b4)
      }
    }

  }

  /** Shrinks the generated scalar by half. */
  def half(a: A): A = Itg.fromBigInt(Itg.toBigInt(a) / 2)

  type ToProp[X] = X => Prop

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: BigInt => S)(f: A => P): Prop = {
    lazy val safeGen = Arb.arbitrary.map { x =>
      @tailrec def rec(x1: A): A =
        if (RangeCheck[S].check(Itg.toBigInt(x1))(checks)) x1 else rec(half(x1))
      rec(x)
    }
    forAll(safeGen)(f)
  }

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: (BigInt, BigInt) => S)(f: (A, A) => P)(implicit d1: DummyImplicit): Prop = {
    lazy val safeGen = for {
      x <- Arb.arbitrary
      y <- Arb.arbitrary
    } yield {
      @tailrec def rec(x1: A, y1: A): (A, A) =
        if (RangeCheck[S].check((Itg.toBigInt(x1), Itg.toBigInt(y1)))(checks.tupled)) (x1, y1) else rec(half(x1), half(y1))
      rec(x, y)
    }
    forAll(safeGen)(f.tupled)
  }

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: (BigInt, BigInt, BigInt) => S)(f: (A, A, A) => P)(implicit d1: DummyImplicit, d2: DummyImplicit): Prop = {
    lazy val safeGen = for {
      x <- Arb.arbitrary
      y <- Arb.arbitrary
      z <- Arb.arbitrary
    } yield {
      @tailrec def rec(x1: A, y1: A, z1: A): (A, A, A) =
        if (RangeCheck[S].check((Itg.toBigInt(x1), Itg.toBigInt(y1), Itg.toBigInt(z1)))(checks.tupled)) (x1, y1, z1) else rec(half(x1), half(y1), half(z1))
      rec(x, y, z)
    }
    forAll(safeGen)(f.tupled)
  }

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: (BigInt, BigInt, BigInt, BigInt) => S)(f: (A, A, A, A) => P)(implicit d1: DummyImplicit, d2: DummyImplicit, d3: DummyImplicit): Prop = {
    lazy val safeGen = for {
      x <- Arb.arbitrary
      y <- Arb.arbitrary
      z <- Arb.arbitrary
      t <- Arb.arbitrary
    } yield {
      @tailrec def rec(x1: A, y1: A, z1: A, t1: A): (A, A, A, A) =
        if (RangeCheck[S].check((Itg.toBigInt(x1), Itg.toBigInt(y1), Itg.toBigInt(z1), Itg.toBigInt(t1)))(checks.tupled)) (x1, y1, z1, t1) else rec(half(x1), half(y1), half(z1), half(t1))
      rec(x, y, z, t)
    }
    forAll(safeGen)(f.tupled)
  }

}


/** Shadows tests from other Laws traits by forcing the generated arguments to have proper size
  * so that intermediate results fit in the tested type.
  * 
  * Contains only commutative variants (the primitive integers have commutative operations),
  * and merges under one trait the additive, multiplicate monoid/group parts.
  * As this is quite specialized, we aim for simplicitly, not generality.
  * 
  */
trait LimitedRangeLaws[A] extends LimitedRangeSyntax[A] with Laws {

  implicit override def Itg: Integral[A]
  implicit def Equ: Eq[A]

  def pred: Predicate[A]

  // BaseLaws

  def signed(implicit A: Signed[A]) = new SimpleRuleSet(
    name = "signed",
    "abs non-negative" → forAllLimitedRange(_.abs)( (x: A) =>
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
    "ordered group" → forAllLimitedRange( (x1, y1, z1) => (x1 + z1, y1 + z1) )( (x: A, y: A, z: A) =>
      (x <= y) ==> (x + z <= y + z)
    ),
    "triangle inequality" → forAllLimitedRange( (x1, y1) => (x1.abs + y1.abs) )( (x: A, y: A) =>
      (x + y).abs <= x.abs + y.abs
    )
  )

  def signedAdditiveAbGroup(implicit signedA: Signed[A], additiveAbGroupA: AdditiveAbGroup[A]) = new DefaultRuleSet(
    name = "signedAdditiveAbGroup",
    parent = Some(signedAdditiveCMonoid),
    "abs(x) equals abs(-x)" → forAllLimitedRange(_.abs)( (x: A) =>
      x.abs === (-x).abs
    )
  )

  def signedGCDRing(implicit signedA: Signed[A], gcdRingA: GCDRing[A]) = new DefaultRuleSet(
    name = "signedGCDRing",
    parent = Some(signedAdditiveAbGroup),
    "gcd(x, y) >= 0" → forAllLimitedRange( (x1, y1) => x1.gcd(y1) )( (x: A, y: A) =>
      (x gcd y).signum >= 0
    ),
    "gcd(x, 1) === 1" → forAll { (x: A) =>
      x.gcd(Ring[A].one) === Ring[A].one
    },
    "gcd(x, 0) === abs(x)" → forAllLimitedRange(_.abs)( (x: A) =>
      x.gcd(Ring[A].zero) === Signed[A].abs(x)
    ),
    "lcm(x, 1) === x" → forAllLimitedRange(_.abs)( (x: A) =>
      x.lcm(Ring[A].one) === x
    )
  )

  def additiveCSemigroup(implicit A: AdditiveSemigroup[A]) = new DefaultRuleSet(
    name = "additiveCSemigroup",
    parent = None,
    "associative +" → forAllLimitedRange( (x1, y1, z1) => (x1 + y1, y1 + z1, x1 + y1 + z1) )( (x: A, y: A, z: A) =>
      ((x + y) + z) === (x + (y + z))
    ),
    "commutative +" → forAllLimitedRange( (x1, y1) => x1 + y1 )( (x: A, y: A) =>
      (x + y) === (y + x)
    ),
    "sumN(a, 1) === a" → forAll( (a: A) =>
      A.sumN(a, 1) === a
    ),
    "sumN(a, 2) === a + a" → forAllLimitedRange( x1 => x1 + x1)( (a: A) =>
      A.sumN(a, 2) === (a + a)
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
    "minus consistent" → forAllLimitedRange( (x1, y1) => (x1 - y1, -y1) )( (x: A, y: A) =>
      (x - y) === (x + (-y))
    ),
    "left inverse +" → forAllLimitedRange(x1 => -x1)( (x: A) =>
      A.zero === ((-x) + x)
    ),
    "right inverse +" → forAllLimitedRange(x1 => -x1)( (x: A) =>
      A.zero === (x + (-x))
    )
  )

  def multiplicativeCSemigroup(implicit A: MultiplicativeSemigroup[A]) = new DefaultRuleSet(
    name = "multiplicativeCSemigroup",
    parent = None,
    "associative *" → forAllLimitedRange( (x1, y1, z1) => (x1 * y1, y1 * z1, x1 * y1 * z1) )( (x: A, y: A, z: A) =>
      ((x * y) * z) === (x * (y * z))
    ),
    "commutative *" → forAllLimitedRange( (x1, y1) => x1 * y1 )( (x: A, y: A) =>
      (x * y) === (y * x)
    ),
    "pow(a, 1) === a" → forAll((a: A) =>
      A.pow(a, 1) === a
    ),
    "pow(a, 2) === a + a" → forAllLimitedRange( x1 => x1 * x1)( (x: A) =>
      A.pow(x, 2) === (x * x)
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
    "distributive" → forAllLimitedRange( (x1, y1, z1) => (x1 * (y1 + z1), x1 * y1, x1 * z1) )( (x: A, y: A, z: A) =>
      (x * (y + z) === (x * y + x * z)) && (((x + y) * z) === (x * z + y * z))
    ),
    "pow" → forAllLimitedRange( x1 => (x1 * x1, x1 * x1 * x1) )( (x: A) =>
      ((x pow 1) === x) && ((x pow 2) === x * x) && ((x pow 3) === x * x * x)
    )
  )

  def cRing(implicit A: CRing[A]) = new RingProperties(
    name = "commutative ring",
    parents = Seq(cRig, additiveCMonoid, multiplicativeCMonoid)
  )

  def gcdRing(implicit A: GCDRing[A]) = new RingProperties(
    name = "gcd domain",
    parents = Seq(cRing),
    "gcd/lcm" → forAllLimitedRange( (x1, y1) => x1 * y1 ) { (x: A, y: A) =>
      val d = x gcd y
      val m = x lcm y
      x * y === d * m
    },
    "gcd is commutative" → forAllLimitedRange( (x1, y1) => (x1 gcd y1) ) { (x: A, y: A) =>
      (x gcd y) === (y gcd x)
    },
    "lcm is commutative" → forAllLimitedRange( (x1, y1) => (x1 lcm y1) ) { (x: A, y: A) =>
      (x lcm y) === (y lcm x)
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
    "submultiplicative function" → forAllLimitedRange( (x1, y1) => (x1 * y1) ) { (x: A, y: A) =>
      (pred(x) && pred(y)) ==> {
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
