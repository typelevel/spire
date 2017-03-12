package spire
package laws

import spire.algebra._
import spire.math.{Integral, NumberTag, UByte, UShort, UInt, ULong}
import spire.implicits._

import org.typelevel.discipline.{Laws, Predicate}

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop._

/** Self-contained description for limited range types. */
trait LimitedRange[A] { self =>

  import LimitedRange.RangeCheck

  implicit def Tag: NumberTag[A]

  /** Conversion for limited range type `A` to `BigInt`. Is total. */
  def toBigInt(a: A): BigInt
  def toBigInt(t: (A, A)): (BigInt, BigInt) = (toBigInt(t._1), toBigInt(t._2))
  def toBigInt(t: (A, A, A)): (BigInt, BigInt, BigInt) = (toBigInt(t._1), toBigInt(t._2), toBigInt(t._3))
  def toBigInt(t: (A, A, A, A)): (BigInt, BigInt, BigInt, BigInt) = (toBigInt(t._1), toBigInt(t._2), toBigInt(t._3), toBigInt(t._4))

  /** Conversion to limited range type `A` from `BigInt`. Is not necessarily total,
    * but we must have `fromBigInt(toBigInt(a)) === a` and
    * `fromBigInt(toBigInt(a)/2)` should always be defined.
    */
  def fromBigInt(b: BigInt): A

  /** Shrinks the generated scalar by half. */
  def half(a: A): A = fromBigInt(toBigInt(a)/2)
  def half(t: (A, A)): (A, A) = (half(t._1), half(t._2))
  def half(t: (A, A, A)): (A, A, A) = (half(t._1), half(t._2), half(t._3))
  def half(t: (A, A, A, A)): (A, A, A, A) = (half(t._1), half(t._2), half(t._3), half(t._4))

  /** Checks that `y`, given as an arbitrary length integer, fits into the tested type `A`. */
  def inRange(y: BigInt): Boolean = {
    Tag.hasMinValue.fold(true)(minVal => toBigInt(minVal) <= y) &&
    Tag.hasMaxValue.fold(true)(maxVal => y <= toBigInt(maxVal))
  }

  /** Returns a generator for `A` that passes the provided range checks. */
  def restrictedGen1[S:RangeCheck](checks: BigInt => S)(implicit arb: Arbitrary[A]): Gen[A] =
    arb.arbitrary.map { x =>
      @tailrec def rec(x1: A): A =
        if (RangeCheck[S].check(self, toBigInt(x1))(checks)) x1 else rec(half(x1))
      rec(x)
    }

  /** Returns a generator for `(A, A)` that passes the provided range checks. */
  def restrictedGen2[S:RangeCheck](checks: (BigInt, BigInt) => S)(implicit arb: Arbitrary[A]): Gen[(A, A)] =
    for (x <- arb.arbitrary; y <- arb.arbitrary) yield {
      @tailrec def rec(t: (A, A)): (A, A) =
        if (RangeCheck[S].check(self, toBigInt(t))(checks.tupled)) t else rec(half(t))
      rec((x, y))
    }

  /** Returns a generator for `(A, A, A)` that passes the provided range checks. */
  def restrictedGen3[S:RangeCheck](checks: (BigInt, BigInt, BigInt) => S)(implicit arb: Arbitrary[A]): Gen[(A, A, A)] =
    for (x <- arb.arbitrary; y <- arb.arbitrary; z <- arb.arbitrary) yield {
      @tailrec def rec(t: (A, A, A)): (A, A, A) =
        if (RangeCheck[S].check(self, toBigInt(t))(checks.tupled)) t else rec(half(t))
      rec((x, y, z))
    }

  /** Returns a generator for `(A, A, A)` that passes the provided range checks. */
  def restrictedGen4[S:RangeCheck](checks: (BigInt, BigInt, BigInt, BigInt) => S)(implicit arb: Arbitrary[A]): Gen[(A, A, A, A)] =
    for (x <- arb.arbitrary; y <- arb.arbitrary; z <- arb.arbitrary; w <- arb.arbitrary) yield {
      @tailrec def rec(t: (A, A, A, A)): (A, A, A, A) =
        if (RangeCheck[S].check(self, toBigInt(t))(checks.tupled)) t else rec(half(t))
      rec((x, y, z, w))
    }

}

object LimitedRange {

  def apply[A](implicit ev: LimitedRange[A]): LimitedRange[A] = ev

  /** Creates a `LimitedRange` description from a `Integral` and `NumberTag` implicits. */
  implicit def fromIntegral[A](implicit A: Integral[A], Tag0: NumberTag[A]): LimitedRange[A] =
    new LimitedRange[A] {
      def toBigInt(a: A) = A.toBigInt(a)
      def fromBigInt(b: BigInt) = A.fromBigInt(b)
      def Tag = Tag0
    }

  /** Create a `LimitedRange` description with the provided `NumberTag` and conversion functions. */
  def apply[A](f: A => BigInt, g: BigInt => A)(implicit Tag0: NumberTag[A]): LimitedRange[A] =
    new LimitedRange[A] {
      def toBigInt(a: A) = f(a)
      def fromBigInt(b: BigInt) = g(b)
      def Tag = Tag0
    }

  implicit val uByte: LimitedRange[UByte] = apply(_.toBigInt, b => UByte(b.toInt))
  implicit val uShort: LimitedRange[UShort] = apply(_.toBigInt, b => UShort(b.toInt))
  implicit val uInt: LimitedRange[UInt] = apply(_.toBigInt, b => UInt(b.toLong))
  implicit val uLong: LimitedRange[ULong] = apply(_.toBigInt, ULong.fromBigInt)

  /** Convenient syntax sugar to support different kinds of return types for the size list:
    * simple scalar, small tuples (size = 2,3,4) and seqs.
    */
  trait RangeCheck[S] {
    def check[T](range: LimitedRange[_], tuple: T)(f: T => S): Boolean
  }

  object RangeCheck {

    def apply[S](implicit ev: RangeCheck[S]): RangeCheck[S] = ev

    implicit object scalar extends RangeCheck[BigInt] {
      def check[T](range: LimitedRange[_], tuple: T)(f: T => BigInt) = range.inRange(f(tuple))
    }

    implicit object seq extends RangeCheck[Seq[BigInt]] {
      def check[T](range: LimitedRange[_], tuple: T)(f: T => Seq[BigInt]) = f(tuple).forall(range.inRange)
    }

    implicit object tuple2 extends RangeCheck[(BigInt, BigInt)] {
      def check[T](range: LimitedRange[_], tuple: T)(f: T => (BigInt, BigInt)) = {
        import range.inRange
        val (b1, b2) = f(tuple)
        inRange(b1) && inRange(b2)
      }
    }

    implicit object tuple3 extends RangeCheck[(BigInt, BigInt, BigInt)] {
      def check[T](range: LimitedRange[_], tuple: T)(f: T => (BigInt, BigInt, BigInt)) = {
        import range.inRange
        val (b1, b2, b3) = f(tuple)
        inRange(b1) && inRange(b2) && inRange(b3)
      }
    }

    implicit object tuple4 extends RangeCheck[(BigInt, BigInt, BigInt, BigInt)] {
      def check[T](range: LimitedRange[_], tuple: T)(f: T => (BigInt, BigInt, BigInt, BigInt)) = {
        import range.inRange
        val (b1, b2, b3, b4) = f(tuple)
        inRange(b1) && inRange(b2) && inRange(b3) && inRange(b4)
      }
    }

  }

}


object LimitedRangeLaws {

  def apply[A:Eq:Arbitrary:LimitedRange](implicit _pred: Predicate[A]) = new LimitedRangeLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def LR = LimitedRange[A]
    def pred = _pred
  }

}

/** Syntax support for limited range forAll; the law provides not only a property to check,
  * but also a list of intermediate results that should be contained in the range of the
  * tested primitive type.
  */
trait LimitedRangeSyntax[A] {

  import LimitedRange.RangeCheck

  implicit def LR: LimitedRange[A]

  type ToProp[X] = X => Prop

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: BigInt => S)(f: A => P)(implicit arb: Arbitrary[A]): Prop = {
    forAll(LR.restrictedGen1(checks))(f)
  }

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: (BigInt, BigInt) => S)(f: (A, A) => P)(implicit arb: Arbitrary[A], d1: DummyImplicit): Prop = {
    forAll(LR.restrictedGen2(checks))(f.tupled)
  }

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: (BigInt, BigInt, BigInt) => S)(f: (A, A, A) => P)(implicit arb: Arbitrary[A], d1: DummyImplicit, d2: DummyImplicit): Prop = {
    forAll(LR.restrictedGen3(checks))(f.tupled)
  }

  def forAllLimitedRange[S:RangeCheck, P:ToProp](checks: (BigInt, BigInt, BigInt, BigInt) => S)(f: (A, A, A, A) => P)(implicit arb: Arbitrary[A], d1: DummyImplicit, d2: DummyImplicit, d3: DummyImplicit): Prop = {
    forAll(LR.restrictedGen4(checks))(f.tupled)
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

  implicit def Arb: Arbitrary[A]
  implicit def Equ: Eq[A]

  def pred: Predicate[A]

  // OrderLaws

  def partialOrder(implicit A: PartialOrder[A]) = new DefaultRuleSet(
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

  def order(implicit A: Order[A]) = new DefaultRuleSet(
    name = "order",
    parent = Some(partialOrder),
    "totality" → forAll((x: A, y: A) =>
      x <= y || y <= x
    )
  )


  def signed(implicit A: Signed[A]) = new DefaultRuleSet(
    name = "signed",
    parent = Some(order),
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

  def truncatedDivision(implicit cRingA: CRing[A], truncatedDivisionA: TruncatedDivision[A]) = new DefaultRuleSet(
    name = "truncatedDivision",
    parent = Some(signed),
    "division rule (t_/%)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        val (q, r) = x t_/% y
        x === y * q + r
      }
    },
    "division rule (f_/%)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        val (q, r) = x f_/% y
        x === y * q + r
      }
    },
    "quotient is integer (t_/~)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || (x t_/~ y).toBigIntOpt.nonEmpty
    },
    "quotient is integer (f_/~)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || (x f_/~ y).toBigIntOpt.nonEmpty
    },
    "|r| < |y| (t_%)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        val r = x t_% y
        r.abs < y.abs
      }
    },
    "|r| < |y| (f_%)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        val r = x f_% y
        r.abs < y.abs
      }
    },
    "r = 0 or sign(r) = sign(x) (t_%)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        val r = x t_% y
        r.isZero || (r.sign === x.sign)
      }
    },
    "r = 0 or sign(r) = sign(y) (f_%)" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        val r = x f_% y
        r.isZero || (r.sign === y.sign)
      }
    },
    "t_/~" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        (x t_/% y)._1 === (x t_/~ y)
      }
    },
    "t_%" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        (x t_/% y)._2 === (x t_% y)
      }
    },
    "f_/~" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        (x f_/% y)._1 === (x f_/~ y)
      }
    },
    "f_%" → forAllLimitedRange( (x1, y1) => (x1.abs, y1.abs) ) { (x: A, y: A) =>
      y.isZero || {
        (x f_/% y)._2 === (x f_% y)
      }
    }
  )

  // CombinationLaws

  def signedAdditiveCMonoid(implicit signedA: Signed[A], additiveCMonoidA: AdditiveCMonoid[A]) = new DefaultRuleSet(
    name = "signedAdditiveCMonoid",
    parent = None,
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

  // Additive/Multiplicative group laws and ring laws

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

  def additiveAbGroup(implicit A: AdditiveAbGroup[A]) = new DefaultRuleSet(
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
