package spire.laws

import scala.reflect.ClassTag

import spire.algebra._
import spire.math._

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

object SpireArbitrary {

  implicit def UByteArbitrary: Arbitrary[UByte] =
    Arbitrary(arbitrary[Byte].map(new UByte(_)))

  implicit def UShortArbitrary: Arbitrary[UShort] =
    Arbitrary(arbitrary[Short].map(n => new UShort(n.toChar)))

  implicit def UIntArbitrary: Arbitrary[UInt] =
    Arbitrary(arbitrary[Int].map(new UInt(_)))

  implicit def ULongArbitrary: Arbitrary[ULong] =
    Arbitrary(arbitrary[Long].map(new ULong(_)))

  implicit def FixedScale: Arbitrary[FixedScale] =
    Arbitrary(arbitrary[Short].map(n => new FixedScale(n & 0xffff)))

  implicit def FixedPoint: Arbitrary[FixedPoint] =
    Arbitrary(arbitrary[Long].map(n => new FixedPoint(n)))

  implicit def SafeLongArbitrary: Arbitrary[SafeLong] =
    Arbitrary(Gen.oneOf(
      arbitrary[Long].map(SafeLong(_)),
      arbitrary[BigInt].map(SafeLong(_))))

  implicit def NaturalArbitrary: Arbitrary[Natural] =
    Arbitrary(Gen.oneOf(
      arbitrary[Long].map(n => Natural(n & Long.MaxValue)),
      arbitrary[BigInt].map(n => Natural(n.abs))))

  implicit def RationalArbitrary: Arbitrary[Rational] =
    Arbitrary(Gen.oneOf(
      arbitrary[Long] map (Rational(_)),
      arbitrary[Double] map (Rational(_))))

  implicit def NumberArbitrary: Arbitrary[Number] =
    Arbitrary(Gen.oneOf(
      arbitrary[Long].map(Number(_)),
      arbitrary[Double].map(Number(_)),
      arbitrary[BigDecimal].map(Number(_)),
      arbitrary[Rational].map(Number(_))))

  implicit def AlgebraicArbitrary: Arbitrary[Algebraic] =
    Arbitrary(arbitrary[Int] map (Algebraic(_)))

  implicit def RealArbitrary: Arbitrary[Real] =
    Arbitrary(arbitrary[Rational] map (Real(_)))

  implicit def VectorArbitrary[A: Arbitrary]: Arbitrary[Vector[A]] =
    Arbitrary(arbitrary[List[A]] map (Vector(_: _*)))

  implicit def SignArbitrary[A: Arbitrary]: Arbitrary[Sign] =
    Arbitrary(Gen.oneOf(Sign.Positive, Sign.Zero, Sign.Negative))

  implicit def TermArbitrary[A: Arbitrary]: Arbitrary[poly.Term[A]] =
    Arbitrary(for {
      e <- arbitrary[Short].map(_.toInt)
      c <- arbitrary[A]
    } yield poly.Term(c, e))

  implicit def PolynomialArbitrary[A: Arbitrary: Semiring: Eq: ClassTag]: Arbitrary[Polynomial[A]] =
    Arbitrary(arbitrary[List[poly.Term[A]]].map(ts => Polynomial(ts.take(6))))

  implicit def ComplexArbitrary[A: Arbitrary]: Arbitrary[Complex[A]] =
    Arbitrary(for {
      r <- arbitrary[A]
      i <- arbitrary[A]
    } yield Complex(r, i))

  // hardcoded to infinitesimal dimension=2 :/
  implicit def JetArbitrary[A: Arbitrary: ClassTag]: Arbitrary[Jet[A]] =
    Arbitrary(for {
      r <- arbitrary[A]
      inf0 <- arbitrary[A]
      inf1 <- arbitrary[A]
    } yield Jet(r, Array(inf0, inf1)))

  implicit def QuaternionArbitrary[A: Arbitrary]: Arbitrary[Quaternion[A]] =
    Arbitrary(for {
      r <- arbitrary[A]
      i <- arbitrary[A]
      j <- arbitrary[A]
      k <- arbitrary[A]
    } yield Quaternion(r, i, j, k))

  implicit def BoundArbitrary[A: Arbitrary]: Arbitrary[Interval.Bound[A]] =
    Arbitrary(Gen.oneOf(
      arbitrary[A].map(Interval.Open(_)),
      arbitrary[A].map(Interval.Closed(_)),
      arbitrary[Unit].map(_ => Interval.Unbound[A])))

  implicit def IntervalArbitrary[A: Arbitrary: Order: AdditiveMonoid]: Arbitrary[Interval[A]] =
    Arbitrary(for {
      lower <- arbitrary[Interval.Bound[A]]
      upper <- arbitrary[Interval.Bound[A]]
    } yield Interval.fromBounds(lower, upper))
}

// vim: expandtab:ts=2:sw=2
