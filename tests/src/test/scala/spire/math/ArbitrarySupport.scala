package spire.math

import spire.algebra._

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

object ArbitrarySupport {

  object Ordinal {
    trait _0
    trait _1
    trait _2
    trait _3
    trait _4
    trait _5
    trait _6
    trait _7
    trait _8
    trait _9
    trait _10
    trait _20
    trait _50
    trait _100
  }

  abstract class Size[A](val value: Int)

  object Size {
    import Ordinal._

    implicit object Size0 extends Size[_0](0)
    implicit object Size1 extends Size[_1](1)
    implicit object Size2 extends Size[_2](2)
    implicit object Size3 extends Size[_3](3)
    implicit object Size4 extends Size[_4](4)
    implicit object Size5 extends Size[_5](5)
    implicit object Size6 extends Size[_6](6)
    implicit object Size7 extends Size[_3](7)
    implicit object Size8 extends Size[_3](8)
    implicit object Size9 extends Size[_3](9)
    implicit object Size10 extends Size[_10](10)
    implicit object Size20 extends Size[_20](20)
    implicit object Size50 extends Size[_50](50)
    implicit object Size100 extends Size[_100](100)

    def apply[A](implicit sz: Size[A]): Int = sz.value
  }

  case class Sized[A, L, U](num: A)

  case class Positive[A](num: A)
  case class Negative[A](num: A)
  case class NonZero[A](num: A)
  case class NonPositive[A](num: A)
  case class NonNegative[A](num: A)

  import spire.syntax.all._

  implicit def sized[A: EuclideanRing: Signed: Arbitrary, L: Size, U: Size]: Arbitrary[Sized[A, L, U]] =
    Arbitrary(arbitrary[A].map(a => Sized((a % (Size[U] - Size[L])).abs + Size[L])))

  implicit def positive[A: Signed: Arbitrary]: Arbitrary[Positive[A]] =
    Arbitrary(arbitrary[A].map(_.abs).filter(_.signum > 0).map(Positive(_)))
  implicit def negative[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[Negative[A]] =
    Arbitrary(arbitrary[A].map(-_.abs).filter(_.signum < 0).map(Negative(_)))
  implicit def nonZero[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[NonZero[A]] =
    Arbitrary(arbitrary[A].filter(_.signum != 0).map(NonZero(_)))
  implicit def nonPositive[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[NonPositive[A]] =
    Arbitrary(arbitrary[A].map(-_.abs).filter(_.signum < 1).map(NonPositive(_)))
  implicit def nonNegative[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[NonNegative[A]] =
    Arbitrary(arbitrary[A].map(_.abs).filter(_.signum > -1).map(NonNegative(_)))

  implicit val ubyte: Arbitrary[UByte] =
    Arbitrary(arbitrary[Byte].map(n => UByte(n)))

  implicit val ushort: Arbitrary[UShort] =
    Arbitrary(arbitrary[Short].map(n => UShort(n)))

  implicit val uint: Arbitrary[UInt] =
    Arbitrary(arbitrary[Int].map(n => UInt(n)))

  implicit val ulong: Arbitrary[ULong] =
    Arbitrary(arbitrary[Long].map(n => ULong(n)))

  implicit val natural: Arbitrary[Natural] =
    Arbitrary(arbitrary[BigInt].map(n => Natural(n.abs)))

  implicit val safeLong: Arbitrary[SafeLong] =
    Arbitrary(arbitrary[BigInt].map(n => SafeLong(n)))

  implicit val rational: Arbitrary[Rational] =
    Arbitrary(for {
      n <- arbitrary[Long]
      d <- arbitrary[Long].filter(_ != 0)
    } yield {
      Rational(n, d)
    })

  implicit val real: Arbitrary[Real] =
    Arbitrary(arbitrary[Rational].map(n => Real(n)))

  implicit val cf: Arbitrary[CF] =
    Arbitrary(arbitrary[Rational].map(n => CF(n)))

  implicit def complex[A: Arbitrary: Fractional: Signed: Trig]: Arbitrary[Complex[A]] =
    Arbitrary(for {
      re <- arbitrary[A]
      im <- arbitrary[A]
    } yield {
      Complex(re, im)
    })

  implicit def quaternion[A: Arbitrary: Fractional: Signed: Trig]: Arbitrary[Quaternion[A]] =
    Arbitrary(for {
      r <- arbitrary[A]
      i <- arbitrary[A]
      j <- arbitrary[A]
      k <- arbitrary[A]
    } yield {
      Quaternion(r, i, j, k)
    })

  implicit def interval[A: Arbitrary: Order: AdditiveMonoid]: Arbitrary[Interval[A]] = {
    Arbitrary(for {
      n <- arbitrary[Double]
      lower <- arbitrary[A]
      upper <- arbitrary[A]
    } yield {
      if (n < 0.05) Interval.all[A]
      else if (n < 0.10) Interval.above(lower)
      else if (n < 0.15) Interval.atOrAbove(lower)
      else if (n < 0.20) Interval.below(upper)
      else if (n < 0.25) Interval.atOrBelow(upper)
      else if (n < 0.50) Interval.open(lower, upper)
      else if (n < 0.60) Interval.openBelow(lower, upper)
      else if (n < 0.70) Interval.openAbove(lower, upper)
      else Interval.closed(lower, upper)
    })
  }
}
