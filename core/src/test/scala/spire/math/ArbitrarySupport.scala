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
      n <- arbitrary[BigInt]
      d <- arbitrary[BigInt].filter(_ != 0)
    } yield {
      Rational(n, d)
    })

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
