package spire
package laws

import java.math.BigInteger

import spire.math.extras.{FixedPoint, FixedScale}

import spire.algebra._
import spire.algebra.free._
import spire.math._
import spire.math.interval.{Bound, Closed, Open, Unbound}
import spire.optional.Perm
import spire.syntax.fastFor.fastForRange
import spire.syntax.order._

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

object gen {

  lazy val ubyte: Gen[UByte] =
    arbitrary[Byte].map(new UByte(_))

  lazy val ushort: Gen[UShort] =
    arbitrary[Short].map(n => new UShort(n.toChar))

  lazy val uint: Gen[UInt] =
    arbitrary[Int].map(new UInt(_))

  lazy val ulong: Gen[ULong] =
    arbitrary[Long].map(new ULong(_))

  lazy val trilean: Gen[Trilean] =
    Gen.oneOf(Trilean.True, Trilean.False, Trilean.Unknown)

  lazy val fixedScale: Gen[FixedScale] =
    arbitrary[Short].map(n => new FixedScale(n & 0xffff))

  lazy val fixedPoint: Gen[FixedPoint] =
    arbitrary[Long].map(new FixedPoint(_))

  lazy val bigInteger: Gen[BigInteger] =
    arbitrary[BigInt].map(_.bigInteger)

  lazy val safeLong: Gen[SafeLong] =
    Gen.frequency(1 -> SafeLong(BigInt("393050634124102232869567034555427371542904833")),
                  100 -> arbitrary[Long].map(SafeLong(_)),
                  100 -> arbitrary[BigInt].map(SafeLong(_))
    )

  lazy val natural: Gen[Natural] =
    Gen.oneOf(arbitrary[Long].map(n => Natural(n & Long.MaxValue)), arbitrary[BigInt].map(n => Natural(n.abs)))

  lazy val rational: Gen[Rational] = {
    val rationalFromLongs: Gen[Rational] =
      for {
        n <- arbitrary[Long]
        d <- arbitrary[Long].map(n => if (n == 0) 1L else n)
      } yield Rational(n, d)

    val rationalFromSafeLongs: Gen[Rational] =
      for {
        n <- safeLong
        d <- safeLong.map(n => if (n.isZero) SafeLong.one else n)
      } yield Rational(n, d)

    val bigRational: Gen[Rational] = {
      val m = Rational("1/393050634124102232869567034555427371542904833")
      rationalFromSafeLongs.map(_ * m)
    }

    Gen.frequency(
      10 -> rationalFromLongs, // we keep this to make long/long rationals more frequent
      10 -> arbitrary[Double].map(n => Rational(n)),
      1 -> rationalFromSafeLongs,
      1 -> bigRational, // a rational that is guaranteed to have a big denominator
      1 -> bigRational.map(x => if (x.isZero) Rational.one else x.inverse)
    )
  }

  lazy val number: Gen[Number] =
    Gen.oneOf(arbitrary[Long].map(Number(_)),
              arbitrary[Double].map(Number(_)),
              arbitrary[BigDecimal].map(Number(_)),
              rational.map(Number(_))
    )

  lazy val algebraic: Gen[Algebraic] =
    arbitrary[Int].map(Algebraic(_))

  lazy val real: Gen[Real] =
    rational.map(Real(_))

  lazy val sign: Gen[Sign] =
    Gen.oneOf(Sign.Positive, Sign.Zero, Sign.Negative)

  def term[A: Arbitrary]: Gen[poly.Term[A]] =
    for {
      e <- Gen.chooseNum[Short](0, Short.MaxValue)
      c <- arbitrary[A]
    } yield poly.Term(c, e.toInt)

  def polynomial[A: Arbitrary: Semiring: Eq: ClassTag]: Gen[Polynomial[A]] =
    for {
      ts <- Gen.listOf(term[A])
    } yield Polynomial(ts.take(6))

  def complex[A: Arbitrary]: Gen[Complex[A]] =
    for {
      r <- arbitrary[A]
      i <- arbitrary[A]
    } yield Complex(r, i)

  def jet2[A: Arbitrary: ClassTag]: Gen[Jet[A]] =
    for {
      r <- arbitrary[A]
      inf0 <- arbitrary[A]
      inf1 <- arbitrary[A]
    } yield Jet(r, Array(inf0, inf1))

  def jet[A: Arbitrary: ClassTag]: Gen[Jet[A]] =
    for {
      r <- arbitrary[A]
      infs <- arbitrary[Array[A]]
    } yield Jet(r, infs)

  def quaternion[A: Arbitrary]: Gen[Quaternion[A]] =
    for {
      r <- arbitrary[A]
      i <- arbitrary[A]
      j <- arbitrary[A]
      k <- arbitrary[A]
    } yield Quaternion(r, i, j, k)

  def bound[A: Arbitrary]: Gen[Bound[A]] =
    Gen.oneOf(arbitrary[A].map(Open(_)), arbitrary[A].map(Closed(_)), Gen.const(Unbound[A]()))

  def bounds[A: Arbitrary: Order]: Gen[(A, A)] =
    arbitrary[(A, A)].map { case (x, y) => if (x <= y) (x, y) else (y, x) }

  def makeBoundedInterval[A: Arbitrary: Order](f: (A, A) => Interval[A]): Gen[Interval[A]] =
    bounds[A].map { case (l, u) => f(l, u) }

  def openInterval[A: Arbitrary: Order]: Gen[Interval[A]] =
    makeBoundedInterval[A](Interval.open(_, _))

  def openLowerInterval[A: Arbitrary: Order]: Gen[Interval[A]] =
    makeBoundedInterval[A](Interval.openLower(_, _))

  def openUpperInterval[A: Arbitrary: Order]: Gen[Interval[A]] =
    makeBoundedInterval[A](Interval.openUpper(_, _))

  def closedInterval[A: Arbitrary: Order]: Gen[Interval[A]] =
    makeBoundedInterval[A](Interval.closed(_, _))

  def boundedInterval[A: Arbitrary: Order]: Gen[Interval[A]] =
    Gen.oneOf(openInterval[A], openLowerInterval[A], openUpperInterval[A], closedInterval[A])

  def interval[A: Arbitrary: Order]: Gen[Interval[A]] =
    Gen.frequency[Interval[A]](
      (1, Gen.const(Interval.all[A])),
      (1, arbitrary[A].map(Interval.above(_))),
      (1, arbitrary[A].map(Interval.atOrAbove(_))),
      (1, arbitrary[A].map(Interval.below(_))),
      (1, arbitrary[A].map(Interval.atOrBelow(_))),
      (15, boundedInterval[A])
    )

  def freeMonoid[A: Arbitrary]: Gen[FreeMonoid[A]] =
    for {
      as <- arbitrary[List[A]]
    } yield as.foldLeft(FreeMonoid.empty[A]) { (acc, a) =>
      acc |+| FreeMonoid(a)
    }

  def freeGroup[A: Arbitrary]: Gen[FreeGroup[A]] =
    for {
      aas <- arbitrary[List[Either[A, A]]]
    } yield aas.foldLeft(FreeGroup.id[A]) {
      case (acc, Left(a))  => acc |-| FreeGroup(a)
      case (acc, Right(a)) => acc |+| FreeGroup(a)
    }

  def freeAbGroup[A: Arbitrary]: Gen[FreeAbGroup[A]] =
    for {
      tpls <- arbitrary[List[(A, Short)]]
    } yield tpls.foldLeft(FreeAbGroup.id[A]) { case (acc, (a, n)) =>
      acc |+| Group[FreeAbGroup[A]].combineN(FreeAbGroup(a), n.toInt)
    }

  val perm: Gen[Perm] =
    Gen
      .parameterized { params =>
        val domainSize = params.size / 10 + 1
        Gen.containerOfN[Array, Int](domainSize, Gen.chooseNum(0, Int.MaxValue))
      }
      .flatMap { intArray =>
        val domainSize = intArray.length
        val images = new Array[Int](domainSize)
        fastForRange(0 until domainSize) { i =>
          val j = intArray(i) % (i + 1) // uses the Fisher-Yates shuffle, inside out variant
          images(i) = images(j)
          images(j) = i
        }
        Perm(images.zipWithIndex.filter { case (p, i) => p != i }.toMap)
      }
}
