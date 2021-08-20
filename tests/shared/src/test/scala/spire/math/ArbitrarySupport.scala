package spire
package math

import spire.algebra._

import org.scalacheck.Arbitrary._
import org.scalacheck._
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

    implicit object Size0SpireImplicit extends Size[_0](0)
    implicit object Size1SpireImplicit extends Size[_1](1)
    implicit object Size2SpireImplicit extends Size[_2](2)
    implicit object Size3SpireImplicit extends Size[_3](3)
    implicit object Size4SpireImplicit extends Size[_4](4)
    implicit object Size5SpireImplicit extends Size[_5](5)
    implicit object Size6SpireImplicit extends Size[_6](6)
    implicit object Size7SpireImplicit extends Size[_3](7)
    implicit object Size8SpireImplicit extends Size[_3](8)
    implicit object Size9SpireImplicit extends Size[_3](9)
    implicit object Size10SpireImplicit extends Size[_10](10)
    implicit object Size20SpireImplicit extends Size[_20](20)
    implicit object Size50SpireImplicit extends Size[_50](50)
    implicit object Size100SpireImplicit extends Size[_100](100)

    def apply[A](implicit sz: Size[A]): Int = sz.value
  }

  case class Sized[A, L, U](num: A)

  case class Positive[A](num: A)
  case class Negative[A](num: A)
  case class NonZero[A](num: A)
  case class NonPositive[A](num: A)
  case class NonNegative[A](num: A)

  import spire.syntax.all._

  implicit def sizedSpireImplicit[A: EuclideanRing: Signed: Arbitrary, L: Size, U: Size]: Arbitrary[Sized[A, L, U]] =
    Arbitrary(arbitrary[A].map(a => Sized(a.emod(Size[U] - Size[L]).abs() + Size[L])))

  implicit def positiveSpireImplicit[A: Signed: Arbitrary]: Arbitrary[Positive[A]] =
    Arbitrary(arbitrary[A].map(_.abs()).filter(_.signum() > 0).map(Positive(_)))
  implicit def negativeSpireImplicit[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[Negative[A]] =
    Arbitrary(arbitrary[A].map(-_.abs()).filter(_.signum() < 0).map(Negative(_)))
  implicit def nonZeroSpireImplicit[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[NonZero[A]] =
    Arbitrary(arbitrary[A].filter(_.signum() != 0).map(NonZero(_)))
  implicit def nonPositiveSpireImplicit[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[NonPositive[A]] =
    Arbitrary(arbitrary[A].map(-_.abs()).filter(_.signum() < 1).map(NonPositive(_)))
  implicit def nonNegativeSpireImplicit[A: Signed: AdditiveGroup: Arbitrary]: Arbitrary[NonNegative[A]] =
    Arbitrary(arbitrary[A].map(_.abs()).filter(_.signum() > -1).map(NonNegative(_)))
}
