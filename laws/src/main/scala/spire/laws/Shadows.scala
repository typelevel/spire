package spire.laws

import spire.algebra._
import spire.math.NumberTag

trait Shadowing[A, S] {
  def toShadow(a: A): S
  def fromShadow(s: S): Option[A]
  def isValid(s: S): Boolean = fromShadow(s).nonEmpty
  def checked(s: S): S =
    if (!isValid(s)) throw new IsEq.InvalidTestException else s
}

object Shadowing {
  def apply[A, S](f: A => S, g: S => Option[A]): Shadowing[A, S] = new Shadowing[A, S] {
    def toShadow(a: A): S = f(a)
    def fromShadow(s: S): Option[A] = g(s)
  }

  def bigInt[A:IsIntegral:NumberTag](fromBigInt: BigInt => A): Shadowing[A, BigInt] =
    new Shadowing[A, BigInt] {
      def toShadow(a: A): BigInt = IsIntegral[A].toBigInt(a)
      def fromShadow(s: BigInt): Option[A] = {
        NumberTag[A].hasMinValue match {
          case Some(m) if s < IsIntegral[A].toBigInt(m) => return None
          case _ =>
        }
        NumberTag[A].hasMaxValue match {
          case Some(m) if s > IsIntegral[A].toBigInt(m) => return None
          case _ =>
        }
        Some(fromBigInt(s))
      }
    }
}

/** Represents a primitive value `a: A` along with its shadow `s: S`  */
case class Shadow[A, S](a: A, s: S)

object Shadow {

  implicit def eqInstance[A, S](implicit A: Eq[A], S: Eq[S]): Eq[Shadow[A, S]] = new Eq[Shadow[A, S]] {
    def eqv(x: Shadow[A, S], y: Shadow[A, S]) = {
      val a = A.eqv(x.a, y.a)
      val s = S.eqv(x.s, y.s)
      if (a != s) throw new IllegalArgumentException("The Eq instances for the primitive and shadow value do not match")
      a
    }
  }

  def additiveSemigroup[A:AdditiveSemigroup,S:AdditiveSemigroup](implicit ev: Shadowing[A, S]): AdditiveSemigroup[Shadow[A, S]] =
    new ShadowAdditiveSemigroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveCSemigroup[A:AdditiveCSemigroup,S:AdditiveCSemigroup](implicit ev: Shadowing[A, S]): AdditiveCSemigroup[Shadow[A, S]] =
    new ShadowAdditiveCSemigroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveMonoid[A:AdditiveMonoid,S:AdditiveMonoid](implicit ev: Shadowing[A, S]): AdditiveMonoid[Shadow[A, S]] =
    new ShadowAdditiveMonoid[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveCMonoid[A:AdditiveCMonoid,S:AdditiveCMonoid](implicit ev: Shadowing[A, S]): AdditiveCMonoid[Shadow[A, S]] =
    new ShadowAdditiveCMonoid[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveGroup[A:AdditiveGroup,S:AdditiveGroup](implicit ev: Shadowing[A, S]): AdditiveGroup[Shadow[A, S]] =
    new ShadowAdditiveGroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveAbGroup[A:AdditiveAbGroup,S:AdditiveAbGroup](implicit ev: Shadowing[A, S]): AdditiveAbGroup[Shadow[A, S]] =
    new ShadowAdditiveAbGroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

}

trait ShadowAdditiveSemigroup[A, S] extends AdditiveSemigroup[Shadow[A, S]] {
  implicit val shadowing: Shadowing[A, S]
  import shadowing._
  implicit def A: AdditiveSemigroup[A]
  implicit def S: AdditiveSemigroup[S]


  def plus(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.plus(x.a, y.a), checked(S.plus(x.s, y.s)))

  override def sumN(x: Shadow[A, S], n: Int): Shadow[A, S] =
    Shadow(A.sumN(x.a, n), checked(S.sumN(x.s, n)))

  override def trySum(xs: TraversableOnce[Shadow[A, S]]): Option[Shadow[A, S]] = {
    val seq = xs.toSeq
    val aO = A.trySum( seq.map(_.a) )
    val sO = S.trySum( seq.map(_.s) )
    (aO, sO) match {
      case (Some(a), Some(s)) => Some(Shadow(a, checked(s)))
      case (None, None) => None
      case _ => throw new IllegalArgumentException("Inconsistent results for trySum between primitive and shadow type")
    }
  }
}

trait ShadowAdditiveCSemigroup[A, S] extends AdditiveCSemigroup[Shadow[A, S]] with ShadowAdditiveSemigroup[A, S] {
  implicit def A: AdditiveCSemigroup[A]
  implicit def S: AdditiveCSemigroup[S]
}

trait ShadowAdditiveMonoid[A, S] extends AdditiveMonoid[Shadow[A, S]] with ShadowAdditiveSemigroup[A, S] {
  import shadowing._
  implicit def A: AdditiveMonoid[A]
  implicit def S: AdditiveMonoid[S]

  def zero: Shadow[A, S] = Shadow(A.zero, checked(S.zero))

  override def sum(xs: TraversableOnce[Shadow[A, S]]): Shadow[A, S] = {
    val seq = xs.toSeq
    val a = A.sum(seq.map(_.a))
    val s = S.sum(seq.map(_.s))
    Shadow(a, checked(s))
  }
}

trait ShadowAdditiveCMonoid[A, S] extends AdditiveCMonoid[Shadow[A, S]]
  with ShadowAdditiveMonoid[A, S] with ShadowAdditiveCSemigroup[A, S] {
  implicit def A: AdditiveCMonoid[A]
  implicit def S: AdditiveCMonoid[S]
}

trait ShadowAdditiveGroup[A, S] extends AdditiveGroup[Shadow[A, S]] with ShadowAdditiveMonoid[A, S] {
  import shadowing._
  implicit def A: AdditiveGroup[A]
  implicit def S: AdditiveGroup[S]

  def negate(x: Shadow[A, S]): Shadow[A, S] = Shadow(A.negate(x.a), checked(S.negate(x.s)))
}

trait ShadowAdditiveAbGroup[A, S] extends AdditiveAbGroup[Shadow[A, S]]
  with ShadowAdditiveGroup[A, S] with ShadowAdditiveCMonoid[A, S] {
  implicit def A: AdditiveAbGroup[A]
  implicit def S: AdditiveAbGroup[S]
}
