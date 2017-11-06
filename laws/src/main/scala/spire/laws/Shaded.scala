package spire.laws

import spire.algebra._
import spire.laws

trait Shadowing[A, S] {
  def toShadow(a: A): S
  def fromShadow(s: S): Option[A]
  def isValid(s: S): Boolean = fromShadow(s).nonEmpty
  def checked(s: S): S =
    if (!isValid(s)) throw new laws.IsEq.InvalidTestException else s
}

object Shadowing {
  def apply[A, S](f: A => S, g: S => Option[A]): Shadowing[A, S] = new Shadowing[A, S] {
    def toShadow(a: A): S = f(a)
    def fromShadow(s: S): Option[A] = g(s)
  }
}

/** Represents a primitive value `a: A` along with its shadow `s: S`  */
case class Shaded[A, S](a: A, s: S)

object Shaded {

  implicit def eqInstance[A, S](implicit A: Eq[A], S: Eq[S]): Eq[Shaded[A, S]] = new Eq[Shaded[A, S]] {
    def eqv(x: Shaded[A, S], y: Shaded[A, S]) = {
      val a = A.eqv(x.a, y.a)
      val s = S.eqv(x.s, y.s)
      if (a != s) throw new IllegalArgumentException("The Eq instances for the primitive and shadow value do not match")
      a
    }
  }

  def additiveSemigroup[A:AdditiveSemigroup,S:AdditiveSemigroup](implicit ev: Shadowing[A, S]): AdditiveSemigroup[Shaded[A, S]] =
    new ShadedAdditiveSemigroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveCSemigroup[A:AdditiveCSemigroup,S:AdditiveCSemigroup](implicit ev: Shadowing[A, S]): AdditiveCSemigroup[Shaded[A, S]] =
    new ShadedAdditiveCSemigroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveMonoid[A:AdditiveMonoid,S:AdditiveMonoid](implicit ev: Shadowing[A, S]): AdditiveMonoid[Shaded[A, S]] =
    new ShadedAdditiveMonoid[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveCMonoid[A:AdditiveCMonoid,S:AdditiveCMonoid](implicit ev: Shadowing[A, S]): AdditiveCMonoid[Shaded[A, S]] =
    new ShadedAdditiveCMonoid[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveGroup[A:AdditiveGroup,S:AdditiveGroup](implicit ev: Shadowing[A, S]): AdditiveGroup[Shaded[A, S]] =
    new ShadedAdditiveGroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

  def additiveAbGroup[A:AdditiveAbGroup,S:AdditiveAbGroup](implicit ev: Shadowing[A, S]): AdditiveAbGroup[Shaded[A, S]] =
    new ShadedAdditiveAbGroup[A, S] { def A = implicitly; def S = implicitly; val shadowing = ev }

}

trait ShadedAdditiveSemigroup[A, S] extends AdditiveSemigroup[Shaded[A, S]] {
  implicit val shadowing: Shadowing[A, S]
  import shadowing._
  implicit def A: AdditiveSemigroup[A]
  implicit def S: AdditiveSemigroup[S]


  def plus(x: Shaded[A, S], y: Shaded[A, S]): Shaded[A, S] =
    Shaded(A.plus(x.a, y.a), checked(S.plus(x.s, y.s)))

  override def sumN(x: Shaded[A, S], n: Int): Shaded[A, S] =
    Shaded(A.sumN(x.a, n), checked(S.sumN(x.s, n)))

  override def trySum(xs: TraversableOnce[Shaded[A, S]]): Option[Shaded[A, S]] = {
    val seq = xs.toSeq
    val aO = A.trySum( seq.map(_.a) )
    val sO = S.trySum( seq.map(_.s) )
    (aO, sO) match {
      case (Some(a), Some(s)) => Some(Shaded(a, checked(s)))
      case (None, None) => None
      case _ => throw new IllegalArgumentException("Inconsistent results for trySum between primitive and shadow type")
    }
  }
}

trait ShadedAdditiveCSemigroup[A, S] extends AdditiveCSemigroup[Shaded[A, S]] with ShadedAdditiveSemigroup[A, S] {
  implicit def A: AdditiveCSemigroup[A]
  implicit def S: AdditiveCSemigroup[S]
}

trait ShadedAdditiveMonoid[A, S] extends AdditiveMonoid[Shaded[A, S]] with ShadedAdditiveSemigroup[A, S] {
  import shadowing._
  implicit def A: AdditiveMonoid[A]
  implicit def S: AdditiveMonoid[S]

  def zero: Shaded[A, S] = Shaded(A.zero, checked(S.zero))

  override def sum(xs: TraversableOnce[Shaded[A, S]]): Shaded[A, S] = {
    val seq = xs.toSeq
    val a = A.sum(seq.map(_.a))
    val s = S.sum(seq.map(_.s))
    Shaded(a, checked(s))
  }
}

trait ShadedAdditiveCMonoid[A, S] extends AdditiveCMonoid[Shaded[A, S]]
  with ShadedAdditiveMonoid[A, S] with ShadedAdditiveCSemigroup[A, S] {
  implicit def A: AdditiveCMonoid[A]
  implicit def S: AdditiveCMonoid[S]
}

trait ShadedAdditiveGroup[A, S] extends AdditiveGroup[Shaded[A, S]] with ShadedAdditiveMonoid[A, S] {
  import shadowing._
  implicit def A: AdditiveGroup[A]
  implicit def S: AdditiveGroup[S]

  def negate(x: Shaded[A, S]): Shaded[A, S] = Shaded(A.negate(x.a), checked(S.negate(x.s)))
}

trait ShadedAdditiveAbGroup[A, S] extends AdditiveAbGroup[Shaded[A, S]]
  with ShadedAdditiveGroup[A, S] with ShadedAdditiveCMonoid[A, S] {
  implicit def A: AdditiveAbGroup[A]
  implicit def S: AdditiveAbGroup[S]
}
