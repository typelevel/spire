package spire.laws.shadows

import spire.algebra._

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