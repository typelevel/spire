package spire.laws

import spire.algebra.{AdditiveMonoid, Eq}

trait AdditiveMonoidLaws[A] extends AdditiveSemigroupLaws[A] {
  override implicit def S: AdditiveMonoid[A]

  def leftZero(a: A): IsEq[A] =
    S.plus(S.zero, a) <=> a

  def rightZero(a: A): IsEq[A] =
    S.plus(a, S.zero) <=> a

  def sumN0(a: A): IsEq[A] =
    S.sumN(a, 0) <=> S.zero

  def sumAll: IsEq[A] =
    S.sum(Nil) <=> S.zero

  def isZero(a: A, eqv: Eq[A]): IsEq[Boolean] =
    eqv.eqv(a, S.zero) <=> S.isZero(a)(eqv)
}

object AdditiveMonoidLaws {
  def apply[A](implicit ev: AdditiveMonoid[A]): AdditiveMonoidLaws[A] =
    new AdditiveMonoidLaws[A] { def S: AdditiveMonoid[A] = ev }
}
