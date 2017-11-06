package spire.laws

import spire.algebra.{MultiplicativeMonoid, Eq}

trait MultiplicativeMonoidLaws[A] extends MultiplicativeSemigroupLaws[A] {
  override implicit def S: MultiplicativeMonoid[A]

  def leftOne(a: A): IsEq[A] =
    S.times(S.one, a) <=> a

  def rightOne(a: A): IsEq[A] =
    S.times(a, S.one) <=> a

  def pow0(a: A): IsEq[A] =
    S.pow(a, 0) <=> S.one

  def product: IsEq[A] =
    S.product(Nil) <=> S.one

  def isOne(a: A, eqv: Eq[A]): IsEq[Boolean] =
    eqv.eqv(a, S.one) <=> S.isOne(a)(eqv)
}

object MultiplicativeMonoidLaws {
  def apply[A](implicit ev: MultiplicativeMonoid[A]): MultiplicativeMonoidLaws[A] =
    new MultiplicativeMonoidLaws[A] { def S: MultiplicativeMonoid[A] = ev }
}
