package spire.laws

import spire.algebra.{Monoid, Eq}

trait MonoidLaws[A] extends SemigroupLaws[A] {
  override implicit def S: Monoid[A]

  def leftEmpty(a: A): IsEq[A] =
    S.combine(S.empty, a) <=> a

  def rightEmpty(a: A): IsEq[A] =
    S.combine(a, S.empty) <=> a

  def combine0(a: A): IsEq[A] =
    S.combineN(a, 0) <=> S.empty

  def combineAll: IsEq[A] =
    S.combineAll(Nil) <=> S.empty

  def isEmpty(a: A, eqv: Eq[A]): IsEq[Boolean] =
    eqv.eqv(a, S.empty) <=> S.isEmpty(a)(eqv)
}

object MonoidLaws {
  def apply[A](implicit ev: Monoid[A]): MonoidLaws[A] =
    new MonoidLaws[A] { def S: Monoid[A] = ev }
}
