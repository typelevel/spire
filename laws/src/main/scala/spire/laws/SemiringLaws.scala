package spire.laws

import spire.algebra.Semiring

trait SemiringLaws[A] extends AdditiveCMonoidLaws[A] with MultiplicativeSemigroupLaws[A] {
  override implicit def S: Semiring[A]

  def leftDistributive(x: A, y: A, z: A): IsEq[A] =
    S.times(x, S.plus(y, z)) <=> S.plus(S.times(x, y), S.times(x, z))

  def rightDistributive(x: A, y: A, z: A): IsEq[A] =
    S.times(S.plus(x, y), z) <=> S.plus(S.times(x, z), S.times(y, z))

  def zeroLeftAnnihilates(a: A): IsEq[A] =
    S.times(S.zero, a) <=> S.zero

  def zeroRightAnnihilates(a: A): IsEq[A] =
    S.times(a, S.zero) <=> S.zero
}

object SemiringLaws {
  def apply[A](implicit ev: Semiring[A]): SemiringLaws[A] =
    new SemiringLaws[A] { def S: Semiring[A] = ev }
}
