package spire.laws

import spire.algebra.DivisionRing

trait DivisionRingLaws[A] extends RingLaws[A] {
  override implicit def S: DivisionRing[A]

  // small duplication from multiplicative group, because the multiplicative structure
  // is a monoid over all elements, and a group over the nonzero elements

  def leftReciprocal(x: NonZero[A]): IsEq[A] =
    S.times(S.reciprocal(x.a), x.a) <=> S.one

  def rightReciprocal(x: NonZero[A]): IsEq[A] =
    S.times(x.a, S.reciprocal(x.a)) <=> S.one

  def consistentDiv(x: A, y: NonZero[A]): IsEq[A] =
    S.div(x, y.a) <=> S.times(x, S.reciprocal(y.a))
}

object DivisionRingLaws {
  def apply[A](implicit ev: DivisionRing[A]): DivisionRingLaws[A] =
    new DivisionRingLaws[A] { def S: DivisionRing[A] = ev }
}
