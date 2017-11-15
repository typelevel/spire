package spire.laws

import spire.algebra.{Eq, EuclideanRing}

trait EuclideanRingLaws[A] extends GCDRingLaws[A] {
  override implicit def S: EuclideanRing[A]

  def euclideanDivisionRule(x: A, y: NonZero[A]): IsEq[A] = {
    val (q, r) = S.quotmod(x, y.a)
    x <=> S.plus(S.times(y.a, q), r)
  }

  def equot(x: A, y: NonZero[A]): IsEq[A] =
    S.quotmod(x, y.a)._1 <=> S.quot(x, y.a)

  def emod(x: A, y: NonZero[A]): IsEq[A] =
    S.quotmod(x, y.a)._2 <=> S.mod(x, y.a)

  def euclideanFunction(x: A, y: NonZero[A], eqA: Eq[A]): IsEq[Boolean] = {
    val (q, r) = S.quotmod(x, y.a)
    (S.isZero(r)(eqA) || (S.euclideanFunction(r) < S.euclideanFunction(y.a))) <=> true
  }

  def submultiplicativeEuclideanFunction(x: NonZero[A], y: NonZero[A]): IsEq[Boolean] =
    (S.euclideanFunction(x.a) <= S.euclideanFunction(S.times(x.a, y.a))) <=> true

}

object EuclideanRingLaws {
  def apply[A](implicit ev: EuclideanRing[A]): EuclideanRingLaws[A] =
    new EuclideanRingLaws[A] { def S: EuclideanRing[A] = ev }
}
