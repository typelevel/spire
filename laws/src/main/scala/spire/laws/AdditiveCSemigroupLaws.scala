package spire.laws

import spire.algebra.AdditiveCSemigroup

trait AdditiveCSemigroupLaws[A] extends AdditiveSemigroupLaws[A] {
  override implicit def S: AdditiveCSemigroup[A]

  def plusCommutative(x: A, y: A): IsEq[A] =
    S.plus(x, y) <=> S.plus(y, x)

}

object AdditiveCSemigroupLaws {
  def apply[A](implicit ev: AdditiveCSemigroup[A]): AdditiveCSemigroupLaws[A] =
    new AdditiveCSemigroupLaws[A] { def S: AdditiveCSemigroup[A] = ev }
}
