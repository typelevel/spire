package spire.laws

import spire.algebra.CSemigroup

trait CSemigroupLaws[A] extends SemigroupLaws[A] {
  override implicit def S: CSemigroup[A]

  def combineCommutative(x: A, y: A): IsEq[A] =
    S.combine(x, y) <=> S.combine(y, x)

}

object CSemigroupLaws {
  def apply[A](implicit ev: CSemigroup[A]): CSemigroupLaws[A] =
    new CSemigroupLaws[A] { def S: CSemigroup[A] = ev }
}
