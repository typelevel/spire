package spire.laws

import spire.algebra.MultiplicativeCSemigroup

trait MultiplicativeCSemigroupLaws[A] extends MultiplicativeSemigroupLaws[A] {
  override implicit def S: MultiplicativeCSemigroup[A]

  def timesCommutative(x: A, y: A): IsEq[A] =
    S.times(x, y) <=> S.times(y, x)

}

object MultiplicativeCSemigroupLaws {
  def apply[A](implicit ev: MultiplicativeCSemigroup[A]): MultiplicativeCSemigroupLaws[A] =
    new MultiplicativeCSemigroupLaws[A] { def S: MultiplicativeCSemigroup[A] = ev }
}
