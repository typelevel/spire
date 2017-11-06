package spire.laws

import spire.algebra.AdditiveSemigroup

trait AdditiveSemigroupLaws[A] {
  implicit def S: AdditiveSemigroup[A]

  def semigroupAssociative(x: A, y: A, z: A): IsEq[A] =
    S.plus(S.plus(x, y), z) <=> S.plus(x, S.plus(y, z))

  def sum1(a: A): IsEq[A] =
    S.sumN(a, 1) <=> a

  def sum2(a: A): IsEq[A] =
    S.sumN(a, 2) <=> S.plus(a, a)

  def trySum(xs: Vector[A]): IsEq[Option[A]] =
    S.trySum(xs) <=> xs.reduceOption(S.plus)

}

object AdditiveSemigroupLaws {
  def apply[A](implicit ev: AdditiveSemigroup[A]): AdditiveSemigroupLaws[A] =
    new AdditiveSemigroupLaws[A] { def S: AdditiveSemigroup[A] = ev }
}
