package spire.laws

import spire.algebra.MultiplicativeSemigroup

trait MultiplicativeSemigroupLaws[A] {
  implicit def S: MultiplicativeSemigroup[A]

  def timesAssociative(x: A, y: A, z: A): IsEq[A] =
    S.times(S.times(x, y), z) <=> S.times(x, S.times(y, z))

  def pow1(a: A): IsEq[A] =
    S.pow(a, 1) <=> a

  def pow2(a: A): IsEq[A] =
    S.pow(a, 2) <=> S.times(a, a)

  def pow3(a: A): IsEq[A] =
    S.pow(a, 3) <=> S.times(S.times(a, a), a)

  def tryProduct(xs: Vector[A]): IsEq[Option[A]] =
    S.tryProduct(xs) <=> xs.reduceOption(S.times)
}

object MultiplicativeSemigroupLaws {
  def apply[A](implicit ev: MultiplicativeSemigroup[A]): MultiplicativeSemigroupLaws[A] =
    new MultiplicativeSemigroupLaws[A] { def S: MultiplicativeSemigroup[A] = ev }
}
