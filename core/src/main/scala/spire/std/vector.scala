package spire.std

import spire.algebra._

trait VectorInstancesLow {
  implicit def VectorVectorSpace[A: Rng]: VectorSpace[Vector[A], A] =
    new SeqVectorSpace[A, Vector[A]]

  implicit def VectorEq[A: Eq]: Eq[Vector[A]] =
    new SeqEq[A, Vector[A]]

  implicit def VectorBasis[A: AdditiveMonoid]: Frame[Vector[A], A] =
    new SeqBasis[A, Vector[A]]
}

trait VectorInstances extends VectorInstancesLow {
  implicit def VectorOrder[A: Order]: Order[Vector[A]] =
    new SeqOrder[A, Vector[A]]
}
