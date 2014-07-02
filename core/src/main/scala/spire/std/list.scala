package spire.std

import spire.algebra._

trait ListInstancesLow {
  implicit def ListVectorSpace[A: Rng]: VectorSpace[List[A], A] =
    new SeqVectorSpace[A, List[A]]

  implicit def ListEq[A: Eq]: Eq[List[A]] =
    new SeqEq[A, List[A]]

  implicit def ListBasis[A: AdditiveMonoid]: Frame[List[A], A] =
    new SeqBasis[A, List[A]]
}

trait ListInstances extends ListInstancesLow {
  implicit def ListOrder[A: Order]: Order[List[A]] =
    new SeqOrder[A, List[A]]
}

