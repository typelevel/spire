package spire

package object algebra {

  /**
   * A `Basis` of a [[VectorSpace]] `V` provides a coordinate system on `V` with
   * elements of type `K`. A `Basis` has an existential index type which can be
   * used to access elements of a vector. 
   *
   * If you care about the index type `I`, then you can use [[IndexedBasis]],
   * which takes the index as a 3rd type parameter.
   *
   * Also see [[Frame]], which is a basis indexed by `Int`.
   */
  type Basis[V, K] = IndexedBasis[V, K, I] forSome { type I }

  object Basis {
    final def apply[V, K](implicit basis: Basis[V, K]): Basis[V, K] = basis

    def Free[I, K: AdditiveMonoid](basis: Set[I]): Basis[Map[I, K], K] =
      IndexedBasis.Free(basis)
  }
}
