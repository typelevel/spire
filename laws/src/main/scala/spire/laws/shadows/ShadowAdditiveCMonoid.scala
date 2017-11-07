package spire.laws.shadows

import spire.algebra.AdditiveCMonoid

trait ShadowAdditiveCMonoid[A, S] extends AdditiveCMonoid[Shadow[A, S]]
  with ShadowAdditiveMonoid[A, S] with ShadowAdditiveCSemigroup[A, S] {
  implicit def A: AdditiveCMonoid[A]
  implicit def S: AdditiveCMonoid[S]
}
