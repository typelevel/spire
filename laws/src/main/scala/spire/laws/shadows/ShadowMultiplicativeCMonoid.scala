package spire.laws.shadows

import spire.algebra.MultiplicativeCMonoid

trait ShadowMultiplicativeCMonoid[A, S] extends MultiplicativeCMonoid[Shadow[A, S]]
  with ShadowMultiplicativeMonoid[A, S] with ShadowMultiplicativeCSemigroup[A, S] {
  implicit def A: MultiplicativeCMonoid[A]
  implicit def S: MultiplicativeCMonoid[S]
}
