package spire.laws.shadows

import spire.algebra.MultiplicativeCSemigroup

trait ShadowMultiplicativeCSemigroup[A, S] extends MultiplicativeCSemigroup[Shadow[A, S]] with ShadowMultiplicativeSemigroup[A, S] {
  implicit def A: MultiplicativeCSemigroup[A]
  implicit def S: MultiplicativeCSemigroup[S]
}
