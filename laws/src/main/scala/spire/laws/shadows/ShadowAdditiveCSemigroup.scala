package spire.laws.shadows

import spire.algebra.AdditiveCSemigroup

trait ShadowAdditiveCSemigroup[A, S] extends AdditiveCSemigroup[Shadow[A, S]] with ShadowAdditiveSemigroup[A, S] {
  implicit def A: AdditiveCSemigroup[A]
  implicit def S: AdditiveCSemigroup[S]
}
