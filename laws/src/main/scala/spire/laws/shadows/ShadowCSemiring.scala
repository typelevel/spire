package spire.laws.shadows

import spire.algebra.CSemiring

trait ShadowCSemiring[A, S] extends CSemiring[Shadow[A, S]]
  with ShadowAdditiveCMonoid[A, S] with ShadowMultiplicativeCSemigroup[A, S] {
  import shadowing._
  implicit def A: CSemiring[A]
  implicit def S: CSemiring[S]
}
