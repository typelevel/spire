package spire.laws.shadows

import spire.algebra._

trait ShadowMultiplicativeAbGroup[A, S] extends MultiplicativeAbGroup[Shadow[A, S]]
  with ShadowMultiplicativeGroup[A, S] with ShadowMultiplicativeCMonoid[A, S] {
  implicit def A: MultiplicativeAbGroup[A]
  implicit def S: MultiplicativeAbGroup[S]
}
