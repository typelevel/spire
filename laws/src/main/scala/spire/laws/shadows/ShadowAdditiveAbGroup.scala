package spire.laws.shadows

import spire.algebra._

trait ShadowAdditiveAbGroup[A, S] extends AdditiveAbGroup[Shadow[A, S]]
  with ShadowAdditiveGroup[A, S] with ShadowAdditiveCMonoid[A, S] {
  implicit def A: AdditiveAbGroup[A]
  implicit def S: AdditiveAbGroup[S]
}
