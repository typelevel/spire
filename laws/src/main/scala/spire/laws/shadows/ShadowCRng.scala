package spire.laws.shadows

import spire.algebra.CRng

trait ShadowCRng[A, S] extends CRng[Shadow[A, S]] with ShadowCSemiring[A, S] with ShadowAdditiveAbGroup[A, S] {
  implicit def A: CRng[A]
  implicit def S: CRng[S]
}
