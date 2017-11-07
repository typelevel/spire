package spire.laws.shadows

import spire.algebra.{CRig, CSemiring}

trait ShadowCRig[A, S] extends CRig[Shadow[A, S]]
  with ShadowCSemiring[A, S] with ShadowMultiplicativeCMonoid[A, S] {
  import shadowing._
  implicit def A: CRig[A]
  implicit def S: CRig[S]
}
