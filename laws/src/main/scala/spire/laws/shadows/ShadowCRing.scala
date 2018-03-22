package spire.laws.shadows

import spire.algebra.CRing

trait ShadowCRing[A, S] extends CRing[Shadow[A, S]]
  with ShadowCRig[A, S] with ShadowCRng[A, S] {
  import shadowing._
  implicit def A: CRing[A]
  implicit def S: CRing[S]

  override def fromInt(n: Int): Shadow[A, S] = Shadow(A.fromInt(n), checked(S.fromInt(n)))
  override def fromBigInt(n: BigInt): Shadow[A, S] = Shadow(A.fromBigInt(n), checked(S.fromBigInt(n)))
}
