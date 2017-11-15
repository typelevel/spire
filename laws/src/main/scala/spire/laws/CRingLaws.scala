package spire.laws

import spire.algebra.CRing

trait CRingLaws[A] extends RingLaws[A] with CRigLaws[A] with CRngLaws[A] {
  override implicit def S: CRing[A]
}

object CRingLaws {
  def apply[A](implicit ev: CRing[A]): CRingLaws[A] =
    new CRingLaws[A] { def S: CRing[A] = ev }
}
