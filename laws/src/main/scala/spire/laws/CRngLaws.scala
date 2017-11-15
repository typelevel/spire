package spire.laws

import spire.algebra.CRng

trait CRngLaws[A] extends CSemiringLaws[A] with RngLaws[A] {
  override implicit def S: CRng[A]
}

object CRngLaws {
  def apply[A](implicit ev: CRng[A]): CRngLaws[A] =
    new CRngLaws[A] { def S: CRng[A] = ev }
}
