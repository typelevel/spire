package spire.laws

import spire.algebra.Rng

trait RngLaws[A] extends SemiringLaws[A] with AdditiveAbGroupLaws[A] {
  override implicit def S: Rng[A]
}

object RngLaws {
  def apply[A](implicit ev: Rng[A]): RngLaws[A] =
    new RngLaws[A] { def S: Rng[A] = ev }
}
