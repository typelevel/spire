package spire.laws

import spire.algebra.Ring

trait RingLaws[A] extends RigLaws[A] with RngLaws[A] {
  override implicit def S: Ring[A]
}

object RingLaws {
  def apply[A](implicit ev: Ring[A]): RingLaws[A] =
    new RingLaws[A] { def S: Ring[A] = ev }
}
