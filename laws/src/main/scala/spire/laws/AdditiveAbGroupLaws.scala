package spire.laws

import spire.algebra.AdditiveAbGroup

trait AdditiveAbGroupLaws[A] extends AdditiveGroupLaws[A] with AdditiveCMonoidLaws[A] {
  override implicit def S: AdditiveAbGroup[A]
}

object AdditiveAbGroupLaws {
  def apply[A](implicit ev: AdditiveAbGroup[A]): AdditiveAbGroupLaws[A] =
    new AdditiveAbGroupLaws[A] { def S: AdditiveAbGroup[A] = ev }
}
