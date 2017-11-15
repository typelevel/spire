package spire.laws

import spire.algebra.AdditiveCMonoid

trait AdditiveCMonoidLaws[A] extends AdditiveMonoidLaws[A] with AdditiveCSemigroupLaws[A] {
  override implicit def S: AdditiveCMonoid[A]
}

object AdditiveCMonoidLaws {
  def apply[A](implicit ev: AdditiveCMonoid[A]): AdditiveCMonoidLaws[A] =
  new AdditiveCMonoidLaws[A] { def S: AdditiveCMonoid[A] = ev }
}
