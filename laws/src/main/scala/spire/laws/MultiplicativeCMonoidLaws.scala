package spire.laws

import spire.algebra.MultiplicativeCMonoid

trait MultiplicativeCMonoidLaws[A] extends MultiplicativeMonoidLaws[A] with MultiplicativeCSemigroupLaws[A] {
  override implicit def S: MultiplicativeCMonoid[A]
}

object MultiplicativeCMonoidLaws {
  def apply[A](implicit ev: MultiplicativeCMonoid[A]): MultiplicativeCMonoidLaws[A] =
  new MultiplicativeCMonoidLaws[A] { def S: MultiplicativeCMonoid[A] = ev }
}
