package spire.laws

import spire.algebra.CMonoid

trait CMonoidLaws[A] extends MonoidLaws[A] with CSemigroupLaws[A] {
  override implicit def S: CMonoid[A]
}

object CMonoidLaws {
  def apply[A](implicit ev: CMonoid[A]): CMonoidLaws[A] =
  new CMonoidLaws[A] { def S: CMonoid[A] = ev }
}
