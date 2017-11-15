package spire.laws

import spire.algebra.CSemiring

trait CSemiringLaws[A] extends SemiringLaws[A] with MultiplicativeCSemigroupLaws[A] {
  override implicit def S: CSemiring[A]
}

object CSemiringLaws {
  def apply[A](implicit ev: CSemiring[A]): CSemiringLaws[A] =
    new CSemiringLaws[A] { def S: CSemiring[A] = ev }
}
