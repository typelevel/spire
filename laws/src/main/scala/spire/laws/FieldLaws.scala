package spire.laws

import spire.algebra.Field

trait FieldLaws[A] extends DivisionRingLaws[A] with EuclideanRingLaws[A] {
  override implicit def S: Field[A]
}

object FieldLaws {
  def apply[A](implicit ev: Field[A]): FieldLaws[A] =
    new FieldLaws[A] { def S: Field[A] = ev }
}
