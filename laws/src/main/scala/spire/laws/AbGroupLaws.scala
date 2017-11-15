package spire.laws

import spire.algebra.AbGroup

trait AbGroupLaws[A] extends GroupLaws[A] with CMonoidLaws[A] {
  override implicit def S: AbGroup[A]
}

object AbGroupLaws {
  def apply[A](implicit ev: AbGroup[A]): AbGroupLaws[A] =
    new AbGroupLaws[A] { def S: AbGroup[A] = ev }
}
