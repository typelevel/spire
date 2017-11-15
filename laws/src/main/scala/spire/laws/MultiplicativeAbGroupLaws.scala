package spire.laws

import spire.algebra.MultiplicativeAbGroup

trait MultiplicativeAbGroupLaws[A] extends MultiplicativeGroupLaws[A] with MultiplicativeCMonoidLaws[A] {
  override implicit def S: MultiplicativeAbGroup[A]
}

object MultiplicativeAbGroupLaws {
  def apply[A](implicit ev: MultiplicativeAbGroup[A]): MultiplicativeAbGroupLaws[A] =
    new MultiplicativeAbGroupLaws[A] { def S: MultiplicativeAbGroup[A] = ev }
}
