package spire.laws

import spire.algebra.Group

trait GroupLaws[A] extends MonoidLaws[A] {
  override implicit def S: Group[A]

  def leftInverse(x: A): IsEq[A] =
    S.combine(S.inverse(x), x) <=> S.empty

  def rightInverse(x: A): IsEq[A] =
    S.combine(x, S.inverse(x)) <=> S.empty

  def consistentRemove(x: A, y: A): IsEq[A] =
    S.remove(x, y) <=> S.combine(x, S.inverse(y))

}

object GroupLaws {
  def apply[A](implicit ev: Group[A]): GroupLaws[A] =
    new GroupLaws[A] { def S: Group[A] = ev }
}
