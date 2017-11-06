package spire.laws

import spire.algebra.MultiplicativeGroup

trait MultiplicativeGroupLaws[A] extends MultiplicativeMonoidLaws[A] {
  override implicit def S: MultiplicativeGroup[A]

  def leftNegate(x: A): IsEq[A] =
    S.times(S.reciprocal(x), x) <=> S.one

  def rightNegate(x: A): IsEq[A] =
    S.times(x, S.reciprocal(x)) <=> S.one

  def consistentMinus(x: A, y: A): IsEq[A] =
    S.div(x, y) <=> S.times(x, S.reciprocal(y))

}

object MultiplicativeGroupLaws {
  def apply[A](implicit ev: MultiplicativeGroup[A]): MultiplicativeGroupLaws[A] =
    new MultiplicativeGroupLaws[A] { def S: MultiplicativeGroup[A] = ev }
}
