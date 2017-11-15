package spire.laws

import spire.algebra.AdditiveGroup

trait AdditiveGroupLaws[A] extends AdditiveMonoidLaws[A] {
  override implicit def S: AdditiveGroup[A]

  def leftNegate(x: A): IsEq[A] =
    S.plus(S.negate(x), x) <=> S.zero

  def rightNegate(x: A): IsEq[A] =
    S.plus(x, S.negate(x)) <=> S.zero

  def consistentMinus(x: A, y: A): IsEq[A] =
    S.minus(x, y) <=> S.plus(x, S.negate(y))

}

object AdditiveGroupLaws {
  def apply[A](implicit ev: AdditiveGroup[A]): AdditiveGroupLaws[A] =
    new AdditiveGroupLaws[A] { def S: AdditiveGroup[A] = ev }
}
