package spire.laws.shadows

import spire.algebra.AdditiveGroup

trait ShadowAdditiveGroup[A, S] extends AdditiveGroup[Shadow[A, S]] with ShadowAdditiveMonoid[A, S] {
  import shadowing._
  implicit def A: AdditiveGroup[A]
  implicit def S: AdditiveGroup[S]

  def negate(x: Shadow[A, S]): Shadow[A, S] = Shadow(A.negate(x.a), checked(S.negate(x.s)))

  override def minus(x: Shadow[A, S], y: Shadow[A, S]) =
    Shadow(A.minus(x.a, y.a), checked(S.minus(x.s, y.s)))

}
