package spire.laws.shadows

import spire.algebra.MultiplicativeGroup

trait ShadowMultiplicativeGroup[A, S] extends MultiplicativeGroup[Shadow[A, S]] with ShadowMultiplicativeMonoid[A, S] {
  import shadowing._
  implicit def A: MultiplicativeGroup[A]
  implicit def S: MultiplicativeGroup[S]

  def reciprocal(x: Shadow[A, S]): Shadow[A, S] = Shadow(A.reciprocal(x.a), checked(S.reciprocal(x.s)))

  def div(x: Shadow[A, S], y: Shadow[A, S]) =
    Shadow(A.div(x.a, y.a), checked(S.div(x.s, y.s)))

}
