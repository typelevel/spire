package spire.laws.shadows

import spire.algebra.AdditiveMonoid

trait ShadowAdditiveMonoid[A, S] extends AdditiveMonoid[Shadow[A, S]] with ShadowAdditiveSemigroup[A, S] {
  import shadowing._
  implicit def A: AdditiveMonoid[A]
  implicit def S: AdditiveMonoid[S]

  def zero: Shadow[A, S] = Shadow(A.zero, checked(S.zero))

  override def sum(xs: TraversableOnce[Shadow[A, S]]): Shadow[A, S] = {
    val seq = xs.toSeq
    val a = A.sum(seq.map(_.a))
    val s = S.sum(seq.map(_.s))
    Shadow(a, checked(s))
  }
}
