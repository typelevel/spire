package spire.laws.shadows

import spire.algebra.MultiplicativeMonoid

trait ShadowMultiplicativeMonoid[A, S] extends MultiplicativeMonoid[Shadow[A, S]] with ShadowMultiplicativeSemigroup[A, S] {
  import shadowing._
  implicit def A: MultiplicativeMonoid[A]
  implicit def S: MultiplicativeMonoid[S]

  def one: Shadow[A, S] = Shadow(A.one, checked(S.one))

  override def product(xs: TraversableOnce[Shadow[A, S]]): Shadow[A, S] = {
    val seq = xs.toSeq
    val a = A.product(seq.map(_.a))
    val s = S.product(seq.map(_.s))
    Shadow(a, checked(s))
  }
}
