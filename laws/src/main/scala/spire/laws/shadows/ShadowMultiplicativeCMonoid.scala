package spire.laws.shadows

import spire.algebra.{Eq, MultiplicativeCMonoid}

trait ShadowMultiplicativeCMonoid[A, S]
    extends MultiplicativeCMonoid[Shadow[A, S]]
    with ShadowMultiplicativeCSemigroup[A, S] {
  import shadowing._
  implicit def A: MultiplicativeCMonoid[A]
  implicit def S: MultiplicativeCMonoid[S]
  implicit def eqA: Eq[A]
  implicit def eqS: Eq[S]

  def one: Shadow[A, S] = Shadow(A.one, checked(S.one))

  override def isOne(x: Shadow[A, S])(implicit ev: Eq[Shadow[A, S]]) = {
    val a = A.isOne(x.a)
    val s = S.isOne(x.s)
    assert(a == s)
    a
  }

  override def product(xs: IterableOnce[Shadow[A, S]]): Shadow[A, S] = {
    val seq = xs.iterator.toSeq
    val a = A.product(seq.map(_.a))
    val s = S.product(seq.map(_.s))
    Shadow(a, checked(s))
  }
}
