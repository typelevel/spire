package spire.laws.shadows

import cats.kernel.Comparison
import spire.algebra.PartialOrder

trait ShadowPartialOrder[A, S] extends ShadowEq[A, S] with PartialOrder[Shadow[A, S]] {
  implicit def A: PartialOrder[A]
  implicit def S: PartialOrder[S]

  override def eqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = super[ShadowEq].eqv(x, y)
  override def neqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = super[ShadowEq].neqv(x, y)

  def partialCompare(x: Shadow[A, S], y: Shadow[A, S]): Double = {
    val a = A.partialCompare(x.a, y.a)
    val s = S.partialCompare(x.s, y.s)
    if (a.isNaN) assert(s.isNaN) else assert(a == s)
    a
  }

  override def partialComparison(x: Shadow[A, S], y: Shadow[A, S]): Option[Comparison] = {
    val a = A.partialComparison(x.a, y.a)
    val s = S.partialComparison(x.s, y.s)
    assert(a == s)
    a
  }

  override def tryCompare(x: Shadow[A, S], y: Shadow[A, S]): Option[Int] = {
    val a = A.tryCompare(x.a, y.a)
    val s = S.tryCompare(x.s, y.s)
    assert(a == s)
    a
  }

  override def pmin(x: Shadow[A, S], y: Shadow[A, S]): Option[Shadow[A, S]] = {
    val a = A.pmin(x.a, y.a)
    val s = S.pmin(x.s, y.s)
    (a, s) match {
      case (Some(a1), Some(s1)) => Some(Shadow(a1, s1))
      case (None, None)         => None
      case _                    => throw new IllegalArgumentException("Inconsistent shadowing")
    }
  }

  override def pmax(x: Shadow[A, S], y: Shadow[A, S]): Option[Shadow[A, S]] = {
    val a = A.pmax(x.a, y.a)
    val s = S.pmax(x.s, y.s)
    (a, s) match {
      case (Some(a1), Some(s1)) => Some(Shadow(a1, s1))
      case (None, None)         => None
      case _                    => throw new IllegalArgumentException("Inconsistent shadowing")
    }
  }

  override def lteqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.lteqv(x.a, y.a)
    val s = S.lteqv(x.s, y.s)
    assert(a == s)
    a
  }

  override def lt(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.lt(x.a, y.a)
    val s = S.lt(x.s, y.s)
    assert(a == s)
    a
  }

  override def gteqv(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.gteqv(x.a, y.a)
    val s = S.gteqv(x.s, y.s)
    assert(a == s)
    a
  }

  override def gt(x: Shadow[A, S], y: Shadow[A, S]): Boolean = {
    val a = A.gt(x.a, y.a)
    val s = S.gt(x.s, y.s)
    assert(a == s)
    a
  }
}
