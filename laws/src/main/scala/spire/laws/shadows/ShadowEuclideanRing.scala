package spire.laws.shadows

import spire.algebra.EuclideanRing

trait ShadowEuclideanRing[A, S] extends EuclideanRing[Shadow[A, S]] with ShadowGCDRing[A, S] {
  import shadowing._
  implicit def A: EuclideanRing[A]
  implicit def S: EuclideanRing[S]

  def euclideanFunction(x: Shadow[A, S]): BigInt = {
    val a = A.euclideanFunction(x.a)
    val s = S.euclideanFunction(x.s)
    assert(a == s)
    a
  }

  def quot(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.quot(x.a, y.a), checked(S.quot(x.s, y.s)))

  override def quotmod(x: Shadow[A, S], y: Shadow[A, S]): (Shadow[A, S], Shadow[A, S]) = {
    val (a1, a2) = A.quotmod(x.a, y.a)
    val (s1, s2) = S.quotmod(x.s, y.s)
    (Shadow(a1, checked(s1)), Shadow(a2, checked(s2)))
  }

  def mod(x: Shadow[A, S], y: Shadow[A, S]): Shadow[A, S] =
    Shadow(A.mod(x.a, y.a), S.mod(x.s, y.s))
}
