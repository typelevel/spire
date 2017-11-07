package spire.laws.shadows

import spire.algebra.{CRing, CSemiring, Eq, GCDRing}

trait ShadowGCDRing[A, S] extends GCDRing[Shadow[A, S]] with ShadowCRing[A, S] {
  import shadowing._
  implicit def A: GCDRing[A]
  implicit def S: GCDRing[S]

  def gcd(x: Shadow[A, S], y: Shadow[A, S])(implicit ev: Eq[Shadow[A, S]]): Shadow[A, S] =
    Shadow(A.gcd(x.a, y.a), checked(S.gcd(x.s, y.s)))

  def lcm(x: Shadow[A, S], y: Shadow[A, S])(implicit ev: Eq[Shadow[A, S]]): Shadow[A, S] =
    Shadow(A.lcm(x.a, y.a), checked(S.lcm(x.s, y.s)))
}
