package spire.laws

import spire.algebra.{Eq, GCDRing}

trait GCDRingLaws[A] extends CRingLaws[A] {
  override implicit def S: GCDRing[A]

  def gcdLcmProduct(x: A, y: A, eqA: Eq[A]): IsEq[A] =
    S.times(S.gcd(x, y)(eqA), S.lcm(x, y)(eqA)) <=> S.times(x, y)

  def gcdCommutative(x: A, y: A, eqA: Eq[A]): IsEq[A] =
    S.gcd(x, y)(eqA) <=> S.gcd(y, x)(eqA)

  def lcmCommutative(x: A, y: A, eqA: Eq[A]): IsEq[A] =
    S.lcm(x, y)(eqA) <=> S.lcm(y, x)(eqA)

  def gcdZeroZero(eqA: Eq[A]): IsEq[A] =
    S.gcd(S.zero, S.zero)(eqA) <=> S.zero

  def lcmZeroZero(eqA: Eq[A]): IsEq[A] =
    S.lcm(S.zero, S.zero)(eqA) <=> S.zero

  def lcmZero(a: A, eqA: Eq[A]): IsEq[A] =
    S.lcm(a, S.zero)(eqA) <=> S.zero
}

object GCDRingLaws {
  def apply[A](implicit ev: GCDRing[A]): GCDRingLaws[A] =
    new GCDRingLaws[A] { def S: GCDRing[A] = ev }
}
