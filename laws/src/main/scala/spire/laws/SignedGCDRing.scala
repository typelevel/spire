package spire.laws

/** This is more a convention: as GCD is defined up to a unit, so up to a sign,
  * on an ordered GCD ring we require gcd(x, y) >= 0, which is the common
  * behavior of computer algebra systems.
  */
trait SignedGCDRingLaws[A] extends GCDRingLaws[A] with SignedAdditiveAbGroupLaws[A] {

  def gcdSign(x: A, y: A): IsEq[Boolean] =
    (E.signum(S.gcd(x, y)) >= 0) <=> true

  def gcdByZero(x: A): IsEq[A] =
    S.gcd(x, S.zero) <=> E.abs(x)
}
