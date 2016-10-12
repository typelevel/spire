package spire
package algebra

/**
 * Rig is a ring whose additive structure doesn't have an inverse (ie. it is
 * monoid, not a group). Put another way, a Rig is a Ring without a negative.
 */
trait Rig[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Semiring[A] with AdditiveCMonoid[A] with MultiplicativeMonoid[A] {
  /**
   * This is similar to `Semigroup#pow`, except that `a pow 0` is defined to be
   * the multiplicative identity.
   */
  override def pow(a:A, n:Int):A =
    if (n >= 0) prodn(a, n)
    else throw new IllegalArgumentException(s"Illegal negative exponent $n to Monoid#pow")
}

object Rig {
  @inline final def apply[A](implicit r:Rig[A]): Rig[A] = r
}

/**
 * CRig is a Rig that is commutative under multiplication.
 */
trait CRig[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Rig[A] with MultiplicativeCMonoid[A]

object CRig {
  @inline final def apply[A](implicit r: CRig[A]): CRig[A] = r
}
