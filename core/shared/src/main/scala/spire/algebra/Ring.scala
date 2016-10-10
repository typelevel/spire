package spire
package algebra

/**
 * Ring represents a set (A) that is a group over addition (+) and a monoid
 * over multiplication (*). Aside from this, the multiplication must distribute
 * over addition.
 *
 * Ring implements some methods (for example fromInt) in terms of other more
 * fundamental methods (zero, one and plus). Where possible, these methods
 * should be overridden by more efficient implementations.
 */
trait Ring[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Rig[A] with Rng[A] {
  /**
   * Defined to be equivalent to `additive.sumn(one, n)`. That is, `n`
   * repeated summations of this ring's `one`, or `-one` if `n` is
   * negative.
   */
  def fromInt(n: Int): A = sumN(one, n)
}

object Ring {
  @inline final def apply[A](implicit r: Ring[A]): Ring[A] = r
}

/**
 * CRing is a Ring that is commutative under multiplication.
 */
trait CRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Ring[A] with MultiplicativeCMonoid[A]

object CRing {
  @inline final def apply[A](implicit r: CRing[A]): CRing[A] = r
}
