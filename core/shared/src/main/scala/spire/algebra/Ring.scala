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
   * Convert the given integer to an instance of A.
   *
   * Defined to be equivalent to `sumN(one, n)`.
   *
   * That is, `n` repeated summations of this ring's `one`, or `-n`
   * summations of `-one` if `n` is negative.
   *
   * Most type class instances should consider overriding this method
   * for performance reasons.
   */
  def fromInt(n: Int): A = sumN(one, n)

  /**
   * Convert the given BigInt to an instance of A.
   *
   * This is equivalent to `n` repeated summations of this ring's `one`, or
   * `-n` summations of `-one` if `n` is negative.
   *
   * Most type class instances should consider overriding this method for
   * performance reasons.
   */
  def fromBigInt(n: BigInt): A = Ring.defaultFromBigInt(n)(this)
}

object Ring {
  @inline final def apply[A](implicit r: Ring[A]): Ring[A] = r

  final def defaultFromBigInt[@sp(Int, Long, Float, Double) A](n: BigInt)(implicit ev: Ring[A]): A = {
    if (n.isValidInt) {
      ev.fromInt(n.toInt)
    } else {
      val d = ev.fromInt(1 << 30)
      val mask = (1L << 30) - 1
      @tailrec def loop(k: A, x: BigInt, acc: A): A =
        if (x.isValidInt) {
          ev.plus(ev.times(k, ev.fromInt(x.toInt)), acc)
        } else {
          val y = x >> 30
          val r = ev.fromInt((x & mask).toInt)
          loop(ev.times(d, k), y, ev.plus(ev.times(k, r), acc))
        }

      val absValue = loop(ev.one, n.abs, ev.zero)
      if (n.signum < 0) ev.negate(absValue) else absValue
    }
  }
}

/**
 * CRing is a Ring that is commutative under multiplication.
 */
trait CRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with Ring[A] with CRig[A] with MultiplicativeCMonoid[A]

object CRing {
  @inline final def apply[A](implicit r: CRing[A]): CRing[A] = r
}
