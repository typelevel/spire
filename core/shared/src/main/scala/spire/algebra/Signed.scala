package spire
package algebra

/**
 * A trait for linearly ordered additive abelian groups. The following laws holds:
 *
 * (1) if `a <= b` then `a + c <= b + c` (linear order),
 * (2) `abs(x) = -x` if `x < 0`, or `x` otherwise,
 * (3) `signum(x) = -1` if `x < 0`, `signum(x) = 1` if `x > 0`, `signum(x) = 0` otherwise,
 *
 * from which we deduce
 * 
 * (3) `abs(a + b) <= abs(a) + abs(b)` (triangle inequality).
 * 
 */
trait Signed[@sp(Double, Float, Int, Long) A] extends Any with Order[A] {
  implicit def additiveAbGroup: AdditiveAbGroup[A]

  /** Returns Zero if `a` is 0, Positive if `a` is positive, and Negative is `a` is negative. */
  def sign(a: A): Sign = Sign(signum(a))

  /** Returns 0 if `a` is 0, > 0 if `a` is positive, and < 0 is `a` is negative. */
  def signum(a: A): Int = {
    val c = compare(a, additiveAbGroup.zero)
    if (c < 0) -1
    else if (c > 0) 1
    else 0
  }

  /** An idempotent function that ensures an object has a non-negative sign. */
  def abs(a: A): A = if (signum(a) < 0) additiveAbGroup.negate(a) else a

  def isSignZero(a: A): Boolean = signum(a) == 0
  def isSignPositive(a: A): Boolean = signum(a) > 0
  def isSignNegative(a: A): Boolean = signum(a) < 0

  def isSignNonZero(a: A): Boolean = signum(a) != 0
  def isSignNonPositive(a: A): Boolean = signum(a) <= 0
  def isSignNonNegative(a: A): Boolean = signum(a) >= 0
}

object Signed {
  def apply[A](implicit s: Signed[A]): Signed[A] = s
}
