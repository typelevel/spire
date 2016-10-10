package spire
package algebra

/**
 * A semigroup is any set `A` with an associative operation (`combine`).
 */
trait Semigroup[@sp(Int, Long, Float, Double) A] extends Any {
  def combine(x: A, y: A): A

  /**
   * Return `a` combined with itself `n` times.
   */
  def combineN(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated combination for semigroups must have repetitions > 0")
    else repeatedCombineN(a, n)

  protected[this] def repeatedCombineN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) {
        combine(b, extra)
      } else {
        val x = if ((k & 1) == 1) combine(b, extra) else extra
        loop(combine(b, b), k >>> 1, x)
      }
    loop(a, n - 1, a)
  }

  /**
   *  Given a sequence of `as`, combine them and return the total.
   *
   *  If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  def combineAllOption(as: TraversableOnce[A]): Option[A] = as.reduceOption(combine)
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]): Semigroup[A] = s
}

/**
 * CSemigroup represents a commutative semigroup.
 *
 * A semigroup is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CSemigroup[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Semigroup[A]

object CSemigroup {
  @inline final def apply[A](implicit ev: CSemigroup[A]): CSemigroup[A] = ev
}
