package spire
package algebra

/**
 * A semigroup is any set `A` with an associative operation (`combine`).
 */
trait Semigroup[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any {
  def combine(x: A, y: A): A

  @deprecated("use combine", "0.12.0")
  final def op(x: A, y: A): A = combine(x, y)

  /**
   * Return `a` combined with itself `n` times.
   */
  def combinen(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated combination for semigroups must have repetitions > 0")
    else if (n == 1) a
    else combinenAboveOne(a, n)

  protected def combinenAboveOne(a: A, n: Int): A = {
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
   *  Given a sequence of `as`, combine them using the semigroup and return the total.
   *
   *  If the sequence is empty, returns None. Otherwise, returns Some(total).
   */
  def combineOption(as: TraversableOnce[A]): Option[A] = as.reduceOption(combine)
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]): Semigroup[A] = s

  /**
   * If there exists an implicit `AdditiveSemigroup[A]`, this returns a
   * `Semigroup[A]` using `plus` for `combine`.
   */
  @inline final def additive[A](implicit A: AdditiveSemigroup[A]): Semigroup[A] = A.additive

  /**
   * If there exists an implicit `MultiplicativeSemigroup[A]`, this returns a
   * `Semigroup[A]` using `times` for `combine`.
   */
  @inline final def multiplicative[A](implicit A: MultiplicativeSemigroup[A]): Semigroup[A] = A.multiplicative
}

/**
 * CSemigroup represents a commutative semigroup.
 *
 * A semigroup is commutative if for all x and y, x |+| y === y |+| x.
 */
trait CSemigroup[@sp(Boolean, Byte, Short, Int, Long, Float, Double) A] extends Any with Semigroup[A]

object CSemigroup {
  @inline final def apply[A](implicit ev: CSemigroup[A]): CSemigroup[A] = ev
  @inline final def additive[A](implicit A: AdditiveCSemigroup[A]): CSemigroup[A] =  A.additive
  @inline final def multiplicative[A](implicit A: MultiplicativeCSemigroup[A]): CSemigroup[A] = A.multiplicative
}
