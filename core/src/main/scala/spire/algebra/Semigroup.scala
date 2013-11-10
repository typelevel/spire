package spire.algebra

import scala.{ specialized => spec }
import scala.annotation.{ switch, tailrec }

/**
 * A semigroup is any set `A` with an associative operation (`op`).
 */
trait Semigroup[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A] {
  def op(x: A, y: A): A
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]) = s

  /**
   * If there exists an implicit `AdditiveSemigroup[A]`, this returns a
   * `Semigroup[A]` using `plus` for `op`.
   */
  @inline final def additive[A](implicit A: AdditiveSemigroup[A]) =  A.additive

  /**
   * If there exists an implicit `MultiplicativeSemigroup[A]`, this returns a
   * `Semigroup[A]` using `times` for `op`.
   */
  @inline final def multiplicative[A](implicit A: MultiplicativeSemigroup[A]) = A.multiplicative

  /**
   * Return `a` appended to itself `n` times.
   */
  final def sumn[@spec(Boolean, Byte, Short, Int, Long, Float, Double) A](a: A, n: Int)(implicit A: Semigroup[A]): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A = {
      (k: @annotation.switch) match {
        case 0 => b
        case 1 => A.op(b, extra)
        case n =>
          loop(A.op(b, b), k >>> 1, if (k % 2 == 1) A.op(b, extra) else extra)
      }
    }

    if (n > 0) loop(a, n - 1, a)
    else throw new IllegalArgumentException("Repeated summation for semigroups must have reptitions > 0")
  }
}
