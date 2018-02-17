package spire
package algebra

/**
 * EuclideanRing implements a Euclidean domain.
 *
 * The formal definition says that every euclidean domain A has (at
 * least one) euclidean function f: A -> N (the natural numbers) where:
 *
 *   (for every x and non-zero y) x = yq + r, and r = 0 or f(r) < f(y).
 *
 * This generalizes the Euclidean division of integers, where f represents
 * a measure of length (or absolute value), and the previous equation
 * represents finding the quotient and remainder of x and y. So:
 *
 *   quot(x, y) = q
 *   mod(x, y) = r
 *
 */
trait EuclideanRing[@sp(Int, Long, Float, Double) A] extends Any with GCDRing[A] {
  def euclideanFunction(a: A): BigInt
  def quot(a: A, b: A): A
  def mod(a: A, b: A): A
  def quotmod(a: A, b: A): (A, A) = (quot(a, b), mod(a, b))
}

trait EuclideanRingFunctions[R[T] <: EuclideanRing[T]] extends GCDRingFunctions[R] {
  def euclideanFunction[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: R[A]): BigInt =
    ev.euclideanFunction(a)
  def quot[@sp(Int, Long, Float, Double) A](a: A, b: A)(implicit ev: R[A]): A =
    ev.quot(a, b)
}

object EuclideanRing extends EuclideanRingFunctions[EuclideanRing] {

  @inline final def apply[A](implicit e: EuclideanRing[A]): EuclideanRing[A] = e

  @tailrec final def euclid[@sp(Int, Long, Float, Double) A:Eq:EuclideanRing](a: A, b: A): A = {
    if (EuclideanRing[A].isZero(b)) a else euclid(b, EuclideanRing[A].mod(a, b))
  }

  trait WithEuclideanAlgorithm[@sp(Int, Long, Float, Double) A] extends Any with EuclideanRing[A] { self =>
    def gcd(a: A, b: A)(implicit ev: Eq[A]): A =
      EuclideanRing.euclid(a, b)(ev, self)
    def lcm(a: A, b: A)(implicit ev: Eq[A]): A =
      if (isZero(a) || isZero(b)) zero else times(quot(a, gcd(a, b)), b)
  }

}
