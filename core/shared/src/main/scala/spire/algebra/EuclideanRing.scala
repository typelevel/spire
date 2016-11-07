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
 *   equot(x, y) = q
 *   emod(x, y) = r
 * 
  */
trait EuclideanRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with GCDRing[A] {
  def euclideanFunction(a: A): BigInt
  def equot(a: A, b: A): A
  def emod(a: A, b: A): A
  def equotmod(a: A, b: A): (A, A) = (equot(a, b), emod(a, b))
  def gcd(a: A, b: A)(implicit ev: Eq[A]): A = EuclideanRing.euclid(a, b)(ev, EuclideanRing.this)
  def lcm(a: A, b: A)(implicit ev: Eq[A]): A =
    if (isZero(a) || isZero(b)) zero else times(equot(a, gcd(a, b)), b)
}

object EuclideanRing {
  @inline final def apply[A](implicit e: EuclideanRing[A]): EuclideanRing[A] = e

  @tailrec final def euclid[@sp(Byte, Short, Int, Long, Float, Double) A:Eq:EuclideanRing](a: A, b: A): A = {
    import spire.syntax.euclideanRing._
    if (b.isZero) a else euclid(b, a emod b)
  }
}
