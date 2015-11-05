package spire
package algebra

trait EuclideanRing[@spec(Byte, Short, Int, Long, Float, Double) A] extends Any with CRing[A] {
  def quot(a: A, b: A): A
  def mod(a: A, b: A): A
  def quotmod(a: A, b: A): (A, A) = (quot(a, b), mod(a, b))

  def gcd(a: A, b: A): A
  def lcm(a: A, b: A): A = times(quot(a, gcd(a, b)), b)

  @tailrec protected[this] final def euclid(a: A, b: A)(implicit eq: Eq[A]): A =
    if (eq.eqv(b, zero)) a else euclid(b, mod(a, b))
}

object EuclideanRing {
  @inline final def apply[A](implicit e: EuclideanRing[A]): EuclideanRing[A] = e
}
