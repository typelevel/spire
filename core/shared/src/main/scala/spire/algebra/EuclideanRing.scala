package spire
package algebra

trait EuclideanRing[@sp(Byte, Short, Int, Long, Float, Double) A] extends Any with GCDRing[A] {
  def euclideanFunction(a: A): BigInt
  def equot(a: A, b: A): A
  def emod(a: A, b: A): A
  def equotmod(a: A, b: A): (A, A) = (equot(a, b), emod(a, b))
}

object EuclideanRing {
  @inline final def apply[A](implicit e: EuclideanRing[A]): EuclideanRing[A] = e

  @tailrec final def euclid[@sp(Byte, Short, Int, Long, Float, Double) A:Eq:EuclideanRing](a: A, b: A): A = {
    import spire.syntax.euclideanRing._
    if (b.isZero) a else euclid(b, a emod b)
  }
}
