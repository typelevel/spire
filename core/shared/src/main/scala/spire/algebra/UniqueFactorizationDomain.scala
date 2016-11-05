package spire
package algebra

/** A unique factorization domain is a commutative ring in which each element can be written
  * as a product of prime elements and a unit.
  *
  */
trait UniqueFactorizationDomain[@sp(Byte, Short, Int, Long) A] {
  /** Tests whether the given nonzero element is prime. */
  def isPrime(a: A): Boolean

  /** Returns the factors of the given nonzero element. */
  def factor(a: A): UniqueFactorizationDomain.Factors[A]
}

object UniqueFactorizationDomain {

  trait Factors[@sp(Byte, Short, Int, Long) A] {
    def unit: A
    def factors: Map[A, Int]
  }

  def apply[A](implicit ev: UniqueFactorizationDomain[A]): UniqueFactorizationDomain[A] = ev

}
