package spire
package algebra

import spire.math.{Integral, SafeLong}

/** A unique factorization domain is a commutative ring in which each element can be written
  * as a product of prime elements and a unit.
  * 
  * This trait is outside the commutative ring hierarchy, because the factorization algorithms
  * are costly, and a deliberate choice should be made by the user, for example to use
  * probabilistic algorithms with a specified probability of failure.
  */
trait UniqueFactorizationDomain[@sp(Byte, Short, Int, Long) A] extends Any {
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

  case class WrapFactors[A:Integral](safeLongFactors: spire.math.prime.Factors) extends Factors[A] {
    def unit: A = safeLongFactors.sign match {
      case Sign.Negative => Integral[A].negate(Integral[A].one)
      case Sign.Positive => Integral[A].one
      case _ => sys.error("Factorization of zero is undefined.")
    }
    override def factors: Map[A, Int] = safeLongFactors.factors.map {
      case (f, exp) => ((Integral[A].fromBigInt(f.toBigInt), exp))
    }
  }

  implicit def uniqueFactorizationDomainFromIntegral[A](implicit A: Integral[A]): UniqueFactorizationDomain[A] =
    new UniqueFactorizationDomain[A] {
      def isPrime(a: A): Boolean = SafeLong(A.toBigInt(a)).isPrime
      def factor(a: A): Factors[A] = WrapFactors[A](SafeLong(A.toBigInt(a)).factor)(A)
    }

}
