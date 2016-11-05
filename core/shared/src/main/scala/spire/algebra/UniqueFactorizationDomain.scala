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

  import spire.math.Integral

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

  // TODO: move somewhere
  implicit def uniqueFactorizationDomain[A](implicit ev: Integral[A]): UniqueFactorizationDomain[A] =
    new UniqueFactorizationDomain[A] {
      import spire.math.SafeLong
      import SafeLong.SafeLongAlgebra
      def isPrime(a: A): Boolean = SafeLongAlgebra.isPrime(SafeLong(ev.toBigInt(a)))
      def factor(a: A): Factors[A] = WrapFactors[A](SafeLongAlgebra.factor(SafeLong(ev.toBigInt(a))))
    }

}
