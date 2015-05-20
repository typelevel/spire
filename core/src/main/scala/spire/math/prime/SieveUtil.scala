package spire.math.prime

import spire.math.SafeLong

object SieveUtil {

  /**
   * Represents a prime factor which we need to keep track of.
   *
   * The first field 'p' is the prime itself. The 'next' field is the
   * next multiple of the prime that we expect to see.
   *
   * We use a slightly non-standard compare() function so that the
   * factor with the smallest 'next' field will be the largest.
   */
  case class Factor(p: SafeLong, var next: SafeLong) extends Ordered[Factor] {
    def compare(that: Factor): Int = -(this.next compare that.next)
  }

  /**
   * Reprsents a prime factor which we need to keep track of.
   *
   * Similar to Factor, but in this case the prime is small enough that
   * it fits in an Int. This means that each of our sieve segments will
   * contain at least one multiple of 'p' if not more. So we can use a
   * slightly more compact data structure.
   */
  case class FastFactor(p: Int, var m: SafeLong)

  /**
   * This class simply wraps an Array[FastFactor]. Its only real purpose
   * is to allow us to lazily initialize our fast factors (which we can
   * only do after constructing our first sieve segment).
   */
  case class FastFactors(var arr: Array[FastFactor])

  object FastFactors {
    def empty = FastFactors(new Array[FastFactor](0))
  }
}
