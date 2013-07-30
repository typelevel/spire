package spire.matrix

/**
 * Constants used throughout this package
 */
object Constants {

  /**
   * Magic number to specify that the end of an index range shall be equal to
   * the relevant dimenstion (i.e. vector length, number of rows, or columns,
   * etc)
   */
  val End = -111
}

/**
 * Typeclass representing the properties of Double
 */
trait NumericPropertiesOfDouble {
    /**
     * Relative epsilon machine a la LAPACK, i.e. 1/2 of the definition
     * used in ISO C/C++. C.f. [1] for a decent introduction.
     *
     * [1] http://en.wikipedia.org/wiki/Machine_epsilon#Approximation_using_Java
     */
    val epsilonMachine = java.lang.Double.longBitsToDouble((1023L-53) << 52)

    /**
     * Safe minimum s such that 1/s does not overflow
     */
    val safeMinimum = {
      val sfmin = java.lang.Double.MIN_NORMAL
      val small = 1.0/java.lang.Double.MAX_VALUE
      if(small >= sfmin) small*(1.0 + epsilonMachine)
      else sfmin
    }

    /** Base used for the binary representation of mantissa and exponent */
    val base = 2

    /** Precision */
    val precision = base*epsilonMachine
}
