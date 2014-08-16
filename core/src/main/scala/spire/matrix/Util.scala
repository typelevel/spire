package spire.matrix

/**
 * Typeclass representing the properties of Double
 *
 * [1] LAPACK Users' Guide.
 *     E Anderson, Z Bai, Christian H. Bischof, S Blackford, J Demmel,
 *     J Dongarra, J Du Croz, A Greenbaum, S Hammarling, A McKenney,
 *     and D Sorensen.
 *     Society for Industrial and Applied Mathematics,
 *     Philadelphia, PA, Third.
 */
trait NumericPropertiesOfDouble {
    /**
     * Relative epsilon machine a la LAPACK, i.e. 1/2 of the definition
     * used in ISO C/C++. C.f. [1] for a decent introduction.
     *
     * DLAMCH('Epsilon') in [1]
     *
     * [1] http://en.wikipedia.org/wiki/Machine_epsilon#Approximation_using_Java
     */
    val epsilonMachine = java.lang.Double.longBitsToDouble((1023L-53) << 52)

    /**
     * Safe minimum s such that 1/s does not overflow
     *
     * DLAMCH('Safe Minimum') in [1]
     */
    val safeMinimum = {
      val sfmin = java.lang.Double.MIN_NORMAL
      val small = 1.0/java.lang.Double.MAX_VALUE
      if(small >= sfmin) small*(1.0 + epsilonMachine)
      else sfmin
    }

    /**
     * Base used for the binary representation of mantissa and exponent
     *
     * DLAMCH('Base') in [1]
     */
    val base = 2

    /**
     * Precision
     *
     * DLAMCH('Precision') in [1]
     */
    val precision = base*epsilonMachine

    /** The next number greater than this is +Infinity */
    val overflow = java.lang.Double.MAX_VALUE
}
