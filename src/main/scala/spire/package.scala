package spire.math

import scala.annotation.tailrec

object fun {
  // TODO: add nroot(base:Long, n:Long)

  /**
   * Exponentiation function, e.g. x^y
   *
   * If base^exponent doesn't fit in a Long, the result will overflow (unlike
   * scala.math.pow which will return +/- Infinity). 
   */
  final def pow(base:Long, exponent:Int):Long = if (exponent < 0L) {
    _inv_pow(base, exponent)
  } else {
    _pow(1L, base, exponent)
  }

  // inverse powers for integers will return -1L, 0L, 1L, or throw an error.
  private final def _inv_pow(base:Long, exponent:Int) = if(base == 0L) {
    throw new Exception("zero can't be raised to negative power")
  } else if (base == 1L) {
    1L
  } else if (base == -1L) {
    if (exponent % 2 == 0) -1L else 1L
  } else {
    0L
  }

  // tail-recursive helper method for pow(Long, Long)
  @tailrec private final def _pow(t:Long, b:Long, e:Int): Long = {
    if (e == 0) return t
    _pow(if (e % 2 == 1) t * b else t, b * b, e / 2)
  }

  final def pow(base:Double, exponent:Double):Double = {
    java.lang.Math.pow(base, exponent)
  }
}
