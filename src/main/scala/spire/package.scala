package spire.math

import scala.annotation.tailrec

// TODO: in 2.9 there is a bug where package objects break overloading.
// in 2.10 and beyond this should just be the spire.math package object.

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

  @tailrec def euclidGcd(a: Long, b: Long): Long = if (b == 0L) {
    scala.math.abs(a)
  } else {
    euclidGcd(b, a % b)
  }

  @tailrec def binaryGcd(a: Long, b: Long, shifts: Int = 0): Long = {
    // these are the terminal cases
    if (a == b) a << shifts
    else if (a == 0L) b << shifts
    else if (b == 0L) a << shifts

    // if a is even
    else if ((~a & 1L) != 0L) {
      // factor out 2 from a
      if ((b & 1L) != 0L) binaryGcd(a >> 1, b, shifts)
      // remove out 2 from both, remembering to reapply it at the end.
      else binaryGcd(a >> 1L, b >> 1L, shifts + 1)
    }

    // if b is even, factor out 2
    else if ((~b & 1L) != 0L) binaryGcd(a, b >> 1L, shifts)

    // reduce whichever argument is larger
    else if (a > b) binaryGcd((a - b) >> 1L, b, shifts)
    else binaryGcd((b - a) >> 1L, a, shifts)
  }
}
