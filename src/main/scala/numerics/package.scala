package numerics.math

import scala.annotation.tailrec

object fun {

  /**
   *
   */
  final def pow(base:Int, exponent:Int):Int = {
    pow(base.toLong, exponent.toLong).toInt
  }

  /**
   * Exponentiation function, e.g. x^y
   *
   * If base^exponent doesn't fit in a Long, the result will overflow (unlike
   * scala.math.pow which will return +/- Infinity). 
   */
  final def pow(base:Long, exponent:Long):Long = {
    if (exponent < 0L) sys.error("illegal exponent")
    _pow(1L, base, exponent)
  }

  // tail-recursive helper method for pow(Long, Long)
  @tailrec private final def _pow(t:Long, b:Long, e:Long): Long = {
    if (e == 0L) return t
    _pow(if (e % 2 == 1L) t * b else t, b * b, e / 2L)
  }

  final def pow(base:Float, exponent:Float):Float = {
    java.lang.Math.pow(base.toDouble, exponent.toDouble).toFloat
  }

  final def pow(base:Double, exponent:Double):Double = {
    java.lang.Math.pow(base, exponent)
  }
}
