package numerics

package object math {

  /**
   * Integral exponentiation function, e.g. x^y
   *
   * If base^exponent doesn't fit in a Long, the result will overflow (unlike
   * scala.math.pow which will return +/- Infinity). 
   */
  final def xpow(base:Long, exponent:Long):Long = {
    if (exponent < 0L) sys.error("illegal exponent")
    else if (base == 0L) 0L
    else if (base >= 0L) _xpow(base, exponent)
    else if (base % 2 == 0) _xpow(-base, exponent)
    else -_xpow(-base, exponent)
  }

  /**
   * Internal long exponentiation function. Only works for positive b, e.
   */
  private final def _xpow(_b:Long, _e:Long):Long = {
    var b = _b
    var e = _e
    var t = 1L
    while (true) {
      if (e % 2 == 1L) t *= b
      e /= 2L
      if (t < 0L) sys.error("result exceeds Long")
      if (e == 0L) return t
      b *= b
    }
    t
  }

  final def xpow(base:Int, exponent:Int):Int = {
    val longResult = xpow(base.toLong, exponent.toLong)
    val intResult = longResult.toInt
    if (intResult != longResult) sys.error("result exceeds Int")
    intResult
  }
}
