package spire.math

import scala.annotation.tailrec
import java.lang.Math

// TODO: in 2.9 there is a bug where package objects break overloading.
// in 2.10 and beyond this should just be the spire.math package object.

object fun {
  // largest possible double as BigDecimal
  private final val maxDouble = BigDecimal(Double.MaxValue)

  // natural log of largest possible double as BigDecimal
  private final val logMaxDouble = BigDecimal(Math.log(Double.MaxValue))

  // e^logMaxDouble as BigDecimal
  private final val expLogMaxDouble = BigDecimal(Math.exp(Math.log(Double.MaxValue)))

  /**
   * log() implementations
   */

  final def log(n:Double):Double = Math.log(n)

  final def log(n:BigDecimal):BigDecimal = {
    if (n < 0) sys.error("invalid argument: %s" format n)
    else _log(n, BigDecimal(0))
  }

  // TODO: return to tailrec method when SI-5788 is resolved
  //// Since log(n * x) = log(n) + log(x), we can use scala.math.log to
  //// approximate log for BigDecimal.
  //@tailrec private final def log(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
  //  if (n < 0) {
  //    log(-n, -sofar)
  //  } else if (n <= maxDouble) {
  //    BigDecimal(scala.math.log(n.toDouble)) + sofar
  //  } else {
  //    log(n / maxDouble, logMaxDouble + sofar)
  //  }
  //}

  private final def _log(_n:BigDecimal, _sofar:BigDecimal): BigDecimal = {
    if (_n < 0) return _log(-_n, -_sofar)
    var n = _n
    var sofar = _sofar
    while (true) {
      if (n <= maxDouble) return BigDecimal(Math.log(n.toDouble)) + sofar
      n /= maxDouble
      sofar += logMaxDouble
    }
    sofar // unused, required by scalac
  }

  /**
   * exp() implementations
   */
  final def exp(n:Double):Double = Math.exp(n)

  final def exp(n:BigDecimal):BigDecimal = _exp(n, BigDecimal(1))

  // TODO: return to tailrec method when SI-5788 is resolved
  //// Since exp(a + b) = exp(a) * exp(b), we can use scala.math.log to
  //// approximate exp for BigDecimal.
  //@tailrec private final def exp(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
  //  if (n <= logMaxDouble) {
  //    BigDecimal(scala.math.exp(n.toDouble)) * sofar
  //  } else {
  //    exp(n - logMaxDouble, maxDouble * sofar)
  //  }
  //}

  // Since exp(a + b) = exp(a) * exp(b), we can use scala.math.log to
  // approximate exp for BigDecimal.
  private final def _exp(_n:BigDecimal, _sofar:BigDecimal): BigDecimal = {
    var n = _n
    var sofar = _sofar
    while (true) {
      if (n <= logMaxDouble) return BigDecimal(Math.exp(n.toDouble)) * sofar
      n -= logMaxDouble
      sofar *= maxDouble
    }
    n // unused, required by scalac
  }

  /**
   * pow() implementations
   */

  // Since a^b = e^(log(a) * b) we can use exp and log to write pow.
  // TODO: doesn't make precision guarantees, but it's better than nothing.
  private val maxIntEx = BigDecimal(999999999)
  private val minIntEx = BigDecimal(-999999999)

  final def pow(base:BigDecimal, ex:BigDecimal) =
    if (ex.isValidInt && ex <= maxIntEx && ex >= minIntEx) base.pow(ex.toInt)
    else exp(log(base) * ex)

  /**
   * Exponentiation function, e.g. x^y
   *
   * If base^ex doesn't fit in a Long, the result will overflow (unlike
   * Math.pow which will return +/- Infinity). 
   */
  final def pow(base:Long, ex:Long):Long = if (ex < 0L) {
    if(base == 0L) sys.error("zero can't be raised to negative power")
    else if (base == 1L) 1L
    else if (base == -1L) if ((ex & 1L) == 0L) -1L else 1L
    else 0L
  } else {
    _pow(1L, base, ex)
  }

  // TODO: return to tailrec method when SI-5788 is resolved
  //// tail-recursive helper method for pow(Long, Long)
  //@tailrec private final def _pow(t:Long, b:Long, e:Int): Long = {
  //  if (e == 0) return t
  //  _pow(if ((e & 1) == 1) t * b else t, b * b, e / 2)
  //}

  private final def _pow(_t:Long, _b:Long, _e:Long): Long = {
    var t = _t
    var b = _b
    var e = _e
    while (true) {
      if (e == 0) return t
      if ((e & 1) == 1) t *= b
      b *= b
      e /= 2
    }
    t // unused, required by scalac
  }

  final def pow(base:Double, exponent:Double) = Math.pow(base, exponent)

  // TODO: return to tailrec method when SI-5788 is resolved
  //@tailrec def gcd(a: Long, b: Long): Long = if (b == 0L) {
  //  scala.math.abs(a)
  //} else {
  //  gcd(b, a % b)
  //}

  def gcd(_a: Long, _b: Long): Long = {
    var a = _a
    var b = _b
    while (true) {
      if (b == 0L) return Math.abs(a)
      var tmp = a
      a = b
      b = tmp % b
    }
    a // unused, required by scalac
  }
}
