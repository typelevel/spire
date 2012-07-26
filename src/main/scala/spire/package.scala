package spire.math

import scala.annotation.tailrec

// TODO: in 2.9 there is a bug where package objects break overloading.
// in 2.10 and beyond this should just be the spire.math package object.

object fun {
  // largest possible double as BigDecimal
  private final val maxDouble = BigDecimal(Double.MaxValue)

  // natural log of largest possible double as BigDecimal
  private final val logMaxDouble = BigDecimal(scala.math.log(Double.MaxValue))

  // e^logMaxDouble as BigDecimal
  private final val expLogMaxDouble = BigDecimal(scala.math.exp(scala.math.log(Double.MaxValue)))

  final def log(n:BigDecimal):BigDecimal = {
    if (n < 0) sys.error("invalid argument: %s" format n) else log(n, BigDecimal(0))
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

  private final def log(_n:BigDecimal, _sofar:BigDecimal): BigDecimal = {
    if (_n < 0) return log(-_n, -_sofar)
    var n = _n
    var sofar = _sofar
    while (true) {
      if (n <= maxDouble) return BigDecimal(scala.math.log(n.toDouble)) + sofar
      n /= maxDouble
      sofar += logMaxDouble
    }
    sofar // unused, required by scalac
  }

  final def exp(n:BigDecimal):BigDecimal = exp(n, BigDecimal(1))

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
  private final def exp(_n:BigDecimal, _sofar:BigDecimal): BigDecimal = {
    var n = _n
    var sofar = _sofar
    while (true) {
      if (n <= logMaxDouble) return BigDecimal(scala.math.exp(n.toDouble)) * sofar
      n -= logMaxDouble
      sofar *= maxDouble
    }
    n // unused, required by scalac
  }

  // Since a^b = e^(log(a) * b) we can use exp and log to write pow.
  // TODO: doesn't make precision guarantees, but it's better than nothing.
  final def pow(base:BigDecimal, exponent:BigDecimal) = exp(log(base) * exponent)

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
    if ((exponent & 1) == 0) -1L else 1L
  } else {
    0L
  }

  // TODO: return to tailrec method when SI-5788 is resolved
  //// tail-recursive helper method for pow(Long, Long)
  //@tailrec private final def _pow(t:Long, b:Long, e:Int): Long = {
  //  if (e == 0) return t
  //  _pow(if ((e & 1) == 1) t * b else t, b * b, e / 2)
  //}

  private final def _pow(_t:Long, _b:Long, _e:Int): Long = {
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

  final def pow(base:Double, exponent:Double):Double = java.lang.Math.pow(base, exponent)

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
      if (b == 0L) return scala.math.abs(a)
      var tmp = a
      a = b
      b = tmp % b
    }
    a // unused, required by scalac
  }
}
