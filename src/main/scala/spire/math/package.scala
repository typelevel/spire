package spire.math

import scala.annotation.tailrec
import java.lang.Long.numberOfTrailingZeros
import java.lang.Math
import java.math.BigInteger

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

  // Since log(n * x) = log(n) + log(x), we can use scala.math.log to
  // approximate log for BigDecimal.
  @tailrec private final def _log(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
    if (n <= maxDouble) BigDecimal(Math.log(n.toDouble)) + sofar
    else _log(n / maxDouble, logMaxDouble + sofar)
  }

  /**
   * exp() implementations
   */
  final def exp(n:Double):Double = Math.exp(n)

  final def exp(n:BigDecimal):BigDecimal = _exp(n, BigDecimal(1))

  // Since exp(a + b) = exp(a) * exp(b), we can use scala.math.log to
  // approximate exp for BigDecimal.
  @tailrec private final def _exp(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
    if (n <= logMaxDouble) BigDecimal(Math.exp(n.toDouble)) * sofar
    else _exp(n - logMaxDouble, maxDouble * sofar)
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

  @tailrec final def _pow(t:Long, b:Long, e:Long): Long = {
    if (e == 0L) t
    else if ((e & 1) == 1) _pow(t * b, b * b, e >> 1L)
    else _pow(t, b * b, e >> 1L)
  }

  final def pow(base:Double, exponent:Double) = Math.pow(base, exponent)

  def gcd(_x: Long, _y: Long): Long = {
    if (_x == 0L) return _y
    if (_y == 0L) return _x
  
    var x = Math.abs(_x)
    var xz = numberOfTrailingZeros(x)
    x >>= xz
  
    var y = Math.abs(_y)
    var yz = numberOfTrailingZeros(y)
    y >>= yz
  
    while (x != y) {
      if (x > y) {
        x -= y
        x >>= numberOfTrailingZeros(x)
      } else {
        y -= x
        y >>= numberOfTrailingZeros(y)
      }
    }
  
    if (xz < yz) x << xz else x << yz
  }


  final def gcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  final def gcd(a: BigInteger, b: BigInteger): BigInteger = a.gcd(b)
}
