package spire.math

import scala.annotation.tailrec
import java.lang.Math

// TODO: in 2.9 there is a bug where package objects break overloading.
// in 2.10 and beyond this should just be the spire.math package object.

object fun {
  // largest possible double as BigDecimal
  private final val maxDouble = BigDecimal(Double.MaxValue)

  // natural log of largest possible double as BigDecimal
  private final val logMaxDouble = BigDecimal(scala.math.log(Double.MaxValue))

  // e^logMaxDouble as BigDecimal
  private final val expLogMaxDouble = BigDecimal(scala.math.exp(scala.math.log(Double.MaxValue)))

  final def log(n:BigDecimal):BigDecimal = if (n < 0) sys.error("!!!") else log(n, BigDecimal(0))

  // Since log(n * x) = log(n) + log(x), we can use scala.math.log to
  // approximate log for BigDecimal.
  @tailrec private final def log(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
    if (n < 0) {
      log(-n, -sofar)
    } else if (n <= maxDouble) {
      BigDecimal(scala.math.log(n.toDouble)) + sofar
    } else {
      log(n / maxDouble, logMaxDouble + sofar)
    }
  }

  final def exp(n:BigDecimal):BigDecimal = exp(n, BigDecimal(1))

  // Since exp(a + b) = exp(a) * exp(b), we can use scala.math.log to
  // approximate exp for BigDecimal.
  @tailrec private final def exp(n:BigDecimal, sofar:BigDecimal): BigDecimal = {
    if (n <= expLogMaxDouble) {
      BigDecimal(scala.math.exp(n.toDouble)) * sofar
    } else {
      exp(n - expLogMaxDouble, expLogMaxDouble * sofar)
    }
  }

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
   * scala.math.pow which will return +/- Infinity). 
   */
  final def pow(base:Long, ex:Long):Long = if (ex < 0L) {
    _inv_pow(base, ex)
  } else {
    _pow(1L, base, ex)
  }

  // inverse powers for integers will return -1L, 0L, 1L, or throw an error.
  private final def _inv_pow(base:Long, ex:Long) = if(base == 0L) {
    throw new Exception("zero can't be raised to negative power")
  } else if (base == 1L) {
    1L
  } else if (base == -1L) {
    if (ex % 2L == 0L) -1L else 1L
  } else {
    0L
  }

  // tail-recursive helper method for pow(Long, Long)
  @tailrec private final def _pow(t:Long, b:Long, e:Long): Long = {
    if (e == 0L) return t
    _pow(if (e % 2L == 1L) t * b else t, b * b, e / 2L)
  }

  final def pow(base:Double, ex:Double):Double = Math.pow(base, ex)

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
