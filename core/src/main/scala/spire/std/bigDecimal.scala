package spire.std

import spire.algebra._

import scala.annotation.tailrec

import BigDecimal.RoundingMode.{CEILING, FLOOR, HALF_UP}

import java.lang.Math
import java.math.MathContext

trait BigDecimalIsRing extends Ring[BigDecimal] {
  override def minus(a:BigDecimal, b:BigDecimal): BigDecimal = a - b
  def negate(a:BigDecimal): BigDecimal = -a
  val one: BigDecimal = BigDecimal(1.0)
  def plus(a:BigDecimal, b:BigDecimal): BigDecimal = a + b
  override def pow(a:BigDecimal, b:Int): BigDecimal = a.pow(b)
  override def times(a:BigDecimal, b:BigDecimal): BigDecimal = a * b
  val zero: BigDecimal = BigDecimal(0.0)

  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
}

trait BigDecimalIsEuclideanRing extends EuclideanRing[BigDecimal] with BigDecimalIsRing {
  def quot(a:BigDecimal, b:BigDecimal) = a.quot(b)
  def mod(a:BigDecimal, b:BigDecimal) = a % b
  override def quotmod(a:BigDecimal, b:BigDecimal) = a /% b
  def gcd(a:BigDecimal, b:BigDecimal):BigDecimal = {
    import java.math.BigInteger

    val Two = BigInteger.valueOf(2)
    val Five = BigInteger.valueOf(5)
    val Ten = BigInteger.TEN

    @tailrec
    def reduce0(n: BigInteger, prime: BigInteger, shift: Int = 0): (Int, BigInteger) = {
      val Array(div, rem) = n.divideAndRemainder(prime)
      if (n == BigInteger.ZERO || rem != BigInteger.ZERO) {
        (shift, n)
      } else {
        reduce0(div, prime, shift + 1)
      }
    }

    def reduce(n: BigInteger): (Int, Int, BigInteger) = {
      val (shift10, n0) = reduce0(n, Ten)
      val (shift5, n1) = reduce0(n0, Five)
      val (shift2, n2) = reduce0(n1, Two)
      (shift2 + shift10, shift5 + shift10, n2)
    }

    def gcd0(val0: BigInteger, exp0: Int, val1: BigInteger, exp1: Int): BigDecimal = {
      val (shiftTwo0, shiftFive0, shifted0) = reduce(val0)
      val (shiftTwo1, shiftFive1, shifted1) = reduce(val1)
      val sharedTwo = spire.math.min(shiftTwo0, shiftTwo1 + exp1 - exp0)
      val sharedFive = spire.math.min(shiftFive0, shiftFive1 + exp1 - exp0)
      val reshift = Two.pow(sharedTwo).multiply(Five.pow(sharedFive))
      val n = (shifted0 gcd shifted1).multiply(reshift)
      BigDecimal(new java.math.BigDecimal(n, -exp0))
    }

    val aJbd = a.bigDecimal
    val aVal = aJbd.unscaledValue.abs
    val aExp = -aJbd.scale

    val bJbd = b.bigDecimal
    val bVal = bJbd.unscaledValue.abs
    val bExp = -bJbd.scale

    if (aExp < bExp) gcd0(aVal, aExp, bVal, bExp)
    else gcd0(bVal, bExp, aVal, aExp)
  }
}

trait BigDecimalIsField extends Field[BigDecimal] with BigDecimalIsEuclideanRing {
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n)
  def div(a:BigDecimal, b:BigDecimal) = a / b
}

trait BigDecimalIsNRoot extends NRoot[BigDecimal] {
  def nroot(a: BigDecimal, k: Int): BigDecimal = {
    if (a.mc.getPrecision <= 0)
      throw new ArithmeticException("Cannot find the nroot of a BigDecimal with unlimited precision.")
    NRoot.nroot(a, k, a.mc)
  }

  private[this] val two = BigDecimal(2)

  // this is newton's method
  override def sqrt(n: BigDecimal): BigDecimal = {
    var x: BigDecimal = BigDecimal(0, n.mc)
    var y: BigDecimal = BigDecimal(Math.sqrt(n.toDouble), n.mc)
    while (x != y) {
      x = y;
      y = ((n / x) + x) / two
    }
    y
  }

  def fpow(a:BigDecimal, b:BigDecimal) = spire.math.pow(a, b)
}

object BigDecimalIsTrig {
  // TODO: optimize. also consider improving exp() and just using exp(1)
  @volatile private var eCache: BigDecimal = null
  private def eFromContext(mc: MathContext): BigDecimal = {
    import spire.std.bigDecimal.BigDecimalAlgebra.sqrt
    val eCache0 = eCache

    if (eCache0 == null) {
    } else if (mc == eCache0.mc) {
      return eCache0
    } else if (mc.getPrecision < eCache0.mc.getPrecision) {
      return eCache0.setScale(mc.getPrecision, BigDecimal.RoundingMode.HALF_UP)
    }

    val scale = BigInt(10).pow(mc.getPrecision)

    /**
     * e = sum(k=0 -> inf)(1/k!) = 1 + 1 + 1/2 + 1/6 + 1/24 + ...
     *
     * To avoid doing lots of division, we will just represent e as a
     * rational approximation (num/denom), multiplying both by each
     * new k and adding 1 to the numerator.
     *
     * We keep track of the current value of k! as n, since once n exceeds
     * the scale we have small fractions which won't be representable in
     * the current MathContext.
     */
    var num = BigInt(2)
    var denom = BigInt(1)
    var n = BigInt(1)
    var m = 2
    while (n < scale) {
      num = num * m + 1
      denom = denom * m
      n = n * m
      m += 1
    }

    val result = BigDecimal(num, mc) / BigDecimal(denom, mc)
    eCache = result
    result
  }

  private final val c: Long = 640320L
  private final val c3_over_24: Long = (c * c * c) / 24L

  @volatile private var piCache: BigDecimal = null
  private def piFromContext(mc: MathContext): BigDecimal = {
    val piCache0 = piCache
    import spire.std.bigDecimal.BigDecimalAlgebra.sqrt

    if (piCache0 == null) {
    } else if (mc == piCache0.mc) {
      return piCache0
    } else if (mc.getPrecision < piCache0.mc.getPrecision) {
      return piCache0.setScale(mc.getPrecision, BigDecimal.RoundingMode.HALF_UP)
    }

    /**
     * This is an implementation of Chudnovsky's algorithm for calculating
     * pi, using binary splitting. The implementation is based on the one
     * found at http://www.craig-wood.com/nick/articles/pi-chudnovsky/.
     */
    def bs(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
      if (b - a == 1) {
        val (pab: BigInt, qab: BigInt) = if (a == 0)
          (BigInt(1), BigInt(1))
        else
          ((6 * a - 5) * (2 * a - 1) * (6 * a - 1), a * a * a * c3_over_24)

        val tab = pab * (a * 545140134 + 13591409)
        if ((a & 1) == 0)
          (pab, qab, tab)
        else
          (pab, qab, -tab)
      } else {
        val m = (a + b) >> 1
        val (pam, qam, tam) = bs(a, m)
        val (pmb, qmb, tmb) = bs(m, b)
        val pab = pam * pmb
        val qab = qam * qmb
        val tab = qmb * tam + pam * tmb
        (pab, qab, tab)
      }
    }

    val n = java.lang.Math.log10(c3_over_24 / 72).toInt
    val (p, q, t) = bs(0, n)
    val sqrtc = sqrt(BigDecimal(10005, mc))
    val result = (sqrtc * BigDecimal(q * 426880, mc)) / BigDecimal(t, mc)
    piCache = result
    result
  }

  // TODO: delete or move these into the BigDecimalIsTrig class
  protected[std] final val zero = BigDecimal(0)
  protected[std] final val one = BigDecimal(1)
  protected[std] final val two = BigDecimal(2)
  protected[std] final val four = BigDecimal(4)
}

// ugh. (apart from pi, e, exp) this is very imprecise.
class BigDecimalIsTrig(mc: MathContext = BigDecimal.defaultMathContext) extends Trig[BigDecimal] {
  import BigDecimalIsTrig._

  lazy val e: BigDecimal = eFromContext(mc)
  lazy val pi: BigDecimal = piFromContext(mc)
  protected[std] lazy val twoPi = pi * 2

  def exp(k: BigDecimal): BigDecimal = spire.math.exp(k)
  def log(a: BigDecimal) = spire.math.log(a)

  protected[std] val threeSixty = BigDecimal(360)
  def toRadians(a: BigDecimal): BigDecimal = (a * twoPi) / threeSixty
  def toDegrees(a: BigDecimal): BigDecimal = (a * threeSixty) / twoPi

  @inline final def modTwoPi(n: BigDecimal) = (n % twoPi).toDouble

  // we can avoid overflow and minimize fp-error via %2pi
  // TODO: use a more precise formulation of sin/cos/tan?
  def sin(a: BigDecimal): BigDecimal = BigDecimal(Math.sin(modTwoPi(a)), a.mc)
  def cos(a: BigDecimal): BigDecimal = BigDecimal(Math.cos(modTwoPi(a)), a.mc)
  def tan(a: BigDecimal): BigDecimal = BigDecimal(Math.tan(modTwoPi(a)), a.mc)

  // 'a' will range from -1.0 to 1.0
  // TODO: use a more precise formulation of asin/acos/atan?
  def asin(a: BigDecimal): BigDecimal = BigDecimal(Math.asin(a.toDouble), a.mc)
  def acos(a: BigDecimal): BigDecimal = BigDecimal(Math.acos(a.toDouble), a.mc)
  def atan(a: BigDecimal): BigDecimal = BigDecimal(Math.atan(a.toDouble), a.mc)

  def atan2(y: BigDecimal, x: BigDecimal): BigDecimal = if (x > zero) {
    atan(y / x)
  } else if (x < zero) {
    if (y >= zero) atan(y / x) + pi else atan(y / x) - pi
  } else {
    if (y > zero) pi / two else if (y < zero) -pi / two else zero
  }

  def sinh(x: BigDecimal): BigDecimal = {
    val ex = exp(x)
    (ex * ex - one) / (two * ex)
  }
  def cosh(x: BigDecimal): BigDecimal = {
    val ex = exp(x)
    (ex * ex + one) / (two * ex)
  }
  def tanh(x: BigDecimal): BigDecimal = {
    val ex = exp(x)
    val ex2 = ex * ex
    (ex2 - one) / (ex2 + one)
  }
}

trait BigDecimalOrder extends Order[BigDecimal] {
  override def eqv(x:BigDecimal, y:BigDecimal) = x == y
  override def neqv(x:BigDecimal, y:BigDecimal) = x != y
  override def gt(x: BigDecimal, y: BigDecimal) = x > y
  override def gteqv(x: BigDecimal, y: BigDecimal) = x >= y
  override def lt(x: BigDecimal, y: BigDecimal) = x < y
  override def lteqv(x: BigDecimal, y: BigDecimal) = x <= y
  override def min(x: BigDecimal, y: BigDecimal) = x.min(y)
  override def max(x: BigDecimal, y: BigDecimal) = x.max(y)
  def compare(x: BigDecimal, y: BigDecimal) = x.compare(y)
}

trait BigDecimalIsSigned extends Signed[BigDecimal] {
  def signum(a: BigDecimal): Int = a.signum
  def abs(a: BigDecimal): BigDecimal = a.abs
}

trait BigDecimalIsReal extends IsReal[BigDecimal]
with BigDecimalOrder with BigDecimalIsSigned {
  def toDouble(x: BigDecimal): Double = x.toDouble
  def ceil(a:BigDecimal): BigDecimal = a.setScale(0, CEILING)
  def floor(a:BigDecimal): BigDecimal = a.setScale(0, FLOOR)
  def round(a:BigDecimal): BigDecimal = a.setScale(0, HALF_UP)
  def isWhole(a:BigDecimal) = a % 1.0 == 0.0
}

trait BigDecimalInstances {
  implicit object BigDecimalAlgebra extends BigDecimalIsField with BigDecimalIsNRoot
  implicit object BigDecimalIsReal extends BigDecimalIsReal
  implicit def BigDecimalIsTrig(implicit mc: MathContext = BigDecimal.defaultMathContext) =
    new BigDecimalIsTrig(mc)
}
