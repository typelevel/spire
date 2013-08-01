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
    def approxSqrt(x: BigDecimal): BigDecimal =
      if (x < Double.MaxValue)
        BigDecimal(Math.sqrt(x.toDouble), x.mc)
      else
        approxSqrt(x / Double.MaxValue) * BigDecimal(Math.sqrt(Double.MaxValue), x.mc)

    @tailrec def loop(x: BigDecimal, y: BigDecimal): BigDecimal =
      if (x == y) y else loop(y, ((n / y) + y) / two)

    loop(BigDecimal(0, n.mc), approxSqrt(n))
  }

  def fpow(a: BigDecimal, b: BigDecimal) = spire.math.pow(a, b)
}

object BigDecimalIsTrig {

  @volatile private var eCache: BigDecimal = null
  private[spire] def eFromContext(mc: MathContext): BigDecimal = {
    val eCache0 = eCache

    if (eCache0 == null) {
    } else if (mc == eCache0.mc) {
      return eCache0
    } else if (mc.getPrecision < eCache0.mc.getPrecision) {
      return eCache0.setScale(mc.getPrecision - 1, HALF_UP)
    }

    val result = spire.math.exp(BigDecimal(1, mc))
    eCache = result
    result
  }

  private final val c: Long = 640320L
  private final val c3_over_24: Long = (c * c * c) / 24L

  @volatile private var piCache: BigDecimal = null
  private[spire] def piFromContext(mc: MathContext): BigDecimal = {
    val piCache0 = piCache
    import spire.std.bigDecimal.BigDecimalAlgebra.sqrt

    if (piCache0 == null) {
    } else if (mc == piCache0.mc) {
      return piCache0
    } else if (mc.getPrecision < piCache0.mc.getPrecision) {
      return piCache0.setScale(mc.getPrecision - 1, HALF_UP)
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

    val mc0 = new MathContext(mc.getPrecision + 2, java.math.RoundingMode.FLOOR)

    val n = (mc.getPrecision / java.lang.Math.log10(c3_over_24 / 72)).toInt + 1
    val (p, q, t) = bs(0, n)
    val sqrtc = sqrt(BigDecimal(10005, mc0))
    val quot = (sqrtc * BigDecimal(q * 426880, mc0)) / BigDecimal(t, mc0)
    val r = BigDecimal(quot.bigDecimal, mc).setScale(mc.getPrecision - 1, FLOOR)
    piCache = r
    r
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

  def exp(k: BigDecimal): BigDecimal = spire.math.exp(BigDecimal(k.bigDecimal, mc))
  def log(a: BigDecimal) = spire.math.log(BigDecimal(a.bigDecimal, mc))

  protected[std] val threeSixty = BigDecimal(360)
  def toRadians(a: BigDecimal): BigDecimal = (a * twoPi) / threeSixty
  def toDegrees(a: BigDecimal): BigDecimal = (a * threeSixty) / twoPi

  @inline final def modTwoPi(n: BigDecimal) = (n % twoPi).toDouble

  // we can avoid overflow and minimize fp-error via %2pi
  // TODO: use a more precise formulation of sin/cos/tan?
  //def sin(a: BigDecimal): BigDecimal = BigDecimal(Math.sin(modTwoPi(a)), a.mc)
  //def cos(a: BigDecimal): BigDecimal = BigDecimal(Math.cos(modTwoPi(a)), a.mc)

  def sin(a: BigDecimal): BigDecimal = {
    val x = a % twoPi

    var precision = a.mc.getPrecision + 3
    var leeway = 1000

    @tailrec
    def loop(precision: Int, leeway: Int): BigDecimal = {
      val mc = new MathContext(precision, java.math.RoundingMode.HALF_UP)
      var result = x
      var i = 5
      var m = BigDecimal(120, mc)
      var delta = x.pow(5) / 120 - x.pow(3) / 6
      while (delta.signum != 0) {
        result += delta

        val m1 = m * (i + 1) * (i + 2)
        val m2 = m1 * (i + 3) * (i + 4)
        delta = (x.pow(i + 4) / m2 - x.pow(i + 2) / m1).setScale(precision, HALF_UP)
        m = m2
        i += 4
      }

      if (i <= leeway)
        result.setScale(a.mc.getPrecision - result.precision + result.scale, FLOOR)
      else
        loop(precision + 3, leeway * 1000)
    }

    loop(a.mc.getPrecision + 3, 1000)
  }

  def cos(a: BigDecimal): BigDecimal = {
    val x = a % twoPi

    var precision = a.mc.getPrecision + 3
    var leeway = 1000

    @tailrec
    def loop(precision: Int, leeway: Int): BigDecimal = {
      val mc = new MathContext(precision, java.math.RoundingMode.HALF_UP)
      var result = BigDecimal(1, mc)
      var i = 4
      var m = BigDecimal(24, mc)
      var delta = x.pow(4) / 24 - x.pow(2) / 2
      while (delta.signum != 0) {
        result += delta

        val m1 = m * (i + 1) * (i + 2)
        val m2 = m1 * (i + 3) * (i + 4)
        delta = (x.pow(i + 4) / m2 - x.pow(i + 2) / m1).setScale(precision, HALF_UP)
        m = m2
        i += 4
      }

      if (i <= leeway)
        result.setScale(a.mc.getPrecision - result.precision + result.scale, FLOOR)
      else
        loop(precision + 3, leeway * 1000)
    }

    loop(a.mc.getPrecision + 3, 1000)
  }

  // TODO: we could use a taylor series here as well. but we'd have to
  // compute bernoulli numbers which i don't feel like doing right now.
  def tan(a: BigDecimal): BigDecimal = sin(a) / cos(a)

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
  import BigDecimal.defaultMathContext

  implicit final val BigDecimalAlgebra = new BigDecimalIsField with BigDecimalIsNRoot {}
  implicit final val BigDecimalIsReal = new BigDecimalIsReal {}
  implicit def BigDecimalIsTrig(implicit mc: MathContext = defaultMathContext) =
    new BigDecimalIsTrig(mc)
}
