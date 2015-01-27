package spire.std

import java.lang.Math
import java.math.MathContext

import scala.annotation.tailrec

import BigDecimal.RoundingMode.{CEILING, FLOOR, HALF_UP}

import spire.algebra.{Field, IsRational, NRoot, Order, Signed, Trig}
import spire.math.Rational

trait BigDecimalIsField extends Field[BigDecimal] {
  override def minus(a: BigDecimal, b: BigDecimal): BigDecimal = a - b
  def negate(a: BigDecimal): BigDecimal = -a
  val one: BigDecimal = BigDecimal(1.0)
  def plus(a: BigDecimal, b: BigDecimal): BigDecimal = a + b
  override def pow(a: BigDecimal, b: Int): BigDecimal = a.pow(b)
  override def times(a: BigDecimal, b: BigDecimal): BigDecimal = a * b
  val zero: BigDecimal = BigDecimal(0.0)

  override def fromInt(n: Int): BigDecimal = BigDecimal(n)

  def quot(a: BigDecimal, b: BigDecimal) = a.quot(b)
  def mod(a: BigDecimal, b: BigDecimal) = a % b
  override def quotmod(a: BigDecimal, b: BigDecimal) = a /% b
  def gcd(a: BigDecimal, b: BigDecimal): BigDecimal = {
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

    if (aExp < bExp) gcd0(aVal, aExp, bVal, bExp) else gcd0(bVal, bExp, aVal, aExp)
  }

  override def fromDouble(n: Double): BigDecimal = BigDecimal(n, MathContext.UNLIMITED)
  def div(a: BigDecimal, b: BigDecimal) = a / b
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
    if (n.mc.getPrecision <= 0)
      throw new ArithmeticException("Cannot find the sqrt of a BigDecimal with unlimited precision.")

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

@SerialVersionUID(1L)
class BigDecimalIsTrig(mc: MathContext = BigDecimal.defaultMathContext) extends Trig[BigDecimal] with Serializable {
  import spire.math.Real

  val bits = Real.digitsToBits(mc.getPrecision + 1)

  def fromReal(r: Real): BigDecimal =
    BigDecimal(r(bits).toBigInt) / BigDecimal(BigInt(2).pow(bits))

  lazy val e: BigDecimal = fromReal(Real.e)
  lazy val pi: BigDecimal = fromReal(Real.pi)

  def exp(x: BigDecimal): BigDecimal = fromReal(Real.exp(Real(x)))
  def expm1(x: BigDecimal): BigDecimal = fromReal(Real.exp(Real(x)) - Real.one)
  def log(a: BigDecimal) = fromReal(Real.log(Real(a)))
  def log1p(a: BigDecimal) = fromReal(Real.log(Real(a) + Real.one))

  lazy val degreesPerRadian = fromReal(Real(360) / (Real.pi * Real(2)))
  def toRadians(a: BigDecimal): BigDecimal = a / degreesPerRadian
  def toDegrees(a: BigDecimal): BigDecimal = a * degreesPerRadian

  def sin(a: BigDecimal): BigDecimal = fromReal(Real.sin(Real(a)))
  def cos(a: BigDecimal): BigDecimal = fromReal(Real.cos(Real(a)))
  def tan(a: BigDecimal): BigDecimal = fromReal(Real.tan(Real(a)))

  def asin(a: BigDecimal): BigDecimal = fromReal(Real.asin(Real(a)))
  def acos(a: BigDecimal): BigDecimal = fromReal(Real.acos(Real(a)))
  def atan(a: BigDecimal): BigDecimal = fromReal(Real.atan(Real(a)))

  def atan2(y: BigDecimal, x: BigDecimal): BigDecimal =
    fromReal(Real.atan2(Real(y), Real(x)))

  def sinh(a: BigDecimal): BigDecimal = fromReal(Real.sinh(Real(a)))
  def cosh(a: BigDecimal): BigDecimal = fromReal(Real.cosh(Real(a)))
  def tanh(a: BigDecimal): BigDecimal = fromReal(Real.tanh(Real(a)))
}

trait BigDecimalOrder extends Order[BigDecimal] {
  override def eqv(x: BigDecimal, y: BigDecimal) = x == y
  override def neqv(x: BigDecimal, y: BigDecimal) = x != y
  override def gt(x: BigDecimal, y: BigDecimal) = x > y
  override def gteqv(x: BigDecimal, y: BigDecimal) = x >= y
  override def lt(x: BigDecimal, y: BigDecimal) = x < y
  override def lteqv(x: BigDecimal, y: BigDecimal) = x <= y
  override def min(x: BigDecimal, y: BigDecimal) = x.min(y)
  override def max(x: BigDecimal, y: BigDecimal) = x.max(y)
  // Scala compareTo has no guarantee to return only -1, 0 or 1 as per Spire compare contract,
  // so we call Java's compareTo which does
  def compare(x: BigDecimal, y: BigDecimal) = x.bigDecimal.compareTo(y.bigDecimal)
}

trait BigDecimalIsSigned extends Signed[BigDecimal] {
  def signum(a: BigDecimal): Int = a.signum
  def abs(a: BigDecimal): BigDecimal = a.abs
}

trait BigDecimalIsReal extends IsRational[BigDecimal] with BigDecimalOrder with BigDecimalIsSigned {
  def toDouble(x: BigDecimal): Double = x.toDouble
  def ceil(a: BigDecimal): BigDecimal = a.setScale(0, CEILING)
  def floor(a: BigDecimal): BigDecimal = a.setScale(0, FLOOR)
  def round(a: BigDecimal): BigDecimal = a.setScale(0, HALF_UP)
  def isWhole(a: BigDecimal) = a % 1.0 == 0.0
  def toRational(a:BigDecimal): Rational = Rational(a)
}

@SerialVersionUID(0L)
class BigDecimalAlgebra extends BigDecimalIsField with BigDecimalIsNRoot with BigDecimalIsReal with Serializable

trait BigDecimalInstances {
  import BigDecimal.defaultMathContext

  implicit final val BigDecimalAlgebra = new BigDecimalAlgebra
  implicit def BigDecimalIsTrig(implicit mc: MathContext = defaultMathContext) =
    new BigDecimalIsTrig(mc)

  import spire.math.NumberTag._
  implicit final val BigDecimalTag = new LargeTag[BigDecimal](Approximate, BigDecimal(0))
}
