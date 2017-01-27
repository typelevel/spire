package spire
package std

import java.lang.Math
import java.math.MathContext

import BigDecimal.RoundingMode.{CEILING, FLOOR, HALF_UP}

import spire.algebra._
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
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n, MathContext.UNLIMITED)

  def div(a: BigDecimal, b: BigDecimal): BigDecimal = a / b
}

trait BigDecimalIsNRoot extends NRoot[BigDecimal] {
  def nroot(a: BigDecimal, k: Int): BigDecimal = {
    if (a.mc.getPrecision <= 0)
      throw new ArithmeticException("Cannot find the nroot of a BigDecimal with unlimited precision.")
    spire.math.nroot(a, k, a.mc)
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

  def fpow(a: BigDecimal, b: BigDecimal): BigDecimal = spire.math.pow(a, b)
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
  def log(a: BigDecimal): BigDecimal = fromReal(Real.log(Real(a)))
  def log1p(a: BigDecimal): BigDecimal = fromReal(Real.log(Real(a) + Real.one))

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
  override def eqv(x: BigDecimal, y: BigDecimal): Boolean = x == y
  override def neqv(x: BigDecimal, y: BigDecimal): Boolean = x != y
  override def gt(x: BigDecimal, y: BigDecimal): Boolean = x > y
  override def gteqv(x: BigDecimal, y: BigDecimal): Boolean = x >= y
  override def lt(x: BigDecimal, y: BigDecimal): Boolean = x < y
  override def lteqv(x: BigDecimal, y: BigDecimal): Boolean = x <= y
  override def min(x: BigDecimal, y: BigDecimal): BigDecimal = x.min(y)
  override def max(x: BigDecimal, y: BigDecimal): BigDecimal = x.max(y)
  // Scala compareTo has no guarantee to return only -1, 0 or 1 as per Spire compare contract,
  // so we call Java's compareTo which does
  def compare(x: BigDecimal, y: BigDecimal): Int = x.bigDecimal.compareTo(y.bigDecimal)
}

trait BigDecimalIsSigned extends Signed[BigDecimal] with BigDecimalOrder {
  override def signum(a: BigDecimal): Int = a.signum
  override def abs(a: BigDecimal): BigDecimal = a.abs
}

trait BigDecimalTruncatedDivision extends TruncatedDivisionCRing[BigDecimal] with BigDecimalIsSigned {
  def toBigIntOption(x: BigDecimal): Option[BigInt] =
    if (x.isWhole) Some(x.toBigInt) else None
  def tquot(a: BigDecimal, b: BigDecimal): BigDecimal = a.quot(b)
  def tmod(a: BigDecimal, b: BigDecimal): BigDecimal = a % b
  override def tquotmod(a: BigDecimal, b: BigDecimal): (BigDecimal, BigDecimal) = a /% b
}

trait BigDecimalIsReal extends IsRational[BigDecimal] with BigDecimalTruncatedDivision {
  def toDouble(x: BigDecimal): Double = x.toDouble
  def ceil(a: BigDecimal): BigDecimal = a.setScale(0, CEILING)
  def floor(a: BigDecimal): BigDecimal = a.setScale(0, FLOOR)
  def round(a: BigDecimal): BigDecimal = a.setScale(0, HALF_UP)
  def isWhole(a: BigDecimal): Boolean = a.isWhole
  def toRational(a:BigDecimal): Rational = Rational(a)
}

@SerialVersionUID(0L)
class BigDecimalAlgebra extends BigDecimalIsField
    with BigDecimalIsNRoot
    with BigDecimalIsReal
    with Serializable

trait BigDecimalInstances {
  import BigDecimal.defaultMathContext

  implicit final val BigDecimalAlgebra = new BigDecimalAlgebra
  implicit def BigDecimalIsTrig(implicit mc: MathContext = defaultMathContext): BigDecimalIsTrig =
    new BigDecimalIsTrig(mc)

  import spire.math.NumberTag._
  implicit final val BigDecimalTag = new LargeTag[BigDecimal](Approximate, BigDecimal(0))
}
