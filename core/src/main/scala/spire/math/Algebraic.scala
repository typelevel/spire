/**
 * The `Algebraic` number type's goal is to create a guaranteed accuracy number [1].
 * That is, a `Algebraic` can always be approximated to any given accuracy and, in
 * addition, you are guaranteed that if a `Algebraic` `a` represents a real number
 * `r`, then `a.sign == r.sign`.
 *
 * Central to this is the idea of a zero-bound function; this is a function
 * `lowerBound` s.t. if `r != 0` then `r > r.lowerBound`. Here we use the 
 * BFMSS bound [2], though it seems other (C++) libraries use the max of the
 * BFMSS bound and Li/Yap bound [3].
 *
 * [1] "On Guaranteed Accuracy Computation." C. K. Yap.
 *   http://www.cs.nyu.edu/exact/doc/guaranteed.pdf
 * [2] "A Separation Bound for Real Algebraic Expressions." C. Burnikel, et al.
 *   http://stubber.math-inf.uni-greifswald.de/informatik/PEOPLE/Papers/ESA01/sepbound01.pd
 * [3] "A New Constructive Root Bound for Algebraic Expressions." C. Li and C. Yap.
 */
package spire.math

import spire.algebra.{Eq, EuclideanRing, Field, IsReal, NRoot, Order, Ring, Sign, Signed}
import spire.algebra.Sign.{ Positive, Negative, Zero }
import java.math.{ MathContext, BigInteger, BigDecimal => BigDec }
import scala.math.{ ScalaNumber, ScalaNumericConversions }


import java.util.concurrent.AtomicInteger

package algebraic {

  final class BitBound(bitBound: Int) extends AnyVal {
    import BitBound.bitsToDecimalDigits

    /**
     * Returns the bit bound as a BigDecimal.
     */
    def bound: BigDecimal = ???

    /**
     * Returns the minimum number of absolute decimal digits required to
     * represent this separation bound.
     */
    def decimalDigits: Int = bitsToDecimalDigits(bitBound)
  }

  object BitBound {
    private val Epsilon: Double = 2.220446049250313E-16

    private val FudgeFactor: Double = 1D + 4D * Epsilon

    private val lg2ToLg10: Double = log(2, 10) * FudgeFactor

    private def bitsToDecimalDigits(n: Int): Int =
      ceil(n.toDouble * lg2ToLg10).toInt

    implicit val BitBoundAlgebra: Ring[BitBound] = new Ring[BitBound] {
    }
  }

  sealed trait ZeroBoundFunction[S] {

    /**
     * Returns a separation bound as an int `b`, where the bound is `2^-b`.
     */
    def bound(expr: Algebraic.Expr): BitBound

    def getBound(s: S): BitBound

    def integer(n: Long): S = integer(BigInt(n))

    def integer(n: BigInt): S

    def rational(n: Double): S = rational(BigDecimal(n))

    def rational(n: BigDecimal): S = {
      if (n.scale <= 0) integer(n.toBigInt)
      else Rational(n.unscaledValue, ??? n.scale)
    }

    def rational(n: Rational): S =
      div(integer(n.numerator), integer(n.denominator))

    def add(lhs: S, rhs: S): S

    def sub(lhs: S, rhs: S): S

    def mul(lhs: S, rhs: S): S

    def div(lhs: S, rhs: S): S

    def nroot(sub: S, k: Int): S

    def pow(sub: S, k: Int): S
  }

  final case class BMFSSParams(l: Double, u: Double, weight: Int)

  case object BMFSSBound extends ZeroBoundFunction[BMFSSBoundState] {
    def add(lhs: BMFSSParams, rhs: BMFSSParams): BMFSSParams =
      BMFSSParams(
        lhs.l * rhs.l,
        lhs.u * rhs.l + lhs.l * rhs.u,
        lhs.weight * rhs.weight
      )

    def sub(lhs: BMFSSParams, rhs: BMFSSParams): BMFSSParams =
      add(lhs, rhs)

    def mul(lhs: BMFSSParams, rhs: BMFSSParams): BMFSSParams =
      BMFSSParams(
        lhs.l * rhs.l,
        lhs.u * rhs.u,
        lhs.weight * rhs.weight
      )

    def div(lhs: BMFSSParams, rhs: BMFSSParams): BMFSSParams =
      BMFSSParams(
        lhs.l * rhs.u,
        lhs.u * rhs.l,
        lhs.weight * rhs.weight
      )

    def nroot(sub: BMFSSParams, k: Int): BMFSSParams =
      if (sub.u < sub.l) {
        BMFSSParams(
          (sub.l * sub.u.pow(k - 1)).nroot(k),
          sub.u,
          k * sub.weight
        )
      } else {
        BMFSSParams(
          sub.l,
          (sub.u * sub.l.pow(k - 1)).nroot(k),
          k * sub.weight
        )
      }
  }

  case object LiYapBound extends ZeroBoundFunction {
  }
}

import algebraic._

final class Algebraic(expr: Expr) {
  import Algebraic.Expr

  /**
   * Absolute approximation to scale decimal places.
   */
  def toBigDecimal(scale: Int): BigDecimal = expr.toBigDecimal(scale)

  /**
   * Returns an `Int` with the same sign as this algebraic number. Algebraic
   * numbers support exact sign tests, so this is guaranteed to be accurate.
   */
  def signum: Int = expr.signum

  def unary_- : Algebraic =
    new Algebraic(Expr.Neg(expr))

  def +(that: Algebraic): Algebraic =
    new Algebraic(Expr.Add(this.expr, that.expr))

  def -(that: Algebraic): Algebraic =
    new Algebraic(Expr.Sub(this.expr, that.expr))

  def *(that: Algebraic): Algebraic =
    new Algebraic(Expr.Mul(this.expr, that.expr))

  def /(that: Algebraic): Algebraic =
    new Algebraic(Expr.Div(this.expr, that.expr))

  def nroot(k: Int): Algebraic = if (k < 0) {
    new Algebraic(Expr.Div(Expr.ConstantLong(1), Expr.KRoot(this, -k)))
  } else if (k > 0) {
    new Algebraic(Expr.KRoot(this, k))
  } else {
    throw new ArithmeticException("divide by zero (0-root)")
  }

  def pow(k: Int): Algebraic =
    if (k == Int.MinValue) {
      throw new ArithmeticException("illegal exponent (${Int.MinValue})")
    } else if (k == 0) {
      if (signum == 0) {
        throw new ArithmeticException("undeterminate result (0^0)")
      } else {
        One
      }
    } else if (k == 1) {
      this
    } else if (k < 0) {
      new Algebraic(Expr.Div(Expr.ConstantLong(1), this.pow(-k)))
    } else {
      new Algebraic(Expr.Pow(this, k))
    }
}

object Algebraic {
  val Zero: Algebraic = new Algebraic(Expr.ConstantLong(0))
  val One: Algebraic = new Algebraic(Expr.ConstantLong(1))

  sealed abstract class Expr {
    private val zeroBoundFunctionRef: AtomicReference[ZeroBoundFunction] = new AtomicReference[ZeroBoundFunction]()
    lazy val separationBound: BitBound = zeroBoundFunctionRef.get.apply(this)

    def toBigDecimal(digits: Int): JBigDecimal
    def upperBound: BitBound
    def signum: Int
  }

  object Expr {
    private def mc(digits: Int): MathContext = new MathContext(digits, RoundingMode.DOWN)

    sealed abstract class Constant extends Expr

    case class ConstantLong(n: Long) extends Constant {
      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        new JBigDecimal(n)
    }

    case class ConstantDouble(n: Double) extends Constant {
      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        new JBigDecimal(n)
    }

    case class ConstantBigDecimal(n: BigDecimal) extends Constant {
      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        n.bigDecimal
    }

    case class ConstantRational(n: Rational) extends Constant {
      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal = {
        val num = new JBigDecimal(n.numerator.bigInteger)
        val den = new JBigDecimal(n.denominator.bigInteger)
        num.divide(den, digits, RoundingMode.DOWN)
      }
    }

    case class Neg(sub: Expr) extends Expr {
      def upperBound: Int = sub.upperBound
      def signum: Int = -sub.signum
      def toBigDecimal(digits: Int): JBigDecimal =
        sub.toBigDecimal(digits).negate()
    }

    sealed abstract class AddOrSubExpr extends Expr {
      def upperBound: Int =
        max(lhs.upperBound, rhs.upperBound) + 1

      def signum: Int = {
        val maxDigits = separationBound.decimalDigits + 1

        // An adaptive algorithm to find the sign. Rather than just compute
        // this number to `maxDigits` precision, we start with a smaller
        // precision and keep adding digits until we get one that isn't 0.
        @tailrec def loop(digits0: Int): Int = {
          val digits = min(digits0, maxDigits)
          val approx = toBigDecimal(digits + 1).round(digits, RoundingMode.DOWN)
          if (approx != 0 || digits >= maxDigits) {
            approx.signum
          } else {
            loop(2 * scale)
          }
        }

        findSign(4)
      }

      def toBigDecimal(digits: Int): JBigDecimal = {
        val lValue = lhs.toBigDecimal(digits + 1)
        val rValue = rhs.toBigDecimal(digits + 1)
        val sum = this match {
          case (_: Add) => lValue.add(rValue)
          case (_: Sub) => lValue.subtract(rValue)
        }
        sum.round(digits, RoundingMode.DOWN)
      }
    }

    case class Add(lhs: Expr, rhs: Expr) extends AddOrSubExpr

    case class Sub(lhs: Expr, rhs: Expr) extends AddOrSubExpr

    case class Mul(lhs: Expr, rhs: Expr) extends Expr {
      def upperBound: Int = lhs.upperBound + rhs.upperBound
      def signum: Int = lhs.signum * rhs.signum
      def toBigDecimal(digits: Int): JBigDecimal = {
        val lValue = lhs.toBigDecimal(digits + 1 + rhs.upperBound.decimalDigits)
        val rValue = rhs.toBigDecimal(digits + 1 + lhs.upperBound.decimalDigits)
        lValue.times(rValue).round(digits, RoundingMode.DOWN)
      }
    }

    case class Div(lhs: Expr, rhs: Expr) extends Expr {
      def upperBound: Int = lhs.upperBound - rhs.separationBound
      def signum: Int = if (rhs.signum == 0) {
        throw new ArithmeticException("divide by 0")
      } else {
        lhs.signum * rhs.signum
      }
      def toBigDecimal(digits: Int): JBigDecimal = {
        val lValue = lhs.toBigDecimal(digits + 2 - rhs.separationBound)
        val rDigits = max(
          1 - rhs.separationBound,
          digits + 4 - 2 * rhs.separationBound + lhs.upperBound
        )
        val rValue = rhs.toBigDecimal(rDigits)
        val quotient = lValue.divide(rValue, digits + 1, RoundingMode.DOWN)
        quotient.round(digits, RoundingMode.DOWN)
      }
    }

    case class KRoot(sub: Expr, k: Int) extends Expr {
      def upperBound: Int = (sub.upperBound + 1) / 2

      def signum: Int = {
        val s = sub.signum
        if (s >= 0) s
        else throw new ArithmeticException(s"$k-root of negative number")
      }

      def toBigDecimal(digits: Int): JBigDecimal = {
        val digits0 = max(digits + 1, 1 - (sub.separationBound + 1) / 2)
        val value = sub.toBigDecimal(digits0)
        val n = new JBigDecimal(k)
        @tailrec def loop(prev: JBigDecimal): JBigDecimal = {
          val prevExp = prev.pow(k - 1)
          val delta = value
            .divide(prev, digits + 1, RoundingMode.HALF_UP)
            .subtract(prev)
            .divide(n, digits + 1, RoundingMode.HALF_UP)
          if (delta == JBigDecimal.ZERO) prev
          else prev.add(delta)
        }
        loop(value).round(digits, RoundingMode.DOWN)
      }
    }

    case class Pow(sub: Expr, k: Int) extends Expr {
      require(k > 1)

      def upperBound: Int = k * sub.upperBound
      def signum: Int = {
        val s = sub.signum
        if (s == 0) {
          if (k < 0) throw new ArithmeticException("divide by 0")
          else if (k == 0) throw new ArithmeticException("indeterminate")
          else 0
        } else if (k % 2 == 0) {
          s.abs
        } else {
          s
        }
      }
      def toBigDecimal(digits: Int): JBigDecimal = {
        // We could possibly do better here. Investigate.
        val height = 32 - java.lang.Integer.numberOfLeadingZeros(k - 1) // ceil(lg2(k))
        val maxDigits = digits + height * (1 + sup.upperBound.decimalDigits)
        val leafValue = sub.toBigDecimal(maxDigits)
        leafValue.pow(k)
      }
    }
  }
}

/**
 * An general Algebraic type. Can be used represent real numbers and approximate
 * them on-demand.
 */
@SerialVersionUID(0L)
final class Algebraic private (val expr: Expr[Algebraic])
extends ScalaNumber with ScalaNumericConversions
   with RealLike[Algebraic]
   with BMFSSBound[Algebraic]
   with BigDecimalApprox[Algebraic]
   with FPFilter[Algebraic]
   with ConstantFolder[Algebraic]
   with BubbleUpDivs[Algebraic]
   with PrettyToString[Algebraic]
   with Ordered[Algebraic]
   with Serializable {

  val coexpr: Coexpr[Algebraic] = Algebraic.RealCoexpr

  // ugh
  override def byteValue(): Byte = fpf.toLong map (_.toByte) getOrElse super.toByte
  override def shortValue(): Short = fpf.toLong map (_.toShort) getOrElse super.toShort

  override def equals(that: Any) = that match {
    case that: Algebraic => (this - that).isZero
    case that: Real => (this - Algebraic(that.toRational)).isZero
    case that: Number => (this - Algebraic(that.toRational)).isZero
    case that: Rational => (this - Algebraic(that)).isZero
    case that: BigInt => isWhole && toBigInt == that
    case that: Natural => isWhole && signum >= 0 && that == toBigInt
    case that: SafeLong => isWhole && that == this
    case that: Complex[_] => that == this
    case that: Quaternion[_] => that == this
    case that: BigDecimal => try {
      toBigDecimal(that.mc) == that
    } catch {
      case ae: ArithmeticException => false
    }
    case _ => unifiedPrimitiveEquals(that)
  }

  override def hashCode: Int = if (isWhole && toBigInt == toLong) {
    unifiedPrimitiveHashcode
  } else {
    val x = toBigDecimal(java.math.MathContext.DECIMAL64)
    x.underlying.unscaledValue.hashCode + 23 * x.scale.hashCode + 17
  }
}


object Algebraic extends AlgebraicInstances {

  implicit def apply(n: Int): Algebraic = Expr(n)
  implicit def apply(n: Long): Algebraic = Expr(n)
  implicit def apply(n: BigInt): Algebraic = Expr(n)
  implicit def apply(n: Rational): Algebraic = Expr(n)
  implicit def apply(n: Double): Algebraic = Expr(n)
  implicit def apply(n: BigDecimal): Algebraic = Expr(n)

  implicit object RealCoexpr extends Coexpr[Algebraic] {
    def expr(r: Algebraic): Expr[Algebraic] = r.expr
    def coexpr(e: Expr[Algebraic]): Algebraic = new Algebraic(e)
  }
}

trait AlgebraicInstances {
  implicit final val AlgebraicAlgebra = new AlgebraicAlgebra
  import NumberTag._
  implicit final val AlgebraicTag = new LargeTag[Algebraic](Exact, Algebraic(0))
}

private[math] trait AlgebraicIsRing extends Ring[Algebraic] {
  override def minus(a: Algebraic, b: Algebraic): Algebraic = a - b
  def negate(a: Algebraic): Algebraic = -a
  def one: Algebraic = Algebraic(1)
  def plus(a: Algebraic, b: Algebraic): Algebraic = a + b
  override def pow(a: Algebraic, b: Int): Algebraic = a pow b
  override def times(a: Algebraic, b: Algebraic): Algebraic = a * b
  def zero: Algebraic = Algebraic(0)
  
  override def fromInt(n: Int): Algebraic = Algebraic(n)
}

private[math] trait AlgebraicIsEuclideanRing extends EuclideanRing[Algebraic] with AlgebraicIsRing {
  def quot(a: Algebraic, b: Algebraic): Algebraic = a /~ b
  def mod(a: Algebraic, b: Algebraic): Algebraic = a % b
  def gcd(a: Algebraic, b: Algebraic): Algebraic = euclid(a, b)(Eq[Algebraic])
}

private[math] trait AlgebraicIsField extends Field[Algebraic] with AlgebraicIsEuclideanRing {
  override def fromDouble(n: Double): Algebraic = Algebraic(n)
  def div(a:Algebraic, b:Algebraic) = a / b
}

private[math] trait AlgebraicIsNRoot extends NRoot[Algebraic] {
  def nroot(a: Algebraic, k: Int): Algebraic = a nroot k
  def fpow(a:Algebraic, b:Algebraic) = sys.error("fixme")
}

private[math] trait AlgebraicOrder extends Order[Algebraic] {
  override def eqv(x: Algebraic, y: Algebraic) = (x - y).isZero
  override def neqv(x: Algebraic, y: Algebraic) = (x - y).isNonZero
  def compare(x: Algebraic, y: Algebraic) = (x - y).signum
}

private[math] trait AlgebraicIsSigned extends Signed[Algebraic] {
  override def sign(a: Algebraic): Sign = a.sign
  def signum(a: Algebraic): Int = a.signum
  def abs(a: Algebraic): Algebraic = a.abs
}

private[math] trait AlgebraicIsReal extends IsReal[Algebraic] with AlgebraicOrder with AlgebraicIsSigned {
  def toDouble(x: Algebraic): Double = x.toDouble
  def ceil(a:Algebraic) = if (a % 1 == 0) a else a + 1 - (a % 1)
  def floor(a:Algebraic) = a - (a % 1)
  def round(a:Algebraic) = {
    val m = a % 1
    if (m < 0.5) a - m else a + 1 - m
  }
  def isWhole(a:Algebraic) = a % 1 == 0
}

@SerialVersionUID(0L)
class AlgebraicAlgebra extends AlgebraicIsField with AlgebraicIsNRoot with AlgebraicIsReal with Serializable
