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

import java.lang.Long.numberOfLeadingZeros
import java.lang.Double.{ isInfinite, isNaN }
import java.math.{ MathContext, RoundingMode, BigInteger, BigDecimal => JBigDecimal }

import scala.annotation.tailrec
import scala.math.{ ScalaNumber, ScalaNumericConversions }
import scala.collection.concurrent.TrieMap

import spire.algebra.{Eq, EuclideanRing, Field, IsReal, NRoot, Order, Ring, Sign, Signed}
import spire.algebra.Sign.{ Positive, Negative, Zero }
import spire.macros.Checked.checked
import spire.syntax.order._

@SerialVersionUID(1L)
final class Algebraic(val expr: Algebraic.Expr)
extends ScalaNumber with ScalaNumericConversions with Serializable {
  import Algebraic.{ Zero, One, Expr, MinIntValue, MaxIntValue, MinLongValue, MaxLongValue, JBigDecimalOrder, roundExact }

  /**
   * Returns an `Int` with the same sign as this algebraic number. Algebraic
   * numbers support exact sign tests, so this is guaranteed to be accurate.
   */
  def signum: Int = expr.signum

  /**
   * Returns the sign of this Algebraic number. Algebraic numbers support exact
   * sign tests, so this is guaranteed to be accurate.
   */
  def sign: Sign = Sign(signum)

  def abs: Algebraic =
    if (this.signum < 0) -this else this

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

  def /~(that: Algebraic): Algebraic =
    Algebraic((this / that).toBigInt)

  def %(that: Algebraic): Algebraic =
    this - (this /~ that) * that

  def sqrt: Algebraic = nroot(2)

  def cbrt: Algebraic = nroot(3)

  def nroot(k: Int): Algebraic = if (k < 0) {
    new Algebraic(Expr.Div(Expr.ConstantLong(1), Expr.KRoot(this.expr, -k)))
  } else if (k > 0) {
    new Algebraic(Expr.KRoot(this.expr, k))
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
      new Algebraic(Expr.Div(Expr.ConstantLong(1), this.pow(-k).expr))
    } else {
      new Algebraic(Expr.Pow(this.expr, k))
    }

  def <  (that: Algebraic): Boolean = compare(that) <  0
  def >  (that: Algebraic): Boolean = compare(that) >  0
  def <= (that: Algebraic): Boolean = compare(that) <= 0
  def >= (that: Algebraic): Boolean = compare(that) >= 0

  def compare(that: Algebraic): Int = (this - that).signum

  def isZero: Boolean = signum == 0

  override def equals(that: Any) = that match {
    case (that: Algebraic) => this.compare(that) == 0
    case (that: Real) => this.toReal == that
    case (that: Number) => this.compare(Algebraic(that.toBigDecimal)) == 0
    case (that: Rational) => this.compare(Algebraic(that)) == 0
    case (that: BigInt) => isWhole && toBigInt == that
    case (that: Natural) => isWhole && signum >= 0 && that == toBigInt
    case (that: SafeLong) => isWhole && that == this
    case (that: Complex[_]) => that == this
    case (that: Quaternion[_]) => that == this
    case (that: BigDecimal) => try {
      toBigDecimal(that.mc) == that
    } catch {
      case ae: ArithmeticException => false
    }
    case _ => unifiedPrimitiveEquals(that)
  }

  override def hashCode: Int = if (isWhole && isValidLong) {
    unifiedPrimitiveHashcode
  } else {
    val x = toBigDecimal(java.math.MathContext.DECIMAL64)
    x.underlying.unscaledValue.hashCode + 23 * x.scale.hashCode + 17
  }

  /**
   * Returns the nearest, valid `Int` value to this Algebraic, without going
   * further away from 0 (eg. truncation).
   *
   * If this `Algebraic` represented 1.2, then this would return 1. If this
   * represented -3.3, then this would return -3. If this value is greater than
   * `Int.MaxValue`, then `Int.MaxValue` is returned. If this value is less
   * than `Int.MinValue`, then `Int.MinValue` is returned.
   */
  def intValue: Int = {
    val n = toBigInt
    if (n < MinIntValue) Int.MinValue
    else if (n > MaxIntValue) Int.MaxValue
    else n.intValue
  }

  /**
   * Returns the nearest, valid `Long` value to this Algebraic, without going
   * further away from 0 (eg. truncation).
   *
   * If this `Algebraic` represented 1.2, then this would return 1. If this
   * represented -3.3, then this would return -3. If this value is greater than
   * `Long.MaxValue`, then `Long.MaxValue` is returned. If this value is less
   * than `Long.MinValue`, then `Long.MinValue` is returned.
   */
  def longValue: Long = {
    val n = toBigInt
    if (n < MinLongValue) Long.MinValue
    else if (n > MaxLongValue) Long.MaxValue
    else n.longValue
  }

  def floatValue: Float = ???
  def doubleValue: Double = ???

  /**
   * Returns the nearest, valid `BigInt` value to this Algebraic, without going
   * further away from 0 (eg. truncation).
   *
   * If this `Algebraic` represented 1.2, then this would return 1. If this
   * represented -3.3, then this would return -3.
   */
  def toBigInt: BigInt =
    toBigDecimal(0, RoundingMode.DOWN).toBigInt

  /**
   * Absolute approximation to scale decimal places.
   */
  def toBigDecimal(scale: Int, roundingMode: RoundingMode): BigDecimal =
    BigDecimal(roundExact(this, expr.toBigDecimal(scale + 2), scale, roundingMode))

  def toBigDecimal(mc: MathContext): BigDecimal = {
    import Expr._

    val roundingMode = mc.getRoundingMode

    def rec(e: Expr, digits: Int): JBigDecimal = e match {
      case ConstantLong(n) =>
        new JBigDecimal(n, new MathContext(digits, roundingMode))
      case ConstantDouble(n) =>
        new JBigDecimal(n, new MathContext(digits, roundingMode))
      case ConstantBigDecimal(n) =>
        n.bigDecimal.round(new MathContext(digits, roundingMode))
      case ConstantRational(n) =>
        val num = new JBigDecimal(n.numerator.bigInteger)
        val den = new JBigDecimal(n.denominator.bigInteger)
        num.divide(den, new MathContext(digits, roundingMode))
      case Add(_, _) | Sub(_, _) if e.signum == 0 =>
        JBigDecimal.ZERO
      case Add(lhs, rhs) =>
        val digits0 = digits + e.separationBound.decimalDigits.toInt + 1
        val lValue = rec(lhs, digits0)
        val rValue = rec(rhs, digits0)
        lValue.add(rValue, new MathContext(digits, roundingMode))
      case Sub(lhs, rhs) =>
        val digits0 = digits + e.separationBound.decimalDigits.toInt + 1
        val lValue = rec(lhs, digits0)
        val rValue = rec(rhs, digits0)
        lValue.subtract(rValue, new MathContext(digits, roundingMode))
      case Mul(lhs, rhs) =>
        val lValue = rec(lhs, digits + 1)
        val rValue = rec(rhs, digits + 2)
        lValue.multiply(rValue, new MathContext(digits, roundingMode))
      case Div(lhs, rhs) =>
        val rValue = rec(rhs, digits + 2)
        if (rValue.compareTo(JBigDecimal.ZERO) == 0)
          throw new ArithmeticException("divide by zero")
        val lValue = rec(lhs, digits + 2)
        lValue
          .divide(rValue, new MathContext(digits + 2, roundingMode))
          .round(new MathContext(digits, roundingMode))
      case KRoot(sub, k) =>
        Algebraic.nroot(rec(sub, digits + 2), k, new MathContext(digits + 2, roundingMode))
          .round(new MathContext(digits, roundingMode))
      case Pow(sub, k) =>
        val subValue = rec(sub, digits + ceil(log(k.toDouble)).toInt)
        subValue.pow(digits, new MathContext(digits, roundingMode))
    }
    val approx = rec(expr, mc.getPrecision + 2)
    val newScale = approx.scale - approx.precision + mc.getPrecision
    val adjustedApprox =
      if (newScale <= approx.scale) approx.setScale(newScale + 1)
      else approx
    roundExact(this, adjustedApprox, newScale, roundingMode)
      .round(mc) // We perform a final round, since roundExact uses scales.
  }

  def isWhole: Boolean = this == Algebraic(toBigInt)

  /**
   * Returns `true` if this Algebraic number is a whole number (no fractional
   * part) and fits within the bounds of an `Int`. That is, if `x.isValidInt`,
   * then `Algebraic(x.toInt) == x`.
   */
  override def isValidInt: Boolean = {
    val n = toBigInt
    (n <= MaxIntValue) &&
    (n >= MinIntValue) &&
    (this == Algebraic(n))
  }

  /**
   * Returns `true` if this Algebraic number is a whole number (no fractional
   * part) and fits within the bounds of an `Long`. That is, if `x.isValidLong`,
   * then `Algebraic(x.toLong) == x`.
   */
  def isValidLong: Boolean = {
    val n = toBigInt
    (n <= MaxLongValue) &&
    (n >= MinLongValue) &&
    (this == Algebraic(n))
  }

  /**
   * Returns `true` iff this is a rational expression (ie contains no n-root
   * expressions). Otherwise it is a radical expression and returns false.
   */
  def isRational: Boolean = expr.flags.isRational

  /**
   * If this is a rational expressions, then it returns the exact value as a
   * [[Rational]]. Otherwise, this is a radical expression and `None` is
   * returned.
   */
  def rational: Option[Rational] =
    if (expr.flags.isRational) {
      implicit val nroot: NRoot[Rational] = new NRoot[Rational] {
        def nroot(a: Rational, n: Int): Rational = ???
        def fpow(a:Rational, b:Rational): Rational = ???
      }
      Some(evaluateWith[Rational])
    } else {
      None
    }

  /**
   * Evaluates this algebraic expression with a different number type. All
   * `Algebraic` numbers store the entire expression tree, so we can use this
   * to *replay* the stored expression using a different type. This will
   * accumulate errors as if the number type had been used from the beginning
   * and is only really suitable for more exact number types, like [[Real]].
   */
  def evaluateWith[A: Field: NRoot](implicit conv: ConvertableTo[A]): A = {
    import spire.syntax.field._
    import spire.syntax.nroot._
    import Expr._

    def eval(e: Expr): A = e match {
      case ConstantLong(n) => conv.fromLong(n)
      case ConstantDouble(n) => conv.fromDouble(n)
      case ConstantBigDecimal(n) => conv.fromBigDecimal(n)
      case ConstantRational(n) => conv.fromRational(n)
      case Neg(n) => -eval(n)
      case Add(a, b) => eval(a) + eval(b)
      case Sub(a, b) => eval(a) - eval(b)
      case Mul(a, b) => eval(a) * eval(b)
      case Div(a, b) => eval(a) / eval(b)
      case KRoot(a, k) => eval(a).nroot(k)
      case Pow(a, k) => eval(a).pow(k)
    }

    eval(expr)
  }

  /**
   * Returns an exact [[Real]] representation of this number.
   */
  def toReal: Real = evaluateWith[Real]

  // ScalaNumber. Because of course all Scala numbers are wrappers.
  def underlying: AnyRef = this
}

object Algebraic extends AlgebraicInstances {
  private val MaxIntValue: BigInteger = BigInteger.valueOf(Int.MaxValue.toLong)
  private val MinIntValue: BigInteger = BigInteger.valueOf(Int.MinValue.toLong)
  private val MaxLongValue: BigInteger = BigInteger.valueOf(Long.MaxValue)
  private val MinLongValue: BigInteger = BigInteger.valueOf(Long.MinValue)

  val Zero: Algebraic = new Algebraic(Expr.ConstantLong(0))
  val One: Algebraic = new Algebraic(Expr.ConstantLong(1))

  def apply(n: Int): Algebraic =
    new Algebraic(Expr.ConstantLong(n))

  def apply(n: Long): Algebraic =
    new Algebraic(Expr.ConstantLong(n))

  def apply(n: Float): Algebraic =
    Algebraic(n.toDouble)

  def apply(n: Double): Algebraic =
    if (java.lang.Double.isInfinite(n)) {
      throw new IllegalArgumentException("cannot construct inifinite Algebraic")
    } else if (java.lang.Double.isNaN(n)) {
      throw new IllegalArgumentException("cannot construct Algebraic from NaN")
    } else {
      new Algebraic(Expr.ConstantDouble(n))
    }

  def apply(n: BigInt): Algebraic =
    new Algebraic(Expr.ConstantBigDecimal(BigDecimal(n)))

  def apply(n: BigDecimal): Algebraic =
    new Algebraic(Expr.ConstantBigDecimal(n))

  def apply(n: Rational): Algebraic =
    new Algebraic(Expr.ConstantRational(n))

  def apply(n: String): Algebraic =
    Algebraic(BigDecimal(new JBigDecimal(n)))

  sealed abstract class Expr {
    import Expr._

    def flags: Flags

    private val bounds: TrieMap[ZeroBoundFunction, Any] =
      new TrieMap

    /**
     * Returns the bound for `zbf`, using a cached value if it is available.
     */
    def getBound(zbf: ZeroBoundFunction): zbf.Bound =
      bounds.getOrElseUpdate(zbf, {
        this match {
          case ConstantLong(n) => zbf.integer(n)
          case ConstantDouble(n) => zbf.rational(n)
          case ConstantBigDecimal(n) => zbf.rational(n)
          case ConstantRational(n) => zbf.rational(n)
          case Neg(sub) => zbf.negate(sub.getBound(zbf))
          case Add(lhs, rhs) => zbf.add(lhs.getBound(zbf), rhs.getBound(zbf))
          case Sub(lhs, rhs) => zbf.sub(lhs.getBound(zbf), rhs.getBound(zbf))
          case Mul(lhs, rhs) => zbf.mul(lhs.getBound(zbf), rhs.getBound(zbf))
          case Div(lhs, rhs) => zbf.div(lhs.getBound(zbf), rhs.getBound(zbf))
          case KRoot(sub, k) => zbf.nroot(sub.getBound(zbf), k)
          case Pow(sub, k) => zbf.pow(sub.getBound(zbf), k)
        }
      }).asInstanceOf[zbf.Bound]

    lazy val degreeBound: Long = this match {
      case KRoot(sub, k) =>
        checked(sub.degreeBound * k)
      case _ =>
        children
          .map(_.degreeBound)
          .foldLeft(1L) { (a, b) =>
            checked(a * b)
          }
    }

    def bfmssBound: BitBound =
      getBound(bfmss).getBitBound(degreeBound)

    def separationBound: BitBound = bfmssBound

    def toBigDecimal(digits: Int): JBigDecimal
    def upperBound: BitBound
    def signum: Int

    def children: List[Expr]
  }

  object Expr {
    private def mc(digits: Int): MathContext = new MathContext(digits, RoundingMode.DOWN)

    final class Flags(val bits: Int) extends AnyVal {
      import Flags._

      def | (that: Flags): Flags = new Flags(bits | that.bits)

      def check(n: Int): Boolean = (bits & n) != 0

      def isRational: Boolean = !isRadical
      def isRadical: Boolean = check(RadicalFlag)
      def hasDoubleLeaf: Boolean = check(HasDoubleLeaf)
      def hasBigDecimalLeaf: Boolean = check(HasBigDecimalLeaf)
      def hasRationalLeaf: Boolean = check(HasRationalLeaf)
    }

    object Flags {
      final val RadicalFlag = 1
      final val HasDoubleLeaf = 2
      final val HasBigDecimalLeaf = 4
      final val HasRationalLeaf = 8

      final val IntegerLeaf: Flags = new Flags(0)
      final val DoubleLeaf: Flags = new Flags(HasDoubleLeaf)
      final val BigDecimalLeaf: Flags = new Flags(HasBigDecimalLeaf)
      final val RationalLeaf: Flags = new Flags(HasRationalLeaf)
      final val IsRadical: Flags = new Flags(RadicalFlag)
    }

    sealed abstract class Constant extends Expr {
      def children: List[Expr] = Nil
    }

    sealed abstract class UnaryExpr extends Expr {
      val sub: Expr
      def children: List[Expr] = sub :: Nil
    }

    sealed abstract class BinaryExpr extends Expr {
      val lhs: Expr
      val rhs: Expr
      val flags: Flags = lhs.flags | rhs.flags
      def children: List[Expr] = lhs :: rhs :: Nil
    }

    case class ConstantLong(n: Long) extends Constant {
      def flags: Flags = Flags.IntegerLeaf

      def upperBound: BitBound =
        if (n == 0L) new BitBound(0L)
        else if (n == Long.MinValue) new BitBound(64)
        else new BitBound(64 - numberOfLeadingZeros(abs(n) - 1))

      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        new JBigDecimal(n).setScale(digits, RoundingMode.HALF_UP)
    }

    case class ConstantDouble(n: Double) extends Constant {
      def flags: Flags = Flags.DoubleLeaf

      def upperBound: BitBound = new BitBound(ceil(log(n)).toLong)

      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        new JBigDecimal(n).setScale(digits, RoundingMode.HALF_UP)
    }

    case class ConstantBigDecimal(n: BigDecimal) extends Constant {
      def flags: Flags = Flags.BigDecimalLeaf

      def upperBound: BitBound = new BitBound(ceil(log(n)).toLong)

      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        n.bigDecimal.setScale(digits, RoundingMode.HALF_UP)
    }

    case class ConstantRational(n: Rational) extends Constant {
      def flags: Flags = Flags.RationalLeaf

      def upperBound: BitBound =
        new BitBound(n.numerator.abs.bitLength - n.denominator.bitLength + 1)

      def signum: Int = n.signum

      def toBigDecimal(digits: Int): JBigDecimal = {
        val num = new JBigDecimal(n.numerator.bigInteger)
        val den = new JBigDecimal(n.denominator.bigInteger)
        num.divide(den, digits, RoundingMode.DOWN)
      }
    }

    case class Neg(sub: Expr) extends UnaryExpr {
      def flags: Flags = sub.flags
      def upperBound: BitBound = sub.upperBound
      def signum: Int = -sub.signum
      def toBigDecimal(digits: Int): JBigDecimal =
        sub.toBigDecimal(digits).negate()
    }

    sealed abstract class AddOrSubExpr extends BinaryExpr {
      def upperBound: BitBound =
        new BitBound(max(lhs.upperBound.bitBound, rhs.upperBound.bitBound) + 1)

      def signum: Int = {
        val maxDigits = separationBound.decimalDigits + 1
        val approxOnly = maxDigits > Int.MaxValue

        // An adaptive algorithm to find the sign. Rather than just compute
        // this number to `maxDigits` precision, we start with a smaller
        // precision and keep adding digits until we get one that isn't 0.
        @tailrec def loop(digits0: Long): Int = {
          val digits = min(digits0, min(maxDigits, Int.MaxValue)).toInt
          val approx = toBigDecimal(digits + 1).setScale(digits, RoundingMode.DOWN)
          if (approx.signum != 0 || digits >= maxDigits) {
            approx.signum
          } else if (digits == Int.MaxValue) {
            throw new ArithmeticException("required precision to calculate sign is too high")
          } else {
            loop(2 * digits0)
          }
        }

        loop(4)
      }

      def toBigDecimal(digits: Int): JBigDecimal = {
        val lValue = lhs.toBigDecimal(digits + 1)
        val rValue = rhs.toBigDecimal(digits + 1)
        val sum = this match {
          case (_: Add) => lValue.add(rValue)
          case (_: Sub) => lValue.subtract(rValue)
        }
        val result = sum.setScale(digits, RoundingMode.DOWN)
        result
      }
    }

    case class Add(lhs: Expr, rhs: Expr) extends AddOrSubExpr

    case class Sub(lhs: Expr, rhs: Expr) extends AddOrSubExpr

    case class Mul(lhs: Expr, rhs: Expr) extends BinaryExpr {
      def upperBound: BitBound = lhs.upperBound + rhs.upperBound
      def signum: Int = lhs.signum * rhs.signum
      def toBigDecimal(digits: Int): JBigDecimal = {
        val lDigits = checked(rhs.upperBound.decimalDigits + digits + 1)
        val rDigits = checked(lhs.upperBound.decimalDigits + digits + 1)
        if (lDigits >= Int.MaxValue || rDigits >= Int.MaxValue) {
          throw new IllegalArgumentException("required precision is too high")
        } else {
          val lValue = lhs.toBigDecimal(lDigits.toInt)
          val rValue = rhs.toBigDecimal(rDigits.toInt)
          lValue.multiply(rValue).setScale(digits, RoundingMode.DOWN)
        }
      }
    }

    case class Div(lhs: Expr, rhs: Expr) extends BinaryExpr {
      // Chee Yap's paper has lhs.upperBound - rhs.separationBound, but it
      // makes much more sense to have lhs.upperBound + rhs.separationBound,
      // since separationBound is -lg of the lower bound (for a valid div).
      // That is, lg(x/y) <= ub(x) / lb(y).
      def upperBound: BitBound = lhs.upperBound + rhs.separationBound
      def signum: Int = if (rhs.signum == 0) {
        throw new ArithmeticException("divide by 0")
      } else {
        lhs.signum * rhs.signum
      }
      def toBigDecimal(digits: Int): JBigDecimal = checked {
        val lDigits = digits + 2 - rhs.separationBound.decimalDigits
        val rDigits = max(
          1 - rhs.separationBound.decimalDigits,
          digits + 4 - 2 * rhs.separationBound.decimalDigits + lhs.upperBound.decimalDigits
        )
        if (lDigits >= Int.MaxValue || rDigits >= Int.MaxValue) {
          throw new IllegalArgumentException("required precision is too high")
        } else {
          val lValue = lhs.toBigDecimal(lDigits.toInt)
          val rValue = rhs.toBigDecimal(rDigits.toInt)
          val quotient = lValue.divide(rValue, digits + 1, RoundingMode.DOWN)
          quotient.setScale(digits, RoundingMode.DOWN)
        }
      }
    }

    case class KRoot(sub: Expr, k: Int) extends UnaryExpr {
      val flags: Flags = sub.flags | Flags.IsRadical

      def upperBound: BitBound = (sub.upperBound + 1) / 2

      def signum: Int = {
        val s = sub.signum
        if (s >= 0) s
        else throw new ArithmeticException(s"$k-root of negative number")
      }

      def toBigDecimal(digits: Int): JBigDecimal = {
        val digits0 = max(
          checked(digits + 1),
          checked(1 - (sub.separationBound.decimalDigits + 1) / 2)
        )
        if (digits0 >= Int.MaxValue) {
          throw new IllegalArgumentException("required precision is too high")
        } else {
          val value = sub.toBigDecimal(digits0.toInt)
          Algebraic.nroot(value, k, digits, RoundingMode.DOWN)
        }
      }
    }

    case class Pow(sub: Expr, k: Int) extends UnaryExpr {
      require(k > 1)

      def flags: Flags = sub.flags

      def upperBound: BitBound = sub.upperBound * k
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
        val maxDigits = checked(digits + height * (1 + sub.upperBound.decimalDigits))
        if (maxDigits >= Int.MaxValue) {
          throw new IllegalArgumentException("required precision is too high")
        } else {
          val leafValue = sub.toBigDecimal(maxDigits.toInt)
          leafValue.pow(k)
        }
      }
    }
  }

  final class BitBound(val bitBound: Long) extends AnyVal {
    import BitBound.bitsToDecimalDigits

    /**
     * Returns the bit bound as a BigDecimal.
     */
    def bound: BigDecimal = ???

    /**
     * Returns the minimum number of absolute decimal digits required to
     * represent this separation bound.
     */
    def decimalDigits: Long = bitsToDecimalDigits(bitBound)

    def +(that: BitBound): BitBound = new BitBound(this.bitBound + that.bitBound)
    def -(that: BitBound): BitBound = new BitBound(this.bitBound - that.bitBound)
    def *(that: BitBound): BitBound = new BitBound(this.bitBound * that.bitBound)
    def /(that: BitBound): BitBound = new BitBound(this.bitBound / that.bitBound)

    def +(rhs: Int): BitBound = new BitBound(this.bitBound + rhs)
    def -(rhs: Int): BitBound = new BitBound(this.bitBound - rhs)
    def *(rhs: Int): BitBound = new BitBound(this.bitBound * rhs)
    def /(rhs: Int): BitBound = new BitBound(this.bitBound / rhs)
  }

  object BitBound {
    private val Epsilon: Double = 2.220446049250313E-16

    private val FudgeFactor: Double = 1D + 4D * Epsilon

    private val lg2ToLg10: Double = log(2, 10) * FudgeFactor

    private def bitsToDecimalDigits(n: Long): Long =
      ceil(n.toDouble * lg2ToLg10).toLong

    final def apply(n: Int): BitBound = new BitBound(n)
  }

  /**
   * A zero bound function, defined over an algebraic expression algebra.
   */
  sealed trait ZeroBoundFunction {
    type Bound

    def integer(n: Long): Bound = integer(BigInt(n))

    def integer(n: BigInt): Bound

    def rational(n: Double): Bound =
      rational(BigDecimal(n))

    def rational(n: BigDecimal): Bound =
      rational(Rational(n))

    def rational(n: Rational): Bound =
      div(integer(n.numerator), integer(n.denominator))

    def negate(sub: Bound): Bound

    def add(lhs: Bound, rhs: Bound): Bound

    def sub(lhs: Bound, rhs: Bound): Bound

    def mul(lhs: Bound, rhs: Bound): Bound

    def div(lhs: Bound, rhs: Bound): Bound

    def nroot(sub: Bound, k: Int): Bound

    def pow(sub: Bound, k: Int): Bound
  }

  case object bfmss extends ZeroBoundFunction {

    final case class Bound(l: Long, u: Long) {
      def getBitBound(degreeBound: Long): BitBound =
        new BitBound(l + u * (degreeBound - 1))
    }

    def integer(n: BigInt): Bound =
      Bound(0, n.abs.bitLength + 1)

    def negate(sub: Bound): Bound =
      sub

    // We're not being fair to the BFMSS bound here. We're really just
    // setting a bound on the max value. However, the alternative would
    // require us to work outside of log arithmetic.
    def add(lhs: Bound, rhs: Bound): Bound = checked {
      Bound(
        lhs.l + rhs.l,
        math.max(lhs.u + rhs.l, lhs.l + rhs.u) + 1
      )
    }

    def sub(lhs: Bound, rhs: Bound): Bound =
      add(lhs, rhs)

    def mul(lhs: Bound, rhs: Bound): Bound = checked {
      Bound(
        lhs.l + rhs.l,
        lhs.u + rhs.u
      )
    }

    def div(lhs: Bound, rhs: Bound): Bound = checked {
      Bound(
        lhs.l + rhs.u,
        lhs.u + rhs.l
      )
    }

    def nroot(sub: Bound, k: Int): Bound = checked {
      if (sub.u < sub.l) {
        Bound(
          (sub.l + (k - 1) * sub.u) / k,
          sub.u
        )
      } else {
        Bound(
          sub.l,
          (sub.u * (k - 1) * sub.l) / k
        )
      }
    }

    def pow(sub: Bound, k: Int): Bound = {
      @tailrec def sum(acc: Long, k: Int, extra: Long): Long =
        if (k == 1) {
          checked(acc + extra)
        } else {
          val x =
            if ((k & 1) == 1) checked(acc + extra)
            else extra
          sum(checked(acc + acc), k >>> 1, x)
        }

      if (k > 1) {
        Bound(
          sum(sub.l, k - 1, sub.l),
          sum(sub.u, k - 1, sub.u)
        )
      } else if (k == 1) {
        sub
      } else if (k == 0) {
        throw new IllegalArgumentException("exponent cannot be 0")
      } else {
        throw new IllegalArgumentException("exponent cannot be negative")
      }
    }
  }

  /**
   * Returns a number that is approximately equal to `x.pow(1/n)`. This number
   * is useful as initial values in converging n-root algorithms, but not as a
   * general purpose n-root algorithm. There are no guarantees about the
   * accuracy here.
   */
  final def nrootApprox(x: JBigDecimal, n: Int): JBigDecimal = {
    // Essentially, we'd like to just find `x.doubleValue.pow(1D / n)`, but x
    // may not be approximable as a finite Double (eg. exponent is larger than
    // 308). So, we basically treat x as a number `a*10^(i+j)`, where
    // `a*10^i` is approximable as a Double and `j % n == 0`. Then, we can
    // approximate the n-th root as `pow(a*10^i, 1 / n) * 10^(j/n)`.

    // If n > ~308, then we could end up with an "approximate" value that is
    // an Infinity, which is no good. So, we approximate all roots > 306 with
    // 306-th root.
    val k = min(n, 306)
    // We need to ensure that the scale of our approximate number leaves `j`
    // evenly divible by n. So, we start by calculating the scale requried to
    // put the decimal place after the first digit
    val width = (ceil(x.unscaledValue.bitLength * log(2) / log(10)) - 1).toInt
    // We then add in (x.scale - width) % n to our initial scale so that the
    // remaining exponenent is divisible by n.
    val safeWidth = width + (x.scale - width) % k
    val approx = new JBigDecimal(x.unscaledValue, safeWidth).doubleValue
    new JBigDecimal(pow(approx, 1D / k))
      .scaleByPowerOfTen(-(x.scale - safeWidth) / k)
      .round(MathContext.DECIMAL64)
  }

  /**
   * Approximates the n-th root using the Newton's method. Rather than using a
   * fixed epsilon, it may use an adaptive epsilon, provided by `getEps`. This
   * function takes the previous approximation, and returns the epsilon as
   * `pow(10, -getEps(prev))`. This allows us to use the same algorithm for
   * both absolute and relative precision approximations. Absolute
   * approximations just returns a fixed epsilon from `getEps`, where as a
   * relative approximation returns an adaptive one, that uses the previous
   * value to guide the required epsilon.
   */
  private final def nroot(value: JBigDecimal, k: Int)(getEps: JBigDecimal => Int): JBigDecimal = {
    if (value.compareTo(JBigDecimal.ZERO) == 0)
      return JBigDecimal.ZERO
    val n = new JBigDecimal(k)
    @tailrec def loop(prev: JBigDecimal, prevDigits: Int, prevEps: JBigDecimal): JBigDecimal = {
      val digits = getEps(prev)
      val eps =
        if (digits == prevDigits) prevEps
        else JBigDecimal.ONE.movePointLeft(digits)
      val prevExp = prev.pow(k - 1)
      val delta = value
        .divide(prev, digits, RoundingMode.HALF_UP)
        .subtract(prev)
        .divide(n, digits, RoundingMode.HALF_UP)
      if (delta.abs.compareTo(eps) <= 0) prev
      else loop(prev.add(delta), digits, eps)
    }
    val init = nrootApprox(value, k)
    loop(init, Int.MinValue, JBigDecimal.ZERO)
  }

  private val bits2dec: Double = log(2, 10)

  /**
   * Returns a relative approximation of the n-th root of `value`, up to
   * the number of digits specified by `mc`. This only uses the rounding mode
   * to chop-off the few remaining digits after the approximation, so may be
   * inaccurate.
   */
  final def nroot(value: JBigDecimal, n: Int, mc: MathContext): JBigDecimal = {
    val result = nroot(value, n) { x =>
      x.scale - ceil(x.unscaledValue.bitLength * bits2dec).toInt + mc.getPrecision + 1
    }
    result.round(mc)
  }

  /**
   * Returns an absolute approximation of the n-th root of `value`, up to
   * `scale` digits past the decimal point. This only uses the rounding mode
   * to chop-off the few remaining digits after the approximation, so may be
   * inaccurate.
   */
  final def nroot(value: JBigDecimal, n: Int, scale: Int, roundingMode: RoundingMode): JBigDecimal =
    nroot(value, n)(_ => scale + 1).setScale(scale, roundingMode)

  private[math] val Half = new JBigDecimal(0.5)

  implicit val JBigDecimalOrder: Order[JBigDecimal] = new Order[JBigDecimal] {
    def compare(x: JBigDecimal, y: JBigDecimal): Int = x compareTo y
  }

  /**
   * Rounds an approximation (`approx`) to the `exact` Algebraic value using
   * the given `scale` and `RoundingMode` (`mode`). This will always be
   * accurate for any algebraic number. So, if `exact` represents 0.15 and the
   * rounding mode is set to `HALF_UP` with a scale of 1, then this is
   * guaranteed to round up to 0.2.
   *
   * @param exact  the exact value to use a reference for tricky cases
   * @param approx the approximate value to round
   * @param scale  the final scale of the result
   * @param mode   the rounding mode to use
   */
  private def roundExact(exact: Algebraic, approx: JBigDecimal, scale: Int, mode: RoundingMode): JBigDecimal = {
    import RoundingMode.{ CEILING, FLOOR, UP }

    if (approx.signum == 0) {
      // If the sign is 0, then we deal with it here.
      mode match {
        case UP | CEILING if exact.signum > 0 =>
          new JBigDecimal(BigInteger.ONE, scale)
        case UP | FLOOR if exact.signum < 0 =>
          new JBigDecimal(BigInteger.ONE.negate, scale)
        case _ =>
          approx.setScale(scale)
      }
    } else if (approx.signum > 0) {
      roundPositive(exact, approx, scale, mode)
    } else {
      val adjustedMode = mode match {
        case CEILING => FLOOR
        case FLOOR => CEILING
        case _ => mode
      }
      roundPositive(-exact, approx.abs, scale, adjustedMode).negate()
    }
  }

  private def roundPositive(exact: Algebraic, approx: JBigDecimal, scale: Int, mode: RoundingMode): JBigDecimal = {
    import RoundingMode.{ CEILING, FLOOR, DOWN, UP, HALF_DOWN, HALF_UP, HALF_EVEN, UNNECESSARY }

    val cutoff = approx.scale - scale
    if (cutoff == 0) {
      // Nothing to do here.
      approx
    } else if (cutoff < 0) {
      // Just add some 0s and we're done!
      approx.setScale(scale)
    } else if (cutoff > 18) {
      // We'd like to work with Long arithmetic, if possible. Our rounding is
      // exact anyways, so it doesn't hurt to remove some digits.
      roundPositive(exact, approx.setScale(scale + 18), scale, mode)
    } else {
      val unscale = spire.math.pow(10L, cutoff.toLong)
      val Array(truncatedUnscaledValue, bigRemainder) =
        approx
          .unscaledValue
          .divideAndRemainder(BigInteger.valueOf(unscale))
      val truncated = new JBigDecimal(truncatedUnscaledValue, scale)
      def epsilon = new JBigDecimal(BigInteger.ONE, scale)
      val remainder = bigRemainder.longValue
      val rounded = mode match {
        case UNNECESSARY =>
          truncated

        case HALF_DOWN | HALF_UP | HALF_EVEN =>
          val dangerZoneStart = (unscale / 2) - 1
          val dangerZoneStop = dangerZoneStart + 2
          if (remainder >= dangerZoneStart && remainder <= dangerZoneStop) {
            val splitter = BigDecimal(new JBigDecimal(
              truncatedUnscaledValue.multiply(BigInteger.TEN).add(BigInteger.valueOf(5)),
              scale + 1
            ))
            val cmp = exact compare Algebraic(splitter)
            val roundUp = (mode: @unchecked) match {
              case HALF_DOWN => cmp > 0
              case HALF_UP => cmp >= 0
              case HALF_EVEN => cmp > 0 || cmp == 0 && truncatedUnscaledValue.testBit(0)
            }
            if (roundUp) truncated.add(epsilon)
            else truncated
          } else if (remainder < dangerZoneStart) {
            truncated
          } else {
            truncated.add(epsilon)
          }

        case CEILING | UP =>
          if (remainder <= 1 && exact <= Algebraic(BigDecimal(truncated))) {
            truncated
          } else {
            truncated.add(epsilon)
          }

        case FLOOR | DOWN =>
          if (remainder <= 0) {
            if (exact < Algebraic(BigDecimal(truncated))) {
              truncated.subtract(epsilon)
            } else {
              truncated
            }
          } else if (remainder >= (unscale - 1)) {
            val roundedUp = truncated.add(epsilon)
            if (exact >= Algebraic(BigDecimal(roundedUp))) {
              roundedUp
            } else {
              truncated
            }
          } else {
            truncated
          }
      }

      rounded
    }
  }
}

trait AlgebraicInstances {
  implicit final val AlgebraicAlgebra = new AlgebraicAlgebra
  import NumberTag._
  implicit final val AlgebraicTag = new LargeTag[Algebraic](Exact, Algebraic(0))
}

private[math] trait AlgebraicIsFieldWithNRoot extends Field[Algebraic] with NRoot[Algebraic] {
  def zero: Algebraic = Algebraic.Zero
  def one: Algebraic = Algebraic.One
  def plus(a: Algebraic, b: Algebraic): Algebraic = a + b
  def negate(a: Algebraic): Algebraic = -a
  override def minus(a: Algebraic, b: Algebraic): Algebraic = a - b
  override def pow(a: Algebraic, b: Int): Algebraic = a pow b
  override def times(a: Algebraic, b: Algebraic): Algebraic = a * b
  def quot(a: Algebraic, b: Algebraic): Algebraic = a /~ b
  def mod(a: Algebraic, b: Algebraic): Algebraic = a % b
  def gcd(a: Algebraic, b: Algebraic): Algebraic = euclid(a, b)(Eq[Algebraic])
  def div(a:Algebraic, b:Algebraic) = a / b
  def nroot(a: Algebraic, k: Int): Algebraic = a nroot k
  def fpow(a:Algebraic, b:Algebraic) = throw new UnsupportedOperationException("unsupported operation")
  override def fromInt(n: Int): Algebraic = Algebraic(n)
  override def fromDouble(n: Double): Algebraic = Algebraic(n)
}

private[math] trait AlgebraicIsReal extends IsReal[Algebraic] {
  def toDouble(x: Algebraic): Double = x.toDouble
  def ceil(a:Algebraic) = Algebraic(a.toBigDecimal(0, RoundingMode.CEILING))
  def floor(a:Algebraic) = Algebraic(a.toBigDecimal(0, RoundingMode.FLOOR))
  def round(a:Algebraic) = Algebraic(a.toBigDecimal(0, RoundingMode.HALF_EVEN))
  def isWhole(a:Algebraic) = a.isWhole
  override def sign(a: Algebraic): Sign = a.sign
  def signum(a: Algebraic): Int = a.signum
  def abs(a: Algebraic): Algebraic = a.abs
  override def eqv(x: Algebraic, y: Algebraic) = x.compare(y) == 0
  override def neqv(x: Algebraic, y: Algebraic) = x.compare(y) != 0
  def compare(x: Algebraic, y: Algebraic) = x.compare(y)
}

@SerialVersionUID(1L)
class AlgebraicAlgebra extends AlgebraicIsFieldWithNRoot with AlgebraicIsReal with Serializable
