package spire.math

import java.lang.Long.numberOfLeadingZeros
import java.lang.Double.{ isInfinite, isNaN }
import java.math.{ MathContext, RoundingMode, BigInteger, BigDecimal => JBigDecimal }
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.math.{ ScalaNumber, ScalaNumericConversions }
import scala.reflect.ClassTag

import spire.algebra.{Eq, EuclideanRing, Field, IsAlgebraic, NRoot, Order, Ring, Sign, Signed}
import spire.algebra.Sign.{ Positive, Negative, Zero }
import spire.macros.Checked.checked
import spire.math.poly.{ Term, BigDecimalRootRefinement, RootFinder, Roots }
import spire.std.bigInt._
import spire.std.bigDecimal._
import spire.std.long._
import spire.syntax.std.seq._

/**
 * Algebraic provides an exact number type for algebraic numbers. Algebraic
 * numbers are roots of polynomials with rational coefficients. With it, we can
 * represent expressions involving addition, multiplication, division, n-roots
 * (eg. `sqrt` or `cbrt`), and roots of rational polynomials. So, it is similar
 * [[Rational]], but adds roots as a valid, exact operation. The cost is that
 * this will not be as fast as [[Rational]] for many operations.
 *
 * In general, you can assume all operations on this number type are exact,
 * except for those that explicitly construct approximations to an Algebraic
 * number, such as `toBigDecimal`.
 *
 * For an overview of the ideas, algorithms, and proofs of this number type,
 * you can read the following papers:
 *
 *  - "On Guaranteed Accuracy Computation." C. K. Yap.
 *  - "Recent Progress in Exact Geometric Computation." C. Li, S. Pion, and C. K. Yap.
 *  - "A New Constructive Root Bound for Algebraic Expressions" by C. Li & C. K. Yap.
 *  - "A Separation Bound for Real Algebraic Expressions." C. Burnikel, et al.
 */
@SerialVersionUID(1L)
final class Algebraic private (val expr: Algebraic.Expr)
extends ScalaNumber with ScalaNumericConversions with Serializable {
  import Algebraic.{ Zero, One, Expr, MinIntValue, MaxIntValue, MinLongValue, MaxLongValue, JBigDecimalOrder, roundExact, BFMSS, LiYap }

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

  /**
   * Return a non-negative `Algebraic` with the same magnitude as this one.
   */
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

  /**
   * Returns an `Algebraic` whose value is just the integer part of
   * `this / that`. This operation is exact.
   */
  def quot(that: Algebraic): Algebraic =
    this /~ that

  /** An alias for [[quot]]. */
  def /~(that: Algebraic): Algebraic =
    Algebraic((this / that).toBigInt)

  /**
   * Returns an `Algebraic` whose value is the difference between `this` and
   * `(this /~ that) * that` -- the modulus.
   */
  def mod(that: Algebraic): Algebraic =
    this % that

  /** An alias for [[mod]]. */
  def %(that: Algebraic): Algebraic =
    this - (this /~ that) * that

  /** Returns the square root of this number. */
  def sqrt: Algebraic = nroot(2)

  /** Returns the cube root of this number. */
  def cbrt: Algebraic = nroot(3)

  /** Returns the `k`-th root of this number. */
  def nroot(k: Int): Algebraic = if (k < 0) {
    new Algebraic(Expr.Div(Expr.ConstantLong(1), Expr.KRoot(this.expr, -k)))
  } else if (k > 0) {
    new Algebraic(Expr.KRoot(this.expr, k))
  } else {
    throw new ArithmeticException("divide by zero (0-root)")
  }

  /** Raise this number to the `k`-th power. */
  def pow(k: Int): Algebraic =
    if (k == Int.MinValue) {
      throw new ArithmeticException(s"illegal exponent (${Int.MinValue})")
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

  /**
   * Returns an integer with the same sign as `this - that`. Specifically, if
   * `this &lt; that`, then the sign is negative, if `this &gt; that`, then the
   * sign is positive, otherwise `this == that` and this returns 0.
   */
  def compare(that: Algebraic): Int = (this - that).signum

  /**
   * Returns `true` iff this Algebraic number is exactly 0.
   */
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

  def toExprString: String = {
    import Expr._

    def recur(e: Expr): String = e match {
      case ConstantLong(n) => n.toString
      case ConstantDouble(n) => n.toString
      case ConstantBigDecimal(n) => n.toString
      case ConstantRational(n) => s"(${n})"
      case ConstantRoot(poly, i, _, _) => s"root($poly, $i)"
      case Neg(sub) => s"-$sub"
      case Add(lhs, rhs) => s"(${recur(lhs)}) + (${recur(rhs)})"
      case Sub(lhs, rhs) => s"(${recur(lhs)}) - (${recur(rhs)})"
      case Mul(lhs, rhs) => s"(${recur(lhs)}) * (${recur(rhs)})"
      case Div(lhs, rhs) => s"(${recur(lhs)}) / (${recur(rhs)})"
      case KRoot(sub, 2) => s"(${recur(sub)}).sqrt"
      case KRoot(sub, 3) => s"(${recur(sub)}).cbrt"
      case KRoot(sub, k) => s"(${recur(sub)}).nroot($k)"
      case Pow(sub, k) => s"${recur(sub)}.pow(k)"
    }

    recur(expr)
  }

  override def toString: String = {
    val approx = toBigDecimal(MathContext.DECIMAL64)
    if (this == Algebraic(approx)) {
      if (approx.signum == 0) {
        "Algebraic(0)"
      } else {
        s"Algebraic(${approx.bigDecimal.stripTrailingZeros})"
      }
    } else {
      s"Algebraic(~$approx)"
    }
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

  /**
   * Returns a `Float` that approximates this value. If the exponent is too
   * large to fit in a float, the `Float.PositiveInfinity` or
   * `Float.NegativeInfinity` is returned.
   */
  def floatValue: Float = toBigDecimal(MathContext.DECIMAL32).toFloat

  /**
   * Returns a `Double` that approximates this value. If the exponent is too
   * large to fit in a double, the `Double.PositiveInfinity` or
   * `Double.NegativeInfinity` is returned.
   */
  def doubleValue: Double = toBigDecimal(MathContext.DECIMAL64).toDouble

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
   * Absolute approximation to `scale` decimal places with the given rounding
   * mode. Rounding is always exact.
   */
  def toBigDecimal(scale: Int, roundingMode: RoundingMode): BigDecimal =
    BigDecimal(roundExact(this, expr.toBigDecimal(scale + 2), scale, roundingMode))

  /**
   * Relative approximation to the precision specified in `mc` with the given
   * rounding mode. Rounding is always exact. The sign is always correct; the
   * sign of the returned `BigDecimal` matches the sign of the exact value this
   * `Algebraic` represents.
   *
   * @param mc the precision and rounding mode of the final result
   * @return an approximation to the value of this algebraic number
   */
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
      case ConstantRoot(poly, _, lb, ub) =>
        // Ugh - on an airplane and can't trust BigDecimal's constructors.
        val poly0 = poly.map { n => new BigDecimal(new JBigDecimal(n.bigInteger), MathContext.UNLIMITED) }
        BigDecimalRootRefinement(poly0, lb, ub, new MathContext(digits, roundingMode)).approximateValue
      case Neg(sub) =>
        rec(sub, digits).negate()
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
        subValue.pow(k, new MathContext(digits, roundingMode))
    }
    val approx = rec(expr, mc.getPrecision + 2)
    val newScale = approx.scale - approx.precision + mc.getPrecision
    val adjustedApprox =
      if (newScale <= approx.scale) approx.setScale(newScale + 1, RoundingMode.DOWN)
      else approx
    roundExact(this, adjustedApprox, newScale, roundingMode)
      .round(mc) // We perform a final round, since roundExact uses scales.
  }

  /**
   * Returns `true` iff this Algebraic exactly represents a valid `BigInt`.
   */
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
  def toRational: Option[Rational] =
    if (expr.flags.isRational) {
      implicit val nroot: NRoot[Rational] with RootFinder[Rational] =
        new NRoot[Rational] with RootFinder[Rational] {
          private def fail =
            throw new ArithmeticException(s"Rational cannot support exact algebraic operations")
          def nroot(a: Rational, n: Int): Rational = fail
          def fpow(a:Rational, b:Rational): Rational = fail
          def findRoots(poly: Polynomial[Rational]): Roots[Rational] = fail
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
   *
   * TODO: Eq/ClassTag come from poly.map - would love to get rid of them.
   */
  def evaluateWith[A: Field: NRoot: RootFinder: Eq: ClassTag](implicit conv: ConvertableTo[A]): A = {
    import spire.syntax.field._
    import spire.syntax.nroot._
    import Expr._

    def eval(e: Expr): A = e match {
      case ConstantLong(n) => conv.fromLong(n)
      case ConstantDouble(n) => conv.fromDouble(n)
      case ConstantBigDecimal(n) => conv.fromBigDecimal(n)
      case ConstantRational(n) => conv.fromRational(n)
      case ConstantRoot(poly, i, _, _) =>
        RootFinder[A].findRoots(poly.map(conv.fromBigInt)).get(i)
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

  /** Returns an Algebraic expression equal to 0. */
  val Zero: Algebraic = new Algebraic(Expr.ConstantLong(0))

  /** Returns an Algebraic expression equal to 1. */
  val One: Algebraic = new Algebraic(Expr.ConstantLong(1))

  /** Returns an Algebraic expression equivalent to `n`. */
  implicit def apply(n: Int): Algebraic =
    new Algebraic(Expr.ConstantLong(n))

  /** Returns an Algebraic expression equivalent to `n`. */
  def apply(n: Long): Algebraic =
    new Algebraic(Expr.ConstantLong(n))

  /**
   * Returns an Algebraic expression equivalent to `n`, if `n` is finite. If
   * `n` is either infinite or `NaN`, then an `IllegalArgumentException` is
   * thrown.
   */
  def apply(n: Float): Algebraic =
    Algebraic(n.toDouble)

  /**
   * Returns an Algebraic expression equivalent to `n`, if `n` is finite. If
   * `n` is either infinite or `NaN`, then an `IllegalArgumentException` is
   * thrown.
   */
  implicit def apply(n: Double): Algebraic =
    if (java.lang.Double.isInfinite(n)) {
      throw new IllegalArgumentException("cannot construct inifinite Algebraic")
    } else if (java.lang.Double.isNaN(n)) {
      throw new IllegalArgumentException("cannot construct Algebraic from NaN")
    } else {
      new Algebraic(Expr.ConstantDouble(n))
    }

  /** Returns an Algebraic expression equivalent to `n`. */
  def apply(n: BigInt): Algebraic =
    new Algebraic(Expr.ConstantBigDecimal(BigDecimal(n)))

  /** Returns an Algebraic expression equivalent to `n`. */
  def apply(n: BigDecimal): Algebraic =
    new Algebraic(Expr.ConstantBigDecimal(n))

  /** Returns an Algebraic expression equivalent to `n`. */
  def apply(n: Rational): Algebraic =
    new Algebraic(Expr.ConstantRational(n))

  /**
   * Returns an Algebraic expression whose value is equivalent to the `i`-th
   * real root of the [[Polynomial]] `poly`. If `i` is negative or does not an
   * index a real root (eg the value is greater than or equal to the number of
   * real roots) then an `ArithmeticException` is thrown. Roots are indexed
   * starting at 0.  So if there are 3 roots, then they are indexed as 0, 1,
   * and 2.
   *
   * @param poly the polynomial containing at least i real roots
   * @param i    the index (0-based) of the root
   * @return an algebraic whose value is the i-th root of the polynomial
   */
  def root(poly: Polynomial[Rational], i: Int): Algebraic = {
    if (i < 0) {
      throw new ArithmeticException(s"invalid real root index: $i")
    } else {
      val zpoly = Roots.removeFractions(poly)
      val intervals = Roots.isolateRoots(zpoly)
      if (i >= intervals.size) {
        throw new ArithmeticException(s"cannot extract root $i, there are only ${intervals.size} roots")
      }
      intervals(i) match {
        case Point(value) =>
          new Algebraic(Expr.ConstantRational(value))
        case Bounded(lb, ub, _) =>
          new Algebraic(Expr.ConstantRoot(zpoly, i, lb, ub))
        case _ =>
          throw new RuntimeException("invalid isolated root interval")
      }
    }
  }

  /**
   * Returns all of the real roots of the given polynomial, in order from
   * smallest to largest.
   *
   * @param poly the polynomial to return the real roots of
   * @return all the real roots of `poly`
   */
  def roots(poly: Polynomial[Rational]): Vector[Algebraic] = {
    val zpoly = Roots.removeFractions(poly)
    val intervals = Roots.isolateRoots(zpoly)
    intervals.zipWithIndex map {
      case (Point(value), _) =>
        new Algebraic(Expr.ConstantRational(value))
      case (Bounded(lb, ub, _), i) =>
        new Algebraic(Expr.ConstantRoot(zpoly, i, lb, ub))
      case x =>
        throw new RuntimeException(s"invalid isolated root interval: $x")
    }
  }

  /**
   * Returns an Algebraic whose value is the real root within (lb, ub). This is
   * potentially unsafe, as we assume that exactly 1 real root lies within the
   * interval, otherwise the results are undetermined.
   *
   * @param poly a polynomial with a real root within (lb, ub)
   * @param i    the index of the root in the polynomial
   * @param lb   the lower bound of the open interval containing the root
   * @param ub   the upper bound of the open interval containing the root
   */
  def unsafeRoot(poly: Polynomial[BigInt], i: Int, lb: Rational, ub: Rational): Algebraic =
    new Algebraic(Expr.ConstantRoot(poly, i, lb, ub))

  /**
   * Returns an Algebraic expression equivalent to `BigDecimal(n)`. If `n` is
   * not parseable as a `BigDecimal` then an exception is thrown.
   */
  def apply(n: String): Algebraic =
    Algebraic(BigDecimal(new JBigDecimal(n)))

  /**
   * The [[Algebraic]] expression AST. `Algebraic` simply stores an expression
   * tree representing all operations performed on it. We then use this tree to
   * deduce certain properties about the algebraic expression and use them to
   * perform exact sign tests, compute approximations, etc.
   *
   * Generally, this should be regarded as an internal implementation detail of
   * `Algebraic`.
   */
  sealed abstract class Expr extends Serializable {
    import Expr._

    protected def flagBits: Int

    /**
     * A set of flags we can quickly compute for an [[Algebraic]] expression.
     *
     * @note we have to do this round-about trip between flagsBits and flags
     * because of 
     */
    def flags: Flags = new Flags(flagBits)

    private val bounds: TrieMap[ZeroBoundFunction, Any] =
      new TrieMap

    /**
     * Returns the bound for `zbf`, using a cached value if it is available.
     */
    def getBound(zbf: ZeroBoundFunction): zbf.Bound =
      bounds.getOrElseUpdate(zbf, zbf(this)).asInstanceOf[zbf.Bound]

    @volatile
    private var cachedDegreeBound: Long = 0L

    private def radicalNodes(): Set[KRoot] = {
      val childRadicals = children.foldLeft(Set.empty[KRoot]) { (acc, child) =>
        acc ++ child.radicalNodes()
      }
      val radicals = this match {
        case expr @ KRoot(sub, k) =>
          childRadicals + expr
        case _ =>
          childRadicals
      }
      if (cachedDegreeBound == 0L) {
        cachedDegreeBound = radicals.foldLeft(1L) { (acc, kroot) =>
          checked { acc * kroot.k }
        }
      }
      radicals
    }

    /**
     * Returns a bound on the degree of this expression.
     */
    def degreeBound: Long = {
      if (cachedDegreeBound == 0L)
        radicalNodes()
      cachedDegreeBound
    }

    /**
     * Returns the BFMSS separation bound.
     */
    def bfmssBound: BitBound =
      new BitBound(getBound(BFMSS).getBitBound(degreeBound))

    /**
     * Returns the Li & Yap separation bound.
     */
    def liYapBound: BitBound =
      new BitBound(getBound(LiYap).getBitBound(degreeBound))

    /**
     * Returns a separation bound for this expression as a bit bound. A
     * separation bound is a lower-bound on the value of this expression that
     * is only valid if this expression is not 0. This bound can thus be used
     * to determine if this value is actually 0 and, if not, the sign, by
     * simply approximating the expression with enough accuracy that it falls
     * on one side or the other of the separation bound.
     */
    def separationBound: BitBound =
      bfmssBound min liYapBound

    /**
     * Returns an asbolute approximation to this expression as a BigDecimal
     * that is accurate up to +/- 10^-digits.
     */
    def toBigDecimal(digits: Int): JBigDecimal

    /**
     * Returns an upper bound on the absolute value of this expression as a
     * bit bound.
     */
    def upperBound: BitBound

    /**
     * Returns a lower bound on the absolute value of this expression as a
     * bit bound.
     *
     * TODO: We could do better here wrt to addition (need a fastSignum: Option[Int])
     */
    def lowerBound: BitBound = -separationBound

    /** Returns an integer with the same sign as this expression. */
    def signum: Int

    /**
     * Returns a list of the children of this expression. A child is a
     * sub-expression required by this expression. For instance, `Add` has 2
     * children, the left-hand and right-hand side sub-expressions. A numeric
     * literal expression, such as `ConstantDouble` or `ConstantRational` has
     * no children.
     */
    def children: List[Expr]
  }

  object Expr {

    /**
     * A set of flags for algebraic expressions, so we can quickly determine
     * some properties, like whether the expression is rational, radical, what
     * types of leaf nodes it has, etc. This is used to help guide algorithmic
     * choices, such as what separation bound to use.
     */
    final class Flags(val bits: Int) extends AnyVal {
      import Flags._

      /** Returns the union of flags `this` and `that`. */
      def | (that: Flags): Flags = new Flags(bits | that.bits)

      private def check(n: Int): Boolean = (bits & n) != 0

      /** Returns `true` iff this expression is composed only of rational operations. */
      def isRational: Boolean = !isRadical

      /** Returns `true` iff this expression contains an n-th root operation. */
      def isRadical: Boolean = check(RadicalFlag)

      /** Returns `true` iff this expression contains a `ConstantDouble` leaf node. */
      def hasDoubleLeaf: Boolean = check(HasDoubleLeaf)

      /** Returns `true` iff this expression contains a `ConstantBigDecimal` leaf node. */
      def hasBigDecimalLeaf: Boolean = check(HasBigDecimalLeaf)

      /** Returns `true` iff this expression contains a `ConstantRational` leaf node. */
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

    /** Constant expressions are leaf nodes, contains literal numbers. */
    sealed abstract class Constant[A] extends Expr {
      def value: A
      def children: List[Expr] = Nil
    }

    /** Unary expressions contain only a single child expression. */
    sealed abstract class UnaryExpr extends Expr {
      val sub: Expr
      def children: List[Expr] = sub :: Nil
    }

    /** Binary expressions contain 2 child expression. */
    sealed abstract class BinaryExpr extends Expr {
      val lhs: Expr
      val rhs: Expr
      val flagBits: Int = (lhs.flags | rhs.flags).bits
      def children: List[Expr] = lhs :: rhs :: Nil
    }

    @SerialVersionUID(0L)
    case class ConstantLong(value: Long) extends Constant[Long] {
      def flagBits: Int = Flags.IntegerLeaf.bits

      def upperBound: BitBound =
        if (value == 0L) new BitBound(0L)
        else if (value == Long.MinValue) new BitBound(64)
        else new BitBound(64 - numberOfLeadingZeros(abs(value) - 1))

      def signum: Int = value.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        new JBigDecimal(value).setScale(digits, RoundingMode.HALF_UP)
    }

    @SerialVersionUID(0L)
    case class ConstantDouble(value: Double) extends Constant[Double] {
      def flagBits: Int = Flags.DoubleLeaf.bits

      def upperBound: BitBound = if (value == 0D) {
        new BitBound(0)
      } else {
        new BitBound(ceil(log(abs(value))).toLong)
      }

      def signum: Int =
        if (value < 0D) -1
        else if (value > 0D) 1
        else 0

      def toBigDecimal(digits: Int): JBigDecimal =
        new JBigDecimal(value).setScale(digits, RoundingMode.HALF_UP)
    }

    @SerialVersionUID(0L)
    case class ConstantBigDecimal(value: BigDecimal) extends Constant[BigDecimal] {
      def flagBits: Int = Flags.BigDecimalLeaf.bits

      def upperBound: BitBound = if (value.signum == 0) {
        new BitBound(0)
      } else {
        // We just need a couple of digits, really.
        val mc = new MathContext(4, RoundingMode.UP)
        new BitBound(ceil(log(value.abs(mc))).toLong)
      }

      def signum: Int = value.signum

      def toBigDecimal(digits: Int): JBigDecimal =
        value.bigDecimal.setScale(digits, RoundingMode.HALF_UP)
    }

    @SerialVersionUID(0L)
    case class ConstantRational(value: Rational) extends Constant[Rational] {
      def flagBits: Int = Flags.RationalLeaf.bits

      def upperBound: BitBound =
        new BitBound(value.numerator.abs.bitLength - value.denominator.bitLength + 1)

      def signum: Int = value.signum

      def toBigDecimal(digits: Int): JBigDecimal = {
        val num = new JBigDecimal(value.numerator.bigInteger)
        val den = new JBigDecimal(value.denominator.bigInteger)
        num.divide(den, digits, RoundingMode.DOWN)
      }
    }

    @SerialVersionUID(0L)
    case class ConstantRoot(poly: Polynomial[BigInt], i: Int, lb: Rational, ub: Rational) extends Constant[Polynomial[BigInt]] {
      def value: Polynomial[BigInt] = poly

      def flagBits: Int = Flags.IsRadical.bits

      def upperBound: BitBound =
        if (ub.signum > 0) {
          new BitBound(ub.numerator.bitLength - ub.denominator.bitLength + 1)
        } else {
          new BitBound(lb.numerator.abs.bitLength - lb.denominator.bitLength + 1)
        }

      def signum: Int =
        if (lb.signum != 0) lb.signum
        else ub.signum

      private val refinement: AtomicReference[BigDecimalRootRefinement] = {
        val poly0 = poly.map { n => new BigDecimal(new JBigDecimal(n.bigInteger), MathContext.UNLIMITED) }
        new AtomicReference(BigDecimalRootRefinement(poly0, lb, ub))
      }

      def toBigDecimal(digits: Int): JBigDecimal = {
        val oldRefinement = refinement.get
        val newRefinement = oldRefinement.refine(digits)
        refinement.set(newRefinement)
        newRefinement.approximateValue
      }

      def lead: BigInt = poly.maxTerm.coeff
      def tail: BigInt = poly.minTerm.coeff
    }

    @SerialVersionUID(0L)
    case class Neg(sub: Expr) extends UnaryExpr {
      def flagBits: Int = sub.flags.bits
      def upperBound: BitBound = sub.upperBound
      def signum: Int = -sub.signum
      def toBigDecimal(digits: Int): JBigDecimal =
        sub.toBigDecimal(digits).negate()
    }

    @SerialVersionUID(0L)
    sealed abstract class AddOrSubExpr extends BinaryExpr {
      def upperBound: BitBound =
        new BitBound(max(lhs.upperBound.bitBound, rhs.upperBound.bitBound) + 1)

      lazy val signum: Int = {
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

    @SerialVersionUID(0L)
    case class Add(lhs: Expr, rhs: Expr) extends AddOrSubExpr

    @SerialVersionUID(0L)
    case class Sub(lhs: Expr, rhs: Expr) extends AddOrSubExpr

    @SerialVersionUID(0L)
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

    @SerialVersionUID(0L)
    case class Div(lhs: Expr, rhs: Expr) extends BinaryExpr {
      def upperBound: BitBound = lhs.upperBound - rhs.lowerBound
      def signum: Int = if (rhs.signum == 0) {
        throw new ArithmeticException("divide by 0")
      } else {
        lhs.signum * rhs.signum
      }
      def toBigDecimal(digits: Int): JBigDecimal = checked {
        val lDigits = digits + 2 - rhs.lowerBound.decimalDigits
        val rDigits = max(
          1 - rhs.lowerBound.decimalDigits,
          digits + 4 - 2 * rhs.lowerBound.decimalDigits + lhs.upperBound.decimalDigits
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

    @SerialVersionUID(0L)
    case class KRoot(sub: Expr, k: Int) extends UnaryExpr {
      val flagBits: Int = (sub.flags | Flags.IsRadical).bits

      def upperBound: BitBound = (sub.upperBound + 1) / 2

      def signum: Int = {
        val s = sub.signum
        if (s >= 0) s
        else throw new ArithmeticException(s"$k-root of negative number")
      }

      def toBigDecimal(digits: Int): JBigDecimal = {
        val digits0 = max(
          checked(digits + 1),
          checked(1 - (sub.lowerBound.decimalDigits + 1) / 2)
        )
        if (digits0 >= Int.MaxValue) {
          throw new IllegalArgumentException("required precision is too high")
        } else {
          val value = sub.toBigDecimal(digits0.toInt)
          Algebraic.nroot(value, k, digits, RoundingMode.DOWN)
        }
      }

      // To avoid multiple traversals during degreeBound, we cache the hashCode
      // for KRoots.
      override lazy val hashCode: Int =
        sub.hashCode * 23 + k * 29 + 13
    }

    @SerialVersionUID(0L)
    case class Pow(sub: Expr, k: Int) extends UnaryExpr {
      require(k > 1)

      def flagBits: Int = sub.flags.bits

      def upperBound: BitBound = sub.upperBound * k
      def signum: Int = {
        val s = sub.signum
        if (s == 0) {
          if (k < 0) throw new ArithmeticException("divide by 0")
          else if (k == 0) throw new ArithmeticException("indeterminate")
          else 0
        } else if (k % 2 == 0) {
          if (s < 0) 1 else s
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

  /**
   * A bit bound represents either an upper or lower bound as some
   * power of 2. Specifically, the bound is typically either `2^bitBound` or
   * `2^-bitBound`.
   */
  final class BitBound(val bitBound: Long) extends AnyVal {
    import BitBound.bitsToDecimalDigits

    /**
     * Returns the minimum number of absolute decimal digits required to
     * represent this separation bound.
     */
    def decimalDigits: Long = bitsToDecimalDigits(bitBound)

    def unary_- : BitBound = new BitBound(-bitBound)

    def +(that: BitBound): BitBound = new BitBound(this.bitBound + that.bitBound)
    def -(that: BitBound): BitBound = new BitBound(this.bitBound - that.bitBound)
    def *(that: BitBound): BitBound = new BitBound(this.bitBound * that.bitBound)
    def /(that: BitBound): BitBound = new BitBound(this.bitBound / that.bitBound)

    def +(rhs: Int): BitBound = new BitBound(this.bitBound + rhs)
    def -(rhs: Int): BitBound = new BitBound(this.bitBound - rhs)
    def *(rhs: Int): BitBound = new BitBound(this.bitBound * rhs)
    def /(rhs: Int): BitBound = new BitBound(this.bitBound / rhs)

    def min(that: BitBound): BitBound =
      if (bitBound < that.bitBound) this else that

    override def toString: String = s"BitBound($bitBound)"
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
    val approx = new JBigDecimal(x.unscaledValue.abs, safeWidth).doubleValue
    new JBigDecimal(x.signum * pow(approx, 1D / k))
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
  private final def nroot(signedValue: JBigDecimal, k: Int)(getEps: JBigDecimal => Int): JBigDecimal = {
    if (signedValue.compareTo(JBigDecimal.ZERO) == 0)
      return JBigDecimal.ZERO
    val value = signedValue.abs
    val n = new JBigDecimal(k)
    @tailrec def loop(prev: JBigDecimal, prevDigits: Int, prevEps: JBigDecimal): JBigDecimal = {
      val digits = getEps(prev)
      val eps =
        if (digits == prevDigits) prevEps
        else JBigDecimal.ONE.movePointLeft(digits)
      val prevExp = prev.pow(k - 1)
      val delta = value
        .divide(prevExp, digits, RoundingMode.HALF_UP)
        .subtract(prev)
        .divide(n, digits, RoundingMode.HALF_UP)
      if (delta.abs.compareTo(eps) <= 0) prev
      else loop(prev.add(delta), digits, eps)
    }
    val init = nrootApprox(value, k)
    val unsignedResult = loop(init, Int.MinValue, JBigDecimal.ZERO)
    if (signedValue.signum < 0) unsignedResult.negate
    else unsignedResult
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

  private implicit val JBigDecimalOrder: Order[JBigDecimal] = new Order[JBigDecimal] {
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
          approx.setScale(scale, RoundingMode.DOWN)
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
      approx.setScale(scale, RoundingMode.DOWN)
    } else if (cutoff > 18) {
      // We'd like to work with Long arithmetic, if possible. Our rounding is
      // exact anyways, so it doesn't hurt to remove some digits.
      roundPositive(exact, approx.setScale(scale + 18, RoundingMode.DOWN), scale, mode)
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

  private val MaxIntValue: BigInteger = BigInteger.valueOf(Int.MaxValue.toLong)
  private val MinIntValue: BigInteger = BigInteger.valueOf(Int.MinValue.toLong)
  private val MaxLongValue: BigInteger = BigInteger.valueOf(Long.MaxValue)
  private val MinLongValue: BigInteger = BigInteger.valueOf(Long.MinValue)

  /**
   * A zero bound function, defined over an algebraic expression algebra.
   */
  sealed abstract class ZeroBoundFunction {

    /**
     * Some state that is computed for each node in the expression tree. This
     * state is typically memoized, to avoid recomputation.
     */
    type Bound

    def apply(expr: Algebraic.Expr): Bound
  }

  /** 
   * An implementation of "A New Constructive Root Bound for Algebraic
   * Expressions" by Chen Li & Chee Yap.
   */
  @SerialVersionUID(0L)
  case object LiYap extends ZeroBoundFunction {
    import Expr._

    final case class Bound(
      /** Bound on the leading coefficient. */
      lc: Long,
      /** Bound on the trailing coefficient. */
      tc: Long,
      /** Bound on the measure. */
      measure: Long,
      /** Lower bound on the value. */
      lb: Long,
      /** Upper bound on the value. */
      ub: Long
    ) {
      def getBitBound(degreeBound: Long): Long = checked {
        ub * (degreeBound - 1) + lc
      }
    }

    def apply(expr: Algebraic.Expr): Bound = checked {
      // Unfortunately, we must call degreeBound early, to avoid many redundant
      // traversals of the Expr tree. Getting this out of the way early on
      // means that we will traverse the tree once and populate the degreeBound
      // cache in all nodes right away. If we do it in a bottom up fashion,
      // then we risk terrible runtime behaviour.
      val degreeBound = expr.degreeBound
      expr match {
        case ConstantLong(n) =>
          rational(Rational(n))

        case ConstantDouble(n) =>
          rational(Rational(n))

        case ConstantBigDecimal(n) =>
          rational(Rational(n))

        case ConstantRational(n) =>
          rational(n)

        case root @ ConstantRoot(poly, _, _, _) =>
          // Bound on the euclidean distance of the coefficients.
          val distBound = poly.terms.map { case Term(c, _) =>
            2L * c.bitLength
          }.qsum / 2L + 1L
          Bound(
            root.lead.bitLength + 1L,
            root.tail.bitLength + 1L,
            distBound,
            Roots.lowerBound(poly),
            Roots.upperBound(poly)
          )

        case Neg(sub) =>
          sub.getBound(this)

        case expr: AddOrSubExpr =>
          val lhsExpr = expr.lhs
          val rhsExpr = expr.rhs
          val lhs = lhsExpr.getBound(this)
          val rhs = rhsExpr.getBound(this)
          val lc = lhs.lc * rhsExpr.degreeBound + rhs.lc * lhsExpr.degreeBound
          val tc = lhs.measure * rhsExpr.degreeBound + rhs.measure * lhsExpr.degreeBound + 2 * degreeBound
          val measure = tc
          val ub = max(lhs.ub, rhs.ub) + 1
          val lb = max(-measure, -(ub * (degreeBound - 1) + lc))
          Bound(lc, tc, measure, lb, ub)

        case Mul(lhsExpr, rhsExpr) =>
          val lhs = lhsExpr.getBound(this)
          val rhs = rhsExpr.getBound(this)
          val lc = lhs.lc * rhsExpr.degreeBound + rhs.lc * lhsExpr.degreeBound
          val tc = lhs.tc * rhsExpr.degreeBound + rhs.tc * lhsExpr.degreeBound
          val measure = lhs.measure * rhsExpr.degreeBound + rhs.measure * lhsExpr.degreeBound
          val lb = lhs.lb + rhs.lb
          val ub = lhs.ub + rhs.ub
          Bound(lc, tc, measure, lb, ub)

        case Div(lhsExpr, rhsExpr) =>
          val lhs = lhsExpr.getBound(this)
          val rhs = rhsExpr.getBound(this)
          val lc = lhs.lc * rhsExpr.degreeBound + rhs.tc * lhsExpr.degreeBound
          val tc = lhs.tc * rhsExpr.degreeBound + rhs.lc * lhsExpr.degreeBound
          val measure = lhs.measure * rhsExpr.degreeBound + rhs.measure * lhsExpr.degreeBound
          val lb = lhs.lb - rhs.ub
          val ub = lhs.ub - rhs.lb
          Bound(lc, tc, measure, lb, ub)

        case KRoot(subExpr, k) =>
          val sub = subExpr.getBound(this)
          val lb = sub.lb / k
          val ub = if (sub.ub % k == 0) (sub.ub / k)
                   else ((sub.ub / k) + 1)
          Bound(sub.lc, sub.tc, sub.measure, lb, ub)

        case Pow(subExpr, k) =>
          val sub = subExpr.getBound(this)
          Bound(sub.lc * k, sub.tc * k, sub.measure * k, sub.lb * k, sub.ub * k)
      }
    }

    private def rational(n: Rational): Bound = {
      // TODO: We can do better here. The + 1 isn't always needed in a & b.
      // Also, the upper and lower bounds could be much tighter if we actually
      // partially perform the division.
      val a = n.numerator.abs.bitLength + 1
      if (n.denominator == BigInt(1)) {
        Bound(0, a, a, a - 1, a)
      } else {
        val b = n.denominator.bitLength + 1
        Bound(b, a, max(a, b), a - b - 1, a - b + 1)
      }
    }
  }

  /**
   * An implementation of "A Separation Bound for Real Algebraic Expressions",
   * by Burnikel, Funke, Mehlhorn, Schirra, and Schmitt. This provides a good
   * [[ZeroBoundFunction]] for use in sign tests.
   *
   * Unlike the paper, we use log-arithmetic instead of working with exact,
   * big integer values. This means our bound isn't technically as good as it
   * could be, but we save the cost of working with arithmetic. We also perform
   * all log arithmetic using `Long`s and check for overflow (throwing
   * `ArithmeticException`s when detected). In practice we shouldn't hit this
   * limit, but in case we do, we prefer to throw over failing silently.
   */
  @SerialVersionUID(0L)
  case object BFMSS extends ZeroBoundFunction {
    import Expr._

    /** Our state that we store, per node. */
    final case class Bound(l: Long, u: Long) {
      def getBitBound(degreeBound: Long): Long = checked {
        l + u * (degreeBound - 1)
      }
    }

    def apply(expr: Algebraic.Expr): Bound = expr match {
      case ConstantLong(n) => integer(n)
      case ConstantDouble(n) => rational(n)
      case ConstantBigDecimal(n) => rational(n)
      case ConstantRational(n) => rational(n)
      case root @ ConstantRoot(poly, _, _, _) =>
        Bound(root.lead.bitLength + 1, Roots.upperBound(poly))
      case Neg(sub) => sub.getBound(this)
      case Add(lhs, rhs) => add(lhs.getBound(this), rhs.getBound(this))
      case Sub(lhs, rhs) => add(lhs.getBound(this), rhs.getBound(this))
      case Mul(lhs, rhs) => mul(lhs.getBound(this), rhs.getBound(this))
      case Div(lhs, rhs) => div(lhs.getBound(this), rhs.getBound(this))
      case KRoot(sub, k) => nroot(sub.getBound(this), k)
      case Pow(sub, k) => pow(sub.getBound(this), k)
    }

    private def integer(n: Long): Bound =
      integer(BigInt(n))

    private def integer(n: BigInt): Bound =
      Bound(0, n.abs.bitLength + 1)

    private def rational(n: Double): Bound =
      rational(BigDecimal(n))

    private def rational(n: BigDecimal): Bound =
      rational(Rational(n))

    private def rational(n: Rational): Bound =
      div(integer(n.numerator), integer(n.denominator))

    // We're not being fair to the BFMSS bound here. We're really just
    // setting a bound on the max value. However, the alternative would
    // require us to work outside of log arithmetic.
    private def add(lhs: Bound, rhs: Bound): Bound = checked {
      Bound(
        lhs.l + rhs.l,
        math.max(lhs.u + rhs.l, lhs.l + rhs.u) + 1
      )
    }

    private def mul(lhs: Bound, rhs: Bound): Bound = checked {
      Bound(
        lhs.l + rhs.l,
        lhs.u + rhs.u
      )
    }

    private def div(lhs: Bound, rhs: Bound): Bound = checked {
      Bound(
        lhs.l + rhs.u,
        lhs.u + rhs.l
      )
    }

    private def nroot(sub: Bound, k: Int): Bound = checked {
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

    private def pow(sub: Bound, k: Int): Bound = {
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

private[math] trait AlgebraicIsReal extends IsAlgebraic[Algebraic] {
  def toDouble(x: Algebraic): Double = x.toDouble
  def toAlgebraic(x: Algebraic): Algebraic = x
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
