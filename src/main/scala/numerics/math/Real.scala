/**
 * The `Real` number type's goal is to create a guaranteed accuracy number [1].
 * That is, a `Real` can always be approximated to any given accuracy and, in
 * addition, you are guaranteed that if a `Real` `a` represents a real number
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
package numerics.math

import java.math.{ MathContext, BigInteger, BigDecimal => BigDec }
import scala.math.{ ScalaNumber, ScalaNumericConversions, max }

import fpf.MaybeDouble
import real._


/**
 * An general Real type. Can be used represent real numbers and approximates
 * them on-demand.
 */
sealed abstract class Real extends ScalaNumber
                           with ScalaNumericConversions
                           with RealLike
                           with ConstantFolder
                           with BubbleUpDivs
                           with Ordered[Real] {
//sealed abstract class Real extends ScalaNumber with ScalaNumericConversions with Ordered[Real] {
  // import Real.transform

  /**
   * Used for the internal floating point filter. Though this is public, it
   * should only be used for good reason.
   */
  lazy val fpf: MaybeDouble = this match {
    case Add(a, b) => a.fpf + b.fpf
    case Sub(a, b) => a.fpf - b.fpf
    case Mul(a, b) => a.fpf * b.fpf
    case Div(a, b) => a.fpf / b.fpf
    case Neg(a) => -a.fpf
    case KRoot(a, k) => a.fpf nroot k
    case IntLit(n) => MaybeDouble(n)
    case BigIntLit(n) => MaybeDouble(n)
  }

  /*
  def abs: Real = if (this.sign == Negative) -this else this

  def *(that: Real): Real = transform(Mul(this, that))
  def +(that: Real): Real = transform(Add(this, that))
  def -(that: Real): Real = transform(Sub(this, that))
  def /(that: Real): Real = transform(Div(this, that))
  def unary_-(): Real = transform(Neg(this))
  def sqrt: Real = this nroot 2
  def nroot(k: Int): Real = {
    if (this.sign == Negative && k % 2 == 0) {
      throw new ArithmeticException("Cannot find an even root of a negative Real.")
    }

    transform(KRoot(this, k))
  }

  // TODO: Create Pow as a 1st class citizen.
  def pow(k: Int): Real = {
    require(k >= 0, "Exponent must be a non-negative integer.")

    if (k == 0) {
      Real(1)
    } else if (k == 1) {
      this
    } else {
      val x = this pow (k / 2)
      val x2 = x * x
      if (k % 2 == 0) x2 else x2 * this
    }
  }

  def compare(that: Real): Int = (this - that).signum
  */

  override def equals(that: Any) = that match {
    case that: Real => (this - that).sign == Zero
    case that: Rational => (this - Real(that)).sign == Zero
    case that: BigInt => isWhole && toBigInt == that
    case that: BigDecimal => try {
      toBigDecimal(that.mc) == that
    } catch {
      case ae: ArithmeticException => false
    }
    case _ => unifiedPrimitiveEquals(that)
  }

  override def hashCode: Int =
    if (isWhole && toBigInt == toLong) unifiedPrimitiveHashcode
    else toDouble.##


  /**
   * Returns `true` if this is a radical expression, `false` otherwise.
   */
  def isRadical: Boolean = this match {
    case Add(a, b) => a.isRadical || b.isRadical
    case Sub(a, b) => a.isRadical || b.isRadical
    case Mul(a, b) => a.isRadical || b.isRadical
    case Div(a, b) => a.isRadical || b.isRadical
    case Neg(a) => a.isRadical
    case KRoot(a, k) => true
    case IntLit(n) => false
    case BigIntLit(n) => false
  }


  def toBigInt: BigInt = fpf.toLong map (BigInt(_)) getOrElse (sign match {
    case Zero => BigInt(0)
    case Negative => -((-this).toBigInt)
    case Positive =>
      val a = this +/- 0.01
      val b = a.toBigInt

      if ((a + 0.02) >= BigDecimal(b + 1)) {
        (this - Real(b + 1)).sign match {
          case Positive => b + 1
          case Negative => b
          case Zero => b + 1
        }
      } else if ((a - 0.02) < BigDecimal(b)) {
        (this - Real(b)).sign match {
          case Positive => b
          case Negative => b - 1
          case Zero => b
        }
      } else {
        b
      }
  })

  def toBigDecimal(implicit mc: MathContext = MathContext.DECIMAL128): BigDecimal =
    this approximateTo mc

  def toRational(implicit ac: ApproximationContext[Rational] = ApproximationContext(Rational(1L, 10000000000000000L))): Rational = simulate[Rational]

  def approximateTo[A,B](a: A)(implicit approx: Approximation[Real,A,B]): B =
    approx(this, a)

  /**
   * Returns an absolute approximation to `this` s.t.
   * `this - err <= this +/- err <= this + err`.
   */
  def +/-(err: BigDecimal): BigDecimal = this approximateTo err

  /*
  def signum: Int = sign.toInt
  */

  // The sign of this `Real`.
  lazy val sign: Sign = fpf.sign getOrElse ({
    import Bounded._

    // The separation bound.
    val sep = BigDecimal(1, -this.decimalLowerBound)

    def findSign(scale: Int): Int = {
      val err = BigDecimal(1, scale)
      val a = this +/- err
      if (a.abs > err) {
        a.signum
      } else if (2 * err <= sep) {
        0
      } else {
        findSign(scale + 1)
      }
    }

    Sign(findSign(0))
  })

  /**
   * Simulates the expression DAG of this `Real` using another number type.
   */
  def simulate[A : Field : Exponential]: A = Real.simulate(this)


  def isWhole: Boolean = fpf.isWhole getOrElse {
    import Implicits._
    (this % Real(1)).sign == Zero
  }

  def underlying: AnyRef = this   // Why not?

  def doubleValue: Double = if (fpf.isExact) {
    fpf.approx
  } else this approximateTo Double

  def floatValue: Float = fpf.toFloat getOrElse doubleValue.toFloat

  def intValue: Int = fpf.toLong map (_.toInt) getOrElse toBigInt.toInt
  def longValue: Long = fpf.toLong getOrElse toBigInt.toLong
}


object Real {
  // private val t = AggregateTransformer(ConstantFolder, DivBubbleTransformer)
  // def transform(num: Real): Real = t.transform(num)

  implicit def apply(n: Int): Real = IntLit(n)
  implicit def apply(n: Long): Real = apply(BigInt(n))
  implicit def apply(n: BigInt): Real = if (n.isValidInt) {
    IntLit(n.toInt)
  } else {
    BigIntLit(n)
  }
  implicit def apply(n: Rational): Real = Real(n.numerator) / Real(n.denominator)
  implicit def apply(n: Double): Real = apply(Rational(n.toString))
  implicit def apply(n: BigDecimal): Real = apply(Rational(n))

  import Implicits._
  def simulate[A : Field : Exponential](n: Real): A = n match {
    case Add(a, b) => simulate(a) + simulate(b)
    case Sub(a, b) => simulate(a) - simulate(b)
    case Mul(a, b) => simulate(a) * simulate(b)
    case Div(a, b) => simulate(a) / simulate(b)
    case Neg(a) => -simulate(a)
    case KRoot(a, k) => simulate(a) nroot k
    case IntLit(n) => ring[A].fromInt(n)
    case BigIntLit(n) => ring[A].fromBigInt(n)
  }
}

sealed trait BinOp {
  def lhs:Real 
  def rhs:Real 
}

object BinOp {
  def unapply(n: Real): Option[(Real,Real)] = n match {
    case n: BinOp => Some((n.lhs, n.rhs))
    case _ => None
  }
}

case class Add(lhs: Real, rhs: Real) extends Real with BinOp
case class Sub(lhs: Real, rhs: Real) extends Real with BinOp
case class Mul(lhs: Real, rhs: Real) extends Real with BinOp
case class Div(lhs: Real, rhs: Real) extends Real with BinOp
case class Neg(a: Real) extends Real
case class KRoot(a: Real, k: Int) extends Real

case class IntLit(value: Int) extends Real {
  override lazy val sign: Sign = if (value == 0) Zero else if (value > 0) Positive else Negative
}

case class BigIntLit(value: BigInt) extends Real {
  override lazy val sign: Sign = if (value == 0) Zero else if (value > 0) Positive else Negative
}

/*
trait Transformer[A] {
  def transform(a: A): A
}

case class AggregateTransformer[A](t1: Transformer[A], t2: Transformer[A]) extends Transformer[A] {
  def transform(a: A): A = t2.transform(t1.transform(a))
}
*/
/**
 * Moves division up from the children of a Real to the root. For example,
 * `a/b + c/d` becomes `(a*d + c*b) / (b*d)`.
 *//*
object DivBubbleTransformer extends Transformer[Real] {
  def transform(num: Real): Real = num match {
    case _: BinOp => num match {
      case Add(Div(a, b), Div(c, d)) => Div(a * d + b * c, b * d)
      case Add(Div(a, b), c) => Div(a + b * c, b)
      case Add(a, Div(b, c)) => Div(a * c + b, c)
      case Sub(Div(a, b), Div(c, d)) => Div(a * d - b * c, b * d)
      case Sub(Div(a, b), c) => Div(a - b * c, b)
      case Sub(a, Div(b, c)) => Div(a * c - b, c)
      case Mul(Div(a, b), Div(c, d)) => Div(a * c, b * d)
      case Mul(Div(a, b), c) => Div(a * c, b)
      case Mul(a, Div(b, c)) => Div(a * b, c)
      case Div(Div(a, b), Div(c, d)) => Div(a * d, b * c)
      case Div(Div(a, b), c) => Div(a, b * c)
      case Div(a, Div(b, c)) => Div(a * c, b)
      case _ => num
    }
    case Neg(Div(a, b)) => Div(Neg(a), b)
    case KRoot(Div(a, b), k) => Div(KRoot(a * (b pow (k - 1)), k), b)
    case _ => num
  }
}

object ConstantFolder extends Transformer[Real] {
  private def wrap(n: Long): Real = if (n > Int.MaxValue || n < Int.MinValue) {
    BigIntLit(BigInt(n))
  } else {
    IntLit(n.toInt)
  }

  private def wrap(n: BigInt): Real =
    if (n.isValidInt) IntLit(n.toInt) else BigIntLit(n)

  def transform(num: Real): Real = num match {
    case Add(IntLit(a), IntLit(b)) => wrap((a: Long) + (b: Long))
    case Add(IntLit(a), BigIntLit(b)) => wrap(b + a)
    case Add(BigIntLit(a), IntLit(b)) => wrap(a + b)
    case Add(BigIntLit(a), BigIntLit(b)) => wrap(a + b)
    case Sub(IntLit(a), IntLit(b)) => wrap((a: Long) - (b: Long))
    case Sub(IntLit(a), BigIntLit(b)) => wrap(BigInt(a) - b)
    case Sub(BigIntLit(a), IntLit(b)) => wrap(a - b)
    case Sub(BigIntLit(a), BigIntLit(b)) => wrap(a - b)
    case Mul(IntLit(a), IntLit(b)) => wrap((a: Long) * (b: Long))
    case Mul(IntLit(a), BigIntLit(b)) => wrap(b * a)
    case Mul(BigIntLit(a), IntLit(b)) => wrap(a * b)
    case Mul(BigIntLit(a), BigIntLit(b)) => wrap(a * b)
    case _ => num
  }
}
*/
