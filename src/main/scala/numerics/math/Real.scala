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
import scala.math.max


/**
 * An general Real type. Can be used represent real numbers and approximates
 * them on-demand.
 */
sealed trait Real {
  import Real.transform

  def *(that: Real): Real = transform(Mul(this, that))
  def +(that: Real): Real = transform(Add(this, that))
  def -(that: Real): Real = transform(Sub(this, that))
  def /(that: Real): Real = transform(Div(this, that))
  def unary_-(): Real = transform(Neg(this))
  def sqrt: Real = this nroot 2
  def nroot(k: Int): Real = transform(KRoot(this, k))

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

  def toDouble: Double = this approximateTo Double
  def toBigDecimal(implicit mc: MathContext): BigDecimal = this approximateTo mc

  def approximateTo[A,B](a: A)(implicit approx: Approximation[Real,A,B]): B =
    approx(this, a)

  /**
   * Returns an absolute approximation to `this` s.t.
   * `this - err <= this +/- err <= this + err`.
   */
  def +/-(err: BigDecimal): BigDecimal = {
    import Bounded._

    val mc = new MathContext(max(this.decimalUpperBound + err.scale, 0))
    this approximateTo mc
  }

  def signum: Int = sign.toInt

  // The sign of this `Real`.
  def sign: Sign = {
    import Bounded._

    // Note: this +/- 10^k -> max(ub - k, 0) digits.

    val ub = this.decimalUpperBound
    val sep = BigDecimal(1, -this.decimalLowerBound)

    def findSign(scale: Int): Int = {
      val err = BigDecimal(1, scale)
      val digits = max(ub + scale, 0)
      val a = this approximateTo (new MathContext(digits))
      if (a.abs > err) {
        a.signum
      } else if (2 * err <= sep) {
        0
      } else {
        findSign(scale + 1)
      }
    }

    Sign(findSign(0))
  }

  /*
    val err = 8
    val init = this approximateTo AbsoluteError(8)
    val lb = BigDecimal(2) pow this.lowerBound

    val bound = Bounded(this)
    val lb = bound.lowerBound
    var prevErr = Rational(2)
    var currErr = Rational.one

    while (true) {
      val rat = this.toRational(e)
      if (rat.abs > e) {
        return if (rat.signum > 0) Positive else Negative
      } else if (prevErr <= lb) {
        return Zero
      } else {
        prevErr = currErr
        currErr = prevErr / 2
      }
    }
  }
  */
}


object Real {
  private val t = DivBubbleTransformer
  def transform(num: Real): Real = t.transform(num)

  implicit def apply(n: Int): Real = IntLit(n)
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
case class KRoot(a: Real, k: Int) extends Real {
  require(k >= 2, "Only positive roots greater than 2 supported.")
}
case class IntLit(value: Int) extends Real {
  override def sign: Sign = if (value == 0) Zero else if (value > 0) Positive else Negative
}



trait Transformer[A] {
  def transform(a: A): A
}

case class AggregateTransformer[A](t1: Transformer[A], t2: Transformer[A]) extends Transformer[A] {
  def transform(a: A): A = t2.transform(t1.transform(a))
}

/**
 * Moves division up from the children of a Real to the root. For example,
 * `a/b + c/d` becomes `(a*d + c*b) / (b*d)`.
 */
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


