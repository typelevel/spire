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
package spire.math

import spire.algebra.{ Zero }
import java.math.{ MathContext, BigInteger, BigDecimal => BigDec }
import scala.math.{ ScalaNumber, ScalaNumericConversions, max }

import real._


/**
 * An general Real type. Can be used represent real numbers and approximate
 * them on-demand.
 */
final class Real private (val expr: Expr[Real])
extends ScalaNumber with ScalaNumericConversions
   with RealLike[Real]
   with BMFSSBound[Real]
   with BigDecimalApprox[Real]
   with FPFilter[Real]
   with ConstantFolder[Real]
   with BubbleUpDivs[Real]
   with PrettyToString[Real]
   with Ordered[Real] {

  val coexpr: Coexpr[Real] = Real.RealCoexpr

  // ugh
  override def byteValue(): Byte = fpf.toLong map (_.toByte) getOrElse super.toByte
  override def shortValue(): Short = fpf.toLong map (_.toShort) getOrElse super.toShort

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

  override def hashCode: Int = if (isWhole && toBigInt == toLong) {
    unifiedPrimitiveHashcode
  } else {
    val x = toBigDecimal(java.math.MathContext.DECIMAL64)
    x.underlying.unscaledValue.hashCode + 23 * x.scale.hashCode + 17
  }
}


object Real {

  implicit def apply(n: Int): Real = Expr(n)
  implicit def apply(n: Long): Real = Expr(n)
  implicit def apply(n: BigInt): Real = Expr(n)
  implicit def apply(n: Rational): Real = Expr(n)
  implicit def apply(n: Double): Real = Expr(n)
  implicit def apply(n: BigDecimal): Real = Expr(n)


  implicit object RealCoexpr extends Coexpr[Real] {
    def expr(r: Real): Expr[Real] = r.expr
    def coexpr(e: Expr[Real]): Real = new Real(e)
  }
}

