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

import spire.algebra._
import spire.algebra.Sign.{ Positive, Negative, Zero }
import java.math.{ MathContext, BigInteger, BigDecimal => BigDec }
import scala.math.{ ScalaNumber, ScalaNumericConversions }

import algebraic._


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
    case that: Algebraic => (this - that).sign == Zero
    case that: Rational => (this - Algebraic(that)).sign == Zero
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
  override def eqv(x: Algebraic, y: Algebraic) = (x - y).sign == Zero
  override def neqv(x: Algebraic, y: Algebraic) = (x - y).sign != Zero
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
