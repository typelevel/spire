package spire.math.fpf

import spire.algebra.Sign
import spire.algebra.Sign.{ Positive, Negative, Zero }
import spire.math._

import scala.math.{ max, abs }
import java.lang.Double.{ NaN, isNaN, isInfinite }


/**
 * A `MaybeDouble` will hold a `Double` approximation so long as the `Double`'s
 * sign can be computed exactly. It is not a general number type, but is meant
 * to be used in tandem with a more accurate, but slow (computationally),
 * number type. When performing comparisons then, this can be checked first to
 * save on potentially slow computation.
 *
 * For this type, if a method returns an `Option`al value, then that indicates
 * that if `None` is returned, the answer cannot be computed exactly, but if
 * `Some(x)` is returned, then that is guaranteed to be correct. For example,
 * `toLong`, `toFloat`, `sign`, `isWhole`, etc. return these types of optional,
 * only-if-correct, type values.
 *
 * Most likely you would not use this directly, but just wrap your number type
 * in a `FPFilter` which maintains a `MaybeDouble` and handles all the lazy
 * computation of the more accurate number type for you.
 */
final class MaybeDouble(val approx: Double, private val mes: Double, private val ind: Int) {
  import MaybeDouble._

  lazy val error: Double = mes * ind * eps
  
  private def invalid: Boolean = isNaN(approx) || isInfinite(approx)

  /**
   * Sometimes a number is too large to fit in a `Double`, the result of an
   * operation cannot be computed (eg. sqrt of a negative number), other times
   * we just give up when trying to keep track of the error... In any case, the
   * `approx` is set to either `NaN` or an `Infinity`, and it will be marked
   * as invalid. In that case, this will return `false` otherwise it will
   * return `true`.
   */
  def isValid: Boolean = !invalid


  def abs: MaybeDouble = new MaybeDouble(math.abs(approx), mes, ind)

  def unary_-(): MaybeDouble = new MaybeDouble(-approx, mes, ind)

  def +(that: MaybeDouble): MaybeDouble =
    new MaybeDouble(approx + that.approx, mes + that.mes, max(ind, that.ind) + 1)

  def -(that: MaybeDouble): MaybeDouble =
    new MaybeDouble(approx - that.approx, mes + that.mes, max(ind, that.ind) + 1)

  def *(that: MaybeDouble): MaybeDouble =
    new MaybeDouble(approx * that.approx, mes * that.mes, ind + that.ind + 1)

  def /(that: MaybeDouble): MaybeDouble =
    new MaybeDouble(approx / that.approx, 
             (math.abs(approx) / math.abs(that.approx) + (mes / that.mes)) /
                (math.abs(that.approx) / that.mes - (that.ind + 1) * eps),
             1 + max(ind, that.ind + 1))
    
  def pow(k: Int): MaybeDouble = if (k > 0) {
    new MaybeDouble(math.pow(approx, k), math.pow(mes, k), ind + k)
  } else if (k == 0) {
    new MaybeDouble(1.0, 1.0, 0)
  } else {

    // In this case, we just do a positive pow, and divide 1 by it.
    
    val b = this.pow(-k)
    val mes = (1 / math.abs(b.approx) + 1 / b.mes) / (math.abs(b.approx) / b.mes - (b.mes + 1) * eps)
    new MaybeDouble(1 / b.approx, mes, b.ind + 2)
  }

  /**
   * FIXME: Implement arbitrary nroots w/ error bounds.
   */
  def nroot(n: Int): MaybeDouble = if (n == 2) sqrt else Invalid
   
  def sqrt: MaybeDouble = {
    val x = math.sqrt(approx)

    if (invalid || approx <= 0.0) {

      // If a < 0, then sqrt(a) == NaN, which is OK for now.
      
      new MaybeDouble(x, math.sqrt(mes) * (1 << 26), ind + 1)
    } else {
      new MaybeDouble(x, (mes / approx) * x, ind + 1)
    }
  }

  /**
   * Currently, if this can't be answered exactly, then the result is made
   * invalid. This should be fixed. If the answer is exact, then it is returned
   * with a 0 error bound.
   */
  def quot(that: MaybeDouble): MaybeDouble = {
    val quot = this / that
    val ub = quot.approx + quot.error
    val lb = quot.approx - quot.error

    if ((lb >= minWhole) && (ub <= maxWhole) && (ub.toLong == lb.toLong)) {
      val i = quot.approx.toLong
      new MaybeDouble(i.toDouble, 0.0, 0)
    } else {
      Invalid
    }
  }

  /**
   * This implements the modulus operator. It will behave similar to % for
   * Double, Long, Int, etc.
   */
  def mod(that: MaybeDouble): MaybeDouble = {
    val a = this.abs
    val b = that.abs
    val absrem = (a - b * (a quot b))
    if (approx < 0) -absrem else absrem
  }


  /**
   * If this `MaybeDouble`'s sign can be computed exactly, then `Some(s)` is
   * returned, where `s` is the sign of the number. Otherwise, `None` is
   * returned.
   */
  val sign: Option[Sign] = if (invalid) None else {
    if (approx > error)
      Some(Positive)
    else if (approx < -error)
      Some(Negative)
    else if (error == 0.0)
      Some(Zero)
    else
      None
  }
  

  /**
   * If this `MaybeDouble`'s sign can be computed exactly, then `Some(x)` is
   * returned where `x` is the `Double` approximation. Otherwise, `None` is
   * returned.
   */
  def opt: Option[Double] = sign map (_ => approx)

  def toInt: Option[Int] = toLong map (_.toInt)

  /**
   * If this can be converted, exactly, into a `Long`, then `Some(n)` will be
   * returned, where `n` is a `Long`. Otherwise, `None` will be returned.
   */
  def toLong: Option[Long] = if (isValid && approx <= maxWhole && approx >= minWhole) {
    val lb = (approx - error).toLong
    val ub = (approx + error).toLong
    if (lb == ub) Some(lb) else None
  } else None

  def toBigInt: Option[BigInt] = toLong map (BigInt(_))

  /**
   * Returns `true` if this `MaybeDouble` is both valid and exact (has an
   * `error` of 0.0).
   */
  def isExact: Boolean = !invalid && error == 0.0

  /**
   * Returns `Some(f)`, where `f` is a `Float`, if `f` is known to be exact.
   * Otherwise, returns `None`.
   */
  def toFloat: Option[Float] =
    if (approx >= Float.MinValue && approx <= Float.MaxValue) {
      val lb = (approx - error).toFloat
      val ub = (approx + error).toFloat
      if (lb == ub) Some(lb) else None
    } else {
      None
    }

  def toDouble: Option[Double] = if (isExact) Some(approx) else None

  def isWhole: Option[Boolean] = if (isExact) {
    toLong map { _.toDouble == approx }
  } else None

  private def consensus(f: Double => Double) = if (isValid) {
    val x = f(approx - error)
    if (x == f(approx + error)) MaybeDouble(x) else MaybeDouble.Invalid
  } else MaybeDouble.Invalid

  def ceil: MaybeDouble = consensus(spire.math.ceil)
  def floor: MaybeDouble = consensus(spire.math.floor)
  def round: MaybeDouble = consensus(spire.math.round)

  override def hashCode: Int = (approx.## + mes.## * 19 + ind * 23)
  override def equals(that: Any): Boolean = that match {
    case that: MaybeDouble =>
      this.approx == that.approx && this.mes == that.mes && this.ind == that.ind
    case _ => false
  }

  override def toString: String = "~" + approx.toString
}


object MaybeDouble {

  // The min relative error, aka 2^{-p} in [1].
  
  val eps = java.lang.Double.longBitsToDouble((1023L - 52) << 52)
  val feps = java.lang.Float.intBitsToFloat((127 - 23) << 23)

  // Max and min integers that Double can represent accurately.

  val maxWhole = java.lang.Double.longBitsToDouble(((-1L) >>> 12) | (1075L << 52))
  val minWhole = -maxWhole

  val Invalid = new MaybeDouble(NaN, 0, 0)


  def apply(x: Float): MaybeDouble = exact(x.toDouble)
  def apply(x: Double): MaybeDouble = exact(x)
  def apply(x: Int): MaybeDouble = exact(x)
  def apply(x: Long): MaybeDouble = if (x >= minWhole && x <= maxWhole) {
    exact(x.toDouble)
  } else {
    approx(x.toDouble)
  }
  def apply(x: BigInt): MaybeDouble = {
    val y = x.toDouble
    
    if (y >= minWhole && y <= maxWhole) exact(y) else approx(y)
  }
  def apply(x: Rational): MaybeDouble = approx(x.toDouble)
  def apply(x: BigDecimal): MaybeDouble = approx(x.toDouble)

  def exact(x: Double): MaybeDouble = new MaybeDouble(x, abs(x), 0)
  def approx(x: Double): MaybeDouble = new MaybeDouble(x, abs(x), 1)
}


