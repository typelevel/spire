package numerics.math.real

import numerics.math._

import java.math.MathContext

import scala.math.ScalaNumber


trait RealLike[A <: RealLike[A]] extends ScalaNumber { self: A =>
  implicit def coexpr: Coexpr[A]

  def sign: Sign
  def signum: Int = sign.toInt
  
  def compare(that: A): Int = (this - that).signum
  
  def abs: A = if (this.sign == Negative) -this else this

  def *(that: A): A = Mul(this, that)
  def +(that: A): A = Add(this, that)
  def -(that: A): A = Sub(this, that)
  def /(that: A): A = Div(this, that)
  def unary_-(): A = Neg[A](this)
  def sqrt: A = this nroot 2
  def nroot(k: Int): A = {
    if (this.sign == Negative && k % 2 == 0) {
      throw new ArithmeticException("Cannot find an even root of a negative Real.")
    }

    KRoot[A](this, k)
  }

  // TODO: Create Pow as a 1st class citizen.
  def pow(k: Int): A = if (k < 0) {
    IntLit[A](1) / (this pow -k)
  } else if (k == 0) {
    IntLit[A](1)
  } else if (k == 1) {
    this
  } else {
    val x = this pow (k / 2)
    val x2 = x * x
    if (k % 2 == 0) x2 else x2 * this
  }


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

  def isWhole: Boolean

  def toInt: Int
  def toLong: Long
  def toBigInt: BigInt
  def toFloat: Float
  def toDouble: Double
  def toBigDecimal(implicit mc: MathContext): BigDecimal
}


