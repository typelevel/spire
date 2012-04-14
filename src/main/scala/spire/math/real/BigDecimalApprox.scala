package spire.math.real

import spire.algebra._
import spire.math._
import java.math.MathContext

import Implicits._


trait BigDecimalApprox[A <: BigDecimalApprox[A]]
extends RealLike[A] with SeparationBound[A] { self: A =>
  import BigDecimalApproximations._

  def sign: Sign = this match {
    case IntLit(n) => Sign(n)
    case BigIntLit(n) => Sign(n.signum)
    case Neg(n) => -(n.sign)
    case Mul(a, b) => a.sign * b.sign
    case Div(a, b) => a.sign * b.sign
    case _ =>

      // The separation bound.
      val sep = BigDecimal(1, -this.decimalLowerBound)

      def findSign(scale: Int): Int = {
        val err = BigDecimal(1, scale)
        val a = this +/- err
        if (a.abs > err) {
          a.signum
        } else if (err * 2 < sep) {
          0
        } else {
          findSign(scale + 1)
        }
      }

      Sign(findSign(0))
  }


  def /~(that: A): A = BigIntLit((this / that).toBigInt)
  def %(that: A): A = this - (this /~ that) * that


  def toBigInt: BigInt = sign match {
    case Zero => BigInt(0)
    case Negative => -((-this).toBigInt)
    case Positive =>
      val a = this +/- 0.01
      val b = a.toBigInt

      if ((a + 0.02) >= BigDecimal(b + 1)) {
        (this - Expr[A](b + 1)).sign match {
          case Positive => b + 1
          case Negative => b
          case Zero => b + 1
        }
      } else if ((a - 0.02) < BigDecimal(b)) {
        (this - Expr[A](b)).sign match {
          case Positive => b
          case Negative => b - 1
          case Zero => b
        }
      } else {
        b
      }
  }

  def approximateTo[B,C](b: B)(implicit approx: Approximation[A,B,C]): C =
    approx(this, b)

  /**
   * Returns an absolute approximation to `this` s.t.
   * `this - err <= this +/- err <= this + err`.
   */
  def +/-(err: BigDecimal): BigDecimal = this approximateTo err

  def toBigDecimal(implicit mc: MathContext = MathContext.DECIMAL128): BigDecimal =
    (this approximateTo mc).round(mc)

  /**
   * Simulate returns the result of the `Real` if it had instead been replaced
   * by the type `A`. So, this won't provide any type
   */
  def simulate[B](implicit f: FractionalWithNRoot[B]): B = this match {
    case IntLit(n) => f.fromInt(n)
    case BigIntLit(n) => f.fromBigInt(n)
    case Add(a, b) => a.simulate[B] + b.simulate[B]
    case Sub(a, b) => a.simulate[B] - b.simulate[B]
    case Mul(a, b) => a.simulate[B] * b.simulate[B]
    case Div(a, b) => a.simulate[B] / b.simulate[B]
    case KRoot(a, k) => a.simulate[B] nroot k
    case Neg(a) => -(a.simulate[B])
  }

  def toRational(implicit ac: ApproximationContext[Rational] = ApproximationContext(Rational(1L, 10000000000000000L))): Rational = simulate[Rational]

  def isWhole: Boolean = (this % IntLit(1)).sign == Zero

  def underlying: AnyRef = this   // Why not?

  def doubleValue: Double = (this approximateTo (new MathContext(17))).toDouble
  def floatValue: Float = doubleValue.toFloat
  def intValue: Int = toBigInt.toInt
  def longValue: Long = toBigInt.toLong

  override def toString: String = {
    val approx = this.toBigDecimal(new MathContext(9))
    val prefix = if (this == Expr[A](approx)) "" else "~"
    prefix + approx.toString
  }
}



