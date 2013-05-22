package spire.math.real

import java.math.{ MathContext, BigDecimal => BigDec }
import scala.math.max

import spire.algebra.{ NRoot }
import spire.algebra.Sign.{ Positive, Zero, Negative }
import spire.math._

/**
 * Provides absolute and relative approximations to `RealLike` types that have
 * mixed in a `SeparationBound`. The absolute approximations take 
 * `BigDecimal` for their context, returning a `BigDecimal` that is equal to
 * the `RealLike` +/- the context (error). The relative approximations take a
 * `MathContext` specifying how many digits to determine the value of the
 * `RealLike` to.
 *
 * Note that a relative approximation of 0 is always 0.
 *
 * Here, we mostly work with `java.math.BigDecimal` as its operations let you
 * provide a `MathContext` directly. However, I'm pretty sure that
 * `a.add(b, mc)` is equivalent to `(a + b).round(mc)`.
 */
object BigDecimalApproximations {

  /**
   * Removes some of the tedium when dealing with manipulating the precision of
   * `MathContext`s.
   */
  final class MathContextOps(mc: MathContext) {
    def roundingMode: java.math.RoundingMode = mc.getRoundingMode
    def precision: Int = mc.getPrecision
    def +(x: Int): MathContext = new MathContext(precision + x, roundingMode)
    def -(x: Int): MathContext = new MathContext(precision - x, roundingMode)
    def *(x: Int): MathContext = new MathContext(precision * x, roundingMode)
    def /(x: Int): MathContext = new MathContext(precision / x, roundingMode)
    def min(x: Int): MathContext = new MathContext(math.min(precision, x), roundingMode)
    def max(x: Int): MathContext = new MathContext(math.max(precision, x), roundingMode)

    def min(omc: MathContext): MathContext = this min omc.getPrecision
    def max(omc: MathContext): MathContext = this max omc.getPrecision
  }


  implicit def mathContextOps(omc: MathContext) = new MathContextOps(omc)


  implicit def Absolute[A <: RealLike[A] with SeparationBound[A]: Coexpr] = new Absolute[A]

  class Absolute[A <: RealLike[A] with SeparationBound[A]: Coexpr]
  extends Approximation[A, BigDecimal, BigDecimal] {
    def apply(a: A, err: BigDecimal): BigDecimal = {
      
      // TODO: Does "scale" always guarantee 10^-scale as a lower bound of err?

      apply(a, err.scale)
    }

    def apply(a: A, bits: Int): BigDecimal = a match {
      case IntLit(n) => BigDecimal(n)
      
      case BigIntLit(n) => BigDecimal(n)
      
      case Neg(a) => -apply(a, bits)
      
      case Add(a, b) => apply(a, bits + 1) + apply(b, bits + 1)
      
      case Sub(a, b) => apply(a, bits + 1) - apply(b, bits + 1)

      case Mul(a, b) =>
        val ae = bits / 2 + 1
        val be = bits + 2 - ae
        val lhs = apply(a, max(ae, bits + 1 + b.decimalUpperBound))
        val rhs = apply(b, max(be, bits + 1 + a.decimalUpperBound))
        lhs * rhs

      case e @ Div(a, b) =>
        if (b.sign == Zero) {
          throw new ArithmeticException("/ by zero")
        } else {
          val ae = (bits + 2) / 2
          val be = bits + 2 - ae

          val lhs = apply(a, bits + 2 - b.decimalLowerBound)
          val rhs = apply(b, max(
                1 - b.decimalLowerBound,
                bits + 2 - 2 * b.decimalLowerBound + a.decimalUpperBound))

          val ub = e.decimalUpperBound
          val mc = new MathContext(ub + bits + 1) // bits + 1 absolute digits.
          BigDecimal(lhs.bigDecimal.divide(rhs.bigDecimal, mc))
        }

      case e @ KRoot(a, k) =>
        import spire.implicits._

        val x = apply(a, max(bits + 1, 1 - a.decimalLowerBound / 2))

        // We need to use the upper bound to determine how many bits we need.
        
        val ub = e.decimalUpperBound
        
        if (x < 0 && k % 2 == 0 && a.sign == Zero) {
          BigDecimal(0)
        } else {
          NRoot.nroot(x, k, new MathContext(ub + bits + 1))
        }
    }
  }


  implicit def Relative[A <: RealLike[A] with SeparationBound[A]: Coexpr] = new Relative[A]


  class Relative[A <: RealLike[A] with SeparationBound[A]: Coexpr]
  extends Approximation[A, MathContext, BigDecimal] {
    
    def apply(n: A, mc: MathContext): BigDecimal = n match {
      case Add(a, b) =>
        if (n.sign == Zero) BigDecimal(0) else {
          val lhs = apply(a, mc + 1 - n.decimalLowerBound).bigDecimal
          val rhs = apply(b, mc + 1 - n.decimalLowerBound).bigDecimal
          BigDecimal(lhs.add(rhs, mc))
        }
      
      case Sub(a, b) =>
        if (n.sign == Zero) BigDecimal(0) else {
          val lhs = apply(a, mc + 1 - n.decimalLowerBound).bigDecimal
          val rhs = apply(b, mc + 1 - n.decimalLowerBound).bigDecimal
          BigDecimal(lhs.subtract(rhs, mc))
        }

      case Mul(a, b) =>

        // Implement heuristic for cost?
        
        val lhs = apply(a, mc + 1).bigDecimal
        val rhs = apply(b, mc + 2).bigDecimal
        BigDecimal(lhs.multiply(rhs, mc))

      case Div(a, b) =>
        val lhs = apply(a, mc + 2).bigDecimal
        val rhs = apply(b, mc + 2).bigDecimal
        BigDecimal(lhs.divide(rhs, mc + 2))

      case Neg(a) =>
        -apply(a, mc)

      case KRoot(a, k) => {
        import spire.implicits._

        val ctxt = mc + 1
        val sub = apply(a, ctxt)
        NRoot.nroot(sub, k, ctxt)
      }

      case IntLit(n) => BigDecimal(n, mc)
      case BigIntLit(n) => BigDecimal(n, mc)
    }
  }
}

