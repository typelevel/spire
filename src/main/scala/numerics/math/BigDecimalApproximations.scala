package numerics.math

import java.math.{ MathContext, BigDecimal => BigDec }
import scala.math.max

/**
 * Provides `Real` approximations to `BigDecimal` using `MathContext` to
 * bound the approximations.
 *
 * Here, we mostly work with `java.math.BigDecimal` as its operations let you
 * provide a `MathContext` directly. However, I'm pretty sure that
 * `a.add(b, mc)` is equivalent to `(a + b).round(mc)`.
 */
object BigDecimalApproximations {

  /**
   * Removes some of the tedium when dealing manipulating the precision of
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


  /**
   * An absolute approximation to a `Real` number using `java.math.BigDecimal`.
   */
  trait AbsApprox {
    implicit def mathContextOps(omc: MathContext) = new MathContextOps(omc)

    def mc: MathContext
    def value: BigDec
  }

  object AbsApprox extends Approximation[Real,MathContext,AbsApprox] {
    def apply(num: Real, mc: MathContext): AbsApprox = num match {
      case Add(lhs, rhs) => AddAbsApprox(lhs, rhs, mc)
      case Sub(lhs, rhs) => SubAbsApprox(lhs, rhs, mc)
      case Mul(lhs, rhs) => MulAbsApprox(lhs, rhs, mc)
      case Div(lhs, rhs) => DivAbsApprox(lhs, rhs, mc)
      case Neg(x) => NegAbsApprox(x, mc)
      case KRoot(x, k) => KRootAbsApprox(x, k, mc)
      case IntLit(x) => IntLitAbsApprox(x, mc)
      case BigIntLit(x) => BigIntLitAbsApprox(x, mc)
      case _ => throw new IllegalArgumentException()
    }
  }

  case class AddAbsApprox(a: Real, b: Real, mc: MathContext) extends AbsApprox {
    lazy val lhs: AbsApprox = AbsApprox(a, mc + 1)
    lazy val rhs: AbsApprox = AbsApprox(b, mc + 1)

    def value: BigDec = lhs.value.add(rhs.value, mc)
  }

  case class SubAbsApprox(a: Real, b: Real, mc: MathContext) extends AbsApprox {
    lazy val lhs: AbsApprox = AbsApprox(a, mc + 1)
    lazy val rhs: AbsApprox = AbsApprox(b, mc + 1)

    def value: BigDec = lhs.value.subtract(rhs.value, mc)
  }

  case class MulAbsApprox(a: Real, b: Real, mc: MathContext) extends AbsApprox {
    import Bounded._

    private def ae = (mc + 2) / 2
    private def be = mc + 2 - ae.precision

    lazy val lhs: AbsApprox = AbsApprox(a, ae max (mc + 1 + a.decimalUpperBound))
    lazy val rhs: AbsApprox = AbsApprox(b, be max (mc + 1 + b.decimalUpperBound))

    def value: BigDec = lhs.value.multiply(rhs.value, mc)
  }


  case class DivAbsApprox(a: Real, b: Real, mc: MathContext) extends AbsApprox {
    import Bounded._

    private def ae = (mc + 2) / 2
    private def be = mc + 2 - ae.precision

    lazy val lhs: AbsApprox = AbsApprox(a, mc + 2 - b.decimalLowerBound)
    lazy val rhs: AbsApprox
      = AbsApprox(b, new MathContext(1 - b.decimalLowerBound, mc.roundingMode)
                       max
                     (mc + 2 - 2 * b.decimalLowerBound + a.decimalUpperBound))

    def value: BigDec = if (b.sign == Zero) {
      throw new ArithmeticException("/ by zero")
    } else {
      lhs.value.divide(rhs.value, mc + 1)
    }
  }

  case class NegAbsApprox(a: Real, mc: MathContext) extends AbsApprox {
    lazy val x: AbsApprox = AbsApprox(a, mc)
    def value: BigDec = x.value.negate()
  }

  case class KRootAbsApprox(a: Real, k: Int, mc: MathContext) extends AbsApprox {
    import Implicits._
    import Bounded._

    lazy val x: AbsApprox = AbsApprox(a, (mc + 1) max (1 - a.decimalLowerBound / 2))

    def value: BigDec = {
      implicit val ctxt = mc + 1
      (BigDecimal(x.value) nroot k).bigDecimal
    }
  }

  case class IntLitAbsApprox(n: Int, mc: MathContext) extends AbsApprox {
    // BigDecimal(n, mc) can/will cause "division impossible" exceptions.
    val value = BigDecimal(n).round(mc).bigDecimal
  }

  case class BigIntLitAbsApprox(n: BigInt, mc: MathContext) extends AbsApprox {
    val value = BigDecimal(n).round(mc).bigDecimal
  }


  /*
  sealed trait RelativeApprox {
    def value: BigDecimal
  }

  // object AbsoluteApproximation extends RelativeApproximation[Real,MathContext,BigDecimal] 

  case class MulRelApprox(a: Real, b: Real, digits: Int) {
    lazy val lhs = RelativeApprox(a, digits + 1)
    lazy val rhs = RelativeApprox(b, digits + 2)

    def value: BigDecimal = lhs.value.multiply(rhs.value, mc)
  }
  case class IntLitRelApprox(n: Int) extends RelativeApprox {
    val value = BigDecimal(n)
  }
  */
}

