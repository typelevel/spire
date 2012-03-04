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

  implicit object Absolute extends Approximation[Real,BigDecimal,BigDecimal] {
    // TODO: Does "scale" always guarantee 10^-scale as a lower bound of err?
    def apply(num: Real, err: BigDecimal): BigDecimal = AbsApprox(num, err.scale).value
  }

  implicit object Relative extends Approximation[Real,MathContext,BigDecimal] {
    def apply(num: Real, mc: MathContext): BigDecimal = BigDecimal(RelApprox(num, mc).value)
  }


  /**
   * An absolute approximation to a `Real` number using `java.math.BigDecimal`.
   */
  trait AbsApprox {
    def bits: Int   // Not really bits, but base 10.
    def value: BigDecimal
  }

  object AbsApprox extends Approximation[Real,Int,AbsApprox] {
    def apply(num: Real, bits: Int): AbsApprox = num match {
      case Add(lhs, rhs) => AddAbsApprox(lhs, rhs, bits)
      case Sub(lhs, rhs) => SubAbsApprox(lhs, rhs, bits)
      case Mul(lhs, rhs) => MulAbsApprox(lhs, rhs, bits)
      case Div(lhs, rhs) => DivAbsApprox(lhs, rhs, bits)
      case Neg(x) => NegAbsApprox(x, bits)
      case KRoot(x, k) => KRootAbsApprox(x, k, bits)
      case IntLit(x) => IntLitAbsApprox(x, bits)
      case BigIntLit(x) => BigIntLitAbsApprox(x, bits)
      case _ => throw new IllegalArgumentException()
    }
  }

  case class AddAbsApprox(a: Real, b: Real, bits: Int) extends AbsApprox {
    lazy val lhs: AbsApprox = AbsApprox(a, bits + 1)
    lazy val rhs: AbsApprox = AbsApprox(b, bits + 1)

    def value = lhs.value + rhs.value
  }

  case class SubAbsApprox(a: Real, b: Real, bits: Int) extends AbsApprox {
    lazy val lhs: AbsApprox = AbsApprox(a, bits + 1)
    lazy val rhs: AbsApprox = AbsApprox(b, bits + 1)

    def value = lhs.value - rhs.value
  }

  case class MulAbsApprox(a: Real, b: Real, bits: Int) extends AbsApprox {
    import Bounded._

    private def ae = (bits + 2) / 2
    private def be = bits + 2 - ae

    lazy val lhs: AbsApprox = AbsApprox(a, max(ae, bits + 1 + b.decimalUpperBound))
    lazy val rhs: AbsApprox = AbsApprox(b, max(be, bits + 1 + a.decimalUpperBound))

    def value = lhs.value * rhs.value
  }


  case class DivAbsApprox(a: Real, b: Real, bits: Int) extends AbsApprox {
    import Bounded._

    private def ae = (bits + 2) / 2
    private def be = bits + 2 - ae

    lazy val lhs: AbsApprox = AbsApprox(a, bits + 2 - b.decimalLowerBound)
    lazy val rhs: AbsApprox = AbsApprox(b, max(
          1 - b.decimalLowerBound,
          bits + 2 - 2 * b.decimalLowerBound + a.decimalUpperBound))

    def value: BigDecimal = if (b.sign == Zero) {
      throw new ArithmeticException("/ by zero")
    } else {
      val ub = Div(a, b).decimalUpperBound
      val mc = new MathContext(ub + bits + 1) // bits + 1 absolute digits.
      BigDecimal(lhs.value.bigDecimal.divide(rhs.value.bigDecimal, mc))
    }
  }

  case class NegAbsApprox(a: Real, bits: Int) extends AbsApprox {
    lazy val x: AbsApprox = AbsApprox(a, bits)
    def value = -(x.value)
  }


  case class KRootAbsApprox(a: Real, k: Int, bits: Int) extends AbsApprox {
    import Implicits._
    import Bounded._

    lazy val x: AbsApprox = AbsApprox(a, max(bits + 1, 1 - a.decimalLowerBound / 2))

    def value: BigDecimal = {
      // We need to use the upper bound to determine how many bits we need.
      val ub = KRoot(a, k).decimalUpperBound
      implicit val mc = new MathContext(ub + bits + 1)
      x.value nroot k
    }
  }

  case class IntLitAbsApprox(n: Int, bits: Int) extends AbsApprox {
    // TODO: Worth approximating n???
    val value = BigDecimal(n)
  }

  case class BigIntLitAbsApprox(n: BigInt, bits: Int) extends AbsApprox {
    // TODO: Worth approximating n???
    val value = BigDecimal(n)
  }


  trait RelApprox {
    implicit def mathContextOps(omc: MathContext) = new MathContextOps(omc)

    def mc: MathContext
    def value: BigDec
  }


  object RelApprox extends Approximation[Real,MathContext,RelApprox] {
    def apply(n: Real, mc: MathContext): RelApprox = n match {
      case n: Add => AddRelApprox(n, mc)
      case n: Sub => SubRelApprox(n, mc)
      case n: Mul => MulRelApprox(n, mc)
      case n: Div => DivRelApprox(n, mc)
      case n: Neg => NegRelApprox(n, mc)
      case n: KRoot => KRootRelApprox(n, mc)
      case IntLit(n) => IntLitRelApprox(n, mc)
      case BigIntLit(n) => BigIntLitRelApprox(n, mc)
    }
  }

  case class AddRelApprox(x: Add, mc: MathContext) extends RelApprox {
    import Bounded._

    lazy val lhs: RelApprox = RelApprox(x.lhs, mc + 1 - x.decimalLowerBound)
    lazy val rhs: RelApprox = RelApprox(x.rhs, mc + 1 - x.decimalLowerBound)

    // The lowerbound is only valid if x != 0. So, we need to check if x == 0.

    def value: BigDec = if (x.sign == Zero) {
      new BigDec(0)
    } else {
      lhs.value.add(rhs.value, mc)
    }
  }

  case class SubRelApprox(x: Sub, mc: MathContext) extends RelApprox {
    import Bounded._

    lazy val lhs: RelApprox = RelApprox(x.lhs, mc + 1 - x.decimalLowerBound)
    lazy val rhs: RelApprox = RelApprox(x.rhs, mc + 1 - x.decimalLowerBound)

    def value: BigDec = if (x.sign == Zero) {
      new BigDec(0)
    } else {
      lhs.value.subtract(rhs.value, mc)
    }
  }

  case class MulRelApprox(x: Mul, mc: MathContext) extends RelApprox {
    lazy val lhs = RelApprox(x.lhs, mc + 1)
    lazy val rhs = RelApprox(x.rhs, mc + 2) // Implement heuristic for cost?

    def value: BigDec = lhs.value.multiply(rhs.value, mc)
  }

  case class DivRelApprox(x: Div, mc: MathContext) extends RelApprox {
    lazy val lhs = RelApprox(x.lhs, mc + 2)
    lazy val rhs = RelApprox(x.rhs, mc + 2)
    def value: BigDec = lhs.value.divide(rhs.value, mc + 2)
  }

  case class NegRelApprox(x: Neg, mc: MathContext) extends RelApprox {
    lazy val approx = RelApprox(x.a, mc)
    def value: BigDec = approx.value.negate()
  }

  case class KRootRelApprox(x: KRoot, mc: MathContext) extends RelApprox {
    import Implicits._

    lazy val approx = RelApprox(x.a, mc + 1)
    def value: BigDec = {
      implicit val ctxt = mc + 1
      (BigDecimal(approx.value) nroot x.k).bigDecimal
    }
  }

  case class IntLitRelApprox(n: Int, mc: MathContext) extends RelApprox {
    val value = BigDecimal(n, mc).bigDecimal
  }

  case class BigIntLitRelApprox(n: BigInt, mc: MathContext) extends RelApprox {
    val value = BigDecimal(n, mc).bigDecimal
  }
}

