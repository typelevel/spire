package numerics.math

import java.math.{ MathContext, BigDecimal => BigDec }
import scala.math.max
import BigDecimalApproximations.{ AbsApprox => BigDecAbsApprox }


/** 
 * A typeclass approach for getting an approximation to an `A` using error `B`
 * in type `C`.
 */
trait Approximation[A,B,C] extends ((A,B) => C) {
  def apply(n: A, err: B): C
}

object Approximation {
  def approximate[A,B,C](a: A, b: B)(implicit approx: Approximation[A,B,C]): C =
    approx(a, b)

  // implicit val absolutePrecision = AbsoluteApprox
  // implicit val absoluteApproximation = AbsoluteApproximation

  implicit object BigDecimalApproximation extends Approximation[Real,MathContext,BigDecimal] {
    def apply(n: Real, mc: MathContext): BigDecimal =
      BigDecimal(BigDecAbsApprox(n, mc).value)
  }

  implicit object DoubleApproximation extends Approximation[Real,Double.type,Double] {
    def apply(n: Real, err: Double.type): Double = {
      val bd: BigDecimal = approximate(n, 17.digits)
      bd.toDouble
    }
  }



  implicit def int2error(k: Int): ApproximationContextBuilder = new ApproximationContextBuilder(k)
  final class ApproximationContextBuilder(k: Int) {
    // def bits: AbsolutePrecision = AbsolutePrecision(k)
    def digits: MathContext = new MathContext(k)
  }
}


/*
case class AbsoluteError(k: Int)

object AbsoluteApproximation extends Approximation[Real,AbsoluteError,BigDecimal] {
  import Approximation._
  import Bounded._

  def apply(n: Real, err: AbsoluteError): BigDecimal = {
    approximate(n, (n.upperBound - err.k) bits)
  }
}


case class AbsolutePrecision(bits: Int)
*/



/*
Transform a Real into an AbsoluteApprox
Real => AbsoluteApprox

If we already have 1 approx
AbsoluteApproximation => AbsoluteApproximation

The entire point of constructing a tree for the absolute approximation is so
that previous results can be used to speed up a recomputation of a better
approx. For example, in the separation bound.
*/
/*
trait AbsoluteApprox {
  def err: Int
  def value: BigDec
}

object AbsoluteApprox extends Approximation[Real,AbsolutePrecision,BigDecimal] {
  def apply(n: Real, err: AbsolutePrecision): BigDecimal =
    BigDecimal(apply(n, err.bits).value)

  def apply(num: Real, err: Int): AbsoluteApprox = num match {
    case Add(lhs, rhs) => AddApprox(lhs, rhs, err)
    case Sub(lhs, rhs) => SubApprox(lhs, rhs, err)
    case Mul(lhs, rhs) => MulApprox(lhs, rhs, err)
    case Div(lhs, rhs) => DivApprox(lhs, rhs, err)
    case Neg(x) => NegApprox(x, err)
    case KRoot(x, k) => KRootApprox(x, k, err)
    case IntLit(x) => IntLitApprox(x, err)
    case _ => throw new IllegalArgumentException()
  }


  // For changing the radix. Perhaps just replace with an approximation? Like
  // 4/13 or 7/23.
  private val bits2digits = math.log(2) / math.log(10)
  def mc(bits: Int) = new MathContext(math.ceil(bits * bits2digits).toInt)
}

case class AddApprox(a: Real, b: Real, err: Int) extends AbsoluteApprox {
  import AbsoluteApprox.mc

  lazy val lhs: AbsoluteApprox = AbsoluteApprox(a, err + 1)
  lazy val rhs: AbsoluteApprox = AbsoluteApprox(b, err + 1)

  // Important: a.plus(b, mc) rounds using mc AFTER the add.
  def value: BigDec = lhs.value.add(rhs.value, mc(err))
}

case class SubApprox(a: Real, b: Real, err: Int) extends AbsoluteApprox {
  import AbsoluteApprox.mc

  lazy val lhs: AbsoluteApprox = AbsoluteApprox(a, err + 1)
  lazy val rhs: AbsoluteApprox = AbsoluteApprox(b, err + 1)

  // Important: a.plus(b, mc) rounds using mc AFTER the add.
  def value: BigDec = lhs.value.subtract(rhs.value, mc(err))
}

case class MulApprox(a: Real, b: Real, err: Int) extends AbsoluteApprox {
  import AbsoluteApprox.mc
  import Bounded._

  private def ae = (err + 2) / 2
  private def be = err + 2 - ae

  lazy val lhs: AbsoluteApprox = AbsoluteApprox(a, math.max(ae, err + 1 + a.upperBound))
  lazy val rhs: AbsoluteApprox = AbsoluteApprox(b, math.max(be, err + 1 + b.upperBound))

  def value: BigDec = lhs.value.multiply(rhs.value, mc(err))
}


case class DivApprox(a: Real, b: Real, err: Int) extends AbsoluteApprox {
  import AbsoluteApprox.mc
  import Bounded._

  private def ae = (err + 2) / 2
  private def be = err + 2 - ae

  lazy val lhs: AbsoluteApprox = AbsoluteApprox(a, err + 2 - b.lowerBound)
  lazy val rhs: AbsoluteApprox
    = AbsoluteApprox(b, math.max(1 - b.lowerBound, err + 2 - 2 * b.lowerBound + a.upperBound))

  def value: BigDec = if (b.sign == Zero) {
    throw new ArithmeticException("/ by zero")
  } else {
    lhs.value.divide(rhs.value, mc(err + 1))
  }
}

case class NegApprox(a: Real, err: Int) extends AbsoluteApprox {
  lazy val x: AbsoluteApprox = AbsoluteApprox(a, err)
  def value: BigDec = x.value.negate()
}

case class KRootApprox(a: Real, k: Int, err: Int) extends AbsoluteApprox {
  import AbsoluteApprox.mc
  import Implicits._
  import Bounded._

  lazy val x: AbsoluteApprox = AbsoluteApprox(a, max(err + 1, 1 - a.lowerBound / 2))

  def value: BigDec = {
    implicit val ctxt = mc(err + 1)
    (BigDecimal(x.value) nroot k).bigDecimal
  }
}

case class IntLitApprox(n: Int, err: Int) extends AbsoluteApprox {
  val value = BigDecimal(n).bigDecimal
}
*/
