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

  implicit val absBigDecimalApproximation = BigDecimalApproximations.Absolute
  implicit val relBigDecimalApproximation = BigDecimalApproximations.Relative

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

