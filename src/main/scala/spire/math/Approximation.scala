package spire.math

//import language.implicitConversions
import java.math.{ MathContext, BigDecimal => BigDec }
import scala.math.max
import spire.math.real.BigDecimalApproximations

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

  // We could also, instead, just use `Absolute` and `Relative` as they are in
  // `BigDecimalApproximations`, but then all types that are
  // `RealLike[A] with SeparationBound[A]` would have to compete for implicit
  // priority if they wanted to define their own `Approximation`s.

  implicit val absBigDecimalApproximation = BigDecimalApproximations.Absolute[Real]
  implicit val relBigDecimalApproximation = BigDecimalApproximations.Relative[Real]

  implicit object DoubleApproximation extends Approximation[Real,Double.type,Double] {
    def apply(n: Real, err: Double.type): Double = {
      val bd: BigDecimal = approximate(n, new MathContext(17))
      bd.toDouble
    }
  }


  implicit def int2error(k: Int): ApproximationContextBuilder = new ApproximationContextBuilder(k)
  final class ApproximationContextBuilder(k: Int) {
    // def bits: AbsolutePrecision = AbsolutePrecision(k)
    def digits: MathContext = new MathContext(k)
  }

  final class ApproximationOps[A](a: A) {
    def approximateTo[B,C](b: B)(implicit approx: Approximation[A,B,C]): C = approx(a, b)
  }

  implicit def approximateAny[A](a: A) = new ApproximationOps(a)
}

