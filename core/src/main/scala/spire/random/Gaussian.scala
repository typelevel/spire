package spire
package random

import java.math.MathContext

import spire.algebra.{Field, NRoot, Order, Trig}

trait Gaussian[@sp(Float, Double) A] extends Any {

  /**
   * Return an `A` that is normally distributed about `mean` with a standard
   * deviation of `stdDev`.
   */
  def apply(mean: A, stdDev: A): Dist[A]
}

object Gaussian extends GaussianInstances {
  @inline final def apply[@sp(Float, Double) A](implicit g: Gaussian[A]): Gaussian[A] = g

  def apply[@sp A](mean: A, stdDev: A)(implicit g: Gaussian[A]): Dist[A] = g(mean, stdDev)
}

trait GaussianInstances {
  import BigDecimal.defaultMathContext
  import spire.std.bigDecimal._

  implicit val float: Gaussian[Float] =
    new Gaussian[Float] {
      def apply(mean: Float, stdDev: Float): Dist[Float] =
        new DistFromGen(g => (Ziggurat.rnor(g) * stdDev + mean).toFloat)
    }

  implicit val double: Gaussian[Double] =
    new Gaussian[Double] {
      def apply(mean: Double, stdDev: Double): Dist[Double] =
        new DistFromGen(g => Ziggurat.rnor(g) * stdDev + mean)
    }

  implicit def bigDecimal(implicit mc: MathContext = defaultMathContext): Gaussian[BigDecimal] =
    new MarsagliaGaussian[BigDecimal]
}

/**
 * An implementation of `Gaussian` that uses the Marsaglia algorithm.
 */
final class MarsagliaGaussian[@sp(Float, Double) A: Field: NRoot: Trig: Order: Uniform] extends Gaussian[A] {
  import spire.syntax.field._
  import spire.syntax.nroot._
  import spire.syntax.trig._
  import spire.syntax.order._

  final private val u = Dist.uniform[A](-Field[A].one, Field[A].one)

  def apply(mean: A, stdDev: A): Dist[A] = {
    new DistFromGen[A]({ gen =>
      @tailrec def loop(): A = {
        val x = u(gen)
        val y = u(gen)
        val s = x * x + y * y
        if (s >= Field[A].one || s === Field[A].zero) {
          loop()
        } else {
          val scale = stdDev * (-2 * s.log() / s).sqrt()
          x * scale + mean // Dropped: y * scale + mean
        }
      }
      loop()
    })
  }
}
