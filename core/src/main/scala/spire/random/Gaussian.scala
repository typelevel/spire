package spire.random

import java.math.MathContext

import scala.{specialized => spec}
import scala.annotation.tailrec

import spire.algebra.{Field, NRoot, Order, Trig}

trait Gaussian[@spec(Float,Double) A] {
  /**
   * Return an `A` that is normally distributed about `mean` with a standard
   * deviation of `stdDev`.
   */
  def apply(mean: A, stdDev: A): Dist[A]
}

object Gaussian extends GaussianInstances {
  @inline final def apply[@spec(Float,Double) A](implicit g: Gaussian[A]): Gaussian[A] = g
}

trait GaussianInstances {
  import BigDecimal.defaultMathContext
  import spire.std.float._
  import spire.std.double._
  import spire.std.bigDecimal._

  implicit val float = new MarsagliaGaussian[Float]
  implicit val double = new MarsagliaGaussian[Double]
  implicit def bigDecimal(implicit mc: MathContext = defaultMathContext) =
    new MarsagliaGaussian[BigDecimal]
}

/**
 * An implementation of `Gaussian` that uses the Marsaglia algorithm.
 */
final class MarsagliaGaussian[@spec(Float,Double) A: Field: NRoot: Trig: Order: Uniform]
extends Gaussian[A] {
  import spire.syntax.field._
  import spire.syntax.nroot._
  import spire.syntax.trig._
  import spire.syntax.order._

  private final val u = Dist.uniform[A](-Field[A].one, Field[A].one)

  def apply(mean: A, stdDev: A): Dist[A] = {
    new DistFromGen[A]({ gen => 
      @tailrec def loop(): A = {
        val x = u(gen)
        val y = u(gen)
        val s = x * x + y * y
        if (s >= Field[A].one || s === Field[A].zero) {
          loop()
        } else {
          val scale = stdDev * (-2 * s.log / s).sqrt
          x * scale + mean // Dropped: y * scale + mean
        }
      }
      loop()
    })

  }
}
