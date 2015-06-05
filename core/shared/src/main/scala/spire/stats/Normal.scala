package spire.stats.distribution

import scala.reflect.runtime.universe._
import spire.algebra._
import spire.random.Generator
import spire.random.Ziggurat

trait NormalDistribution[A] extends ContinuousDistribution[A] {
  val mean: A
  val stdDev: A
}

object Normal {
  def Standard[A: Field: Trig: IsReal: TypeTag](): NormalDistribution[A] = new StandardNormalDistributionImpl[A]()

  def apply[A: Field: Trig: IsReal: TypeTag](min: A, max: A): NormalDistribution[A] = new NormalDistributionImpl[A](min, max)

  class StandardNormalDistributionImpl[A: Field: Trig: IsReal: TypeTag] extends NormalDistribution[A] {
    import spire.math._
    import spire.syntax.field._
    import spire.syntax.trig._
    import spire.syntax.isReal._

    val mean = Field[A].zero
    val stdDev = Field[A].one

    private[stats] val transform: (Generator => A) = (gen => Field[A].fromDouble(Ziggurat.rnor(gen)))

    def pdf(x: A): A = {
      exp(-0.5 * x * x) / sqrt(2 * pi)
    }

    def cdf(x: A): A = {
      0.5 * (1 + erf(x / sqrt(2)))
    }

    private def erf(x: A): A = if (x.signum < 0) {
      -erf(-x)
    } else {
      val t = 1 / (1 + 0.3275911 * x)
      val y = t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429))))
      1 - y * (-x * x).exp()
    }
  }

  class NormalDistributionImpl[A: Field: Trig: IsReal: TypeTag](val mean: A, val stdDev: A) extends NormalDistribution[A] {
    val family = new LocationScaleFamily[A] {
      val stdDist = Standard[A]
      val location = mean
      val scale = stdDev
    }
    private[stats] val transform = family.transform
    def pdf(x: A) = family.pdf(x)
    def cdf(x: A) = family.cdf(x)
  }
}

