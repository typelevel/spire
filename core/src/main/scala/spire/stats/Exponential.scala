package spire.stats.distribution

import scala.reflect.runtime.universe._
import spire.algebra._
import spire.random.Generator
import spire.random.Ziggurat

trait ExponentialDistribution[A] extends ContinuousDistribution[A] {
  val mode: A
  val rate: A
}

object Exponential {
  def Standard[A: Field: Trig: IsReal: TypeTag: StandardBuilder](): ExponentialDistribution[A] = implicitly[StandardBuilder[A]].apply()

  def apply[A: Field: Trig: IsReal: TypeTag: Builder](mode: A, rate: A): ExponentialDistribution[A] = implicitly[Builder[A]].apply(mode, rate)

  abstract class StandardBuilder[A: Field: IsReal: TypeTag] {
    def apply(): ExponentialDistribution[A]
  }

  abstract class Builder[A: Field: IsReal: TypeTag] {
    def apply(mean: A, stdDev: A): ExponentialDistribution[A]
  }

  object StandardBuilder {
    import spire.std.double._

    implicit val doubleBuilder = new StandardBuilder[Double] {
      def apply() = new StandardExponentialDistributionImpl()
    }
  }

  object Builder {
    import spire.std.double._

    implicit val doubleBuilder = new Builder[Double] {
      def apply(mean: Double, stdDev: Double) = new ExponentialDistributionImpl(mean, stdDev)
    }
  }

  class StandardExponentialDistributionImpl[A: Field: Trig: IsReal: TypeTag] extends ExponentialDistribution[A] {
    import spire.math._
    import spire.syntax.field._
    import spire.syntax.isReal._

    val mode = Field[A].zero
    val rate = Field[A].one

    private[stats] val transform: (Generator => A) = (gen => Field[A].fromDouble(Ziggurat.rexp(gen)))

    def pdf(x: A): A = x match {
      case x if (x < Field[A].zero) => Field[A].zero
      case _ => exp(-x)
    }

    def cdf(x: A): A = x match {
      case x if (x < Field[A].zero) => Field[A].zero
      case _ => Field[A].one - exp(-x)
    }
  }

  class ExponentialDistributionImpl[A: Field: Trig: IsReal: TypeTag: StandardBuilder](val mode: A, val rate: A) extends ExponentialDistribution[A] {
    import spire.syntax.field._

    val family = new LocationScaleFamily[A] {
      val stdDist = Standard[A]
      val location = mode
      val scale = Field[A].one / rate
    }

    private[stats] val transform = family.transform
    def pdf(x: A) = family.pdf(x)
    def cdf(x: A) = family.cdf(x)
  }
}

