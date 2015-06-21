package spire.stats.distribution

import scala.reflect.runtime.universe._
import spire.algebra._
import spire.random.Generator
import spire.random.Ziggurat

trait UniformDistribution[A] extends ContinuousDistribution[A] {
  val min: A
  val max: A
}

object Uniform {
  def Standard[A: Field: IsReal: TypeTag: StandardBuilder](): UniformDistribution[A] = implicitly[StandardBuilder[A]].apply()

  def apply[A: Field: IsReal: TypeTag: Builder](min: A, max: A): UniformDistribution[A] = implicitly[Builder[A]].apply(min, max)

  abstract class StandardBuilder[A: Field: IsReal: TypeTag] {
    def apply(): UniformDistribution[A]
  }

  abstract class Builder[A: Field: IsReal: TypeTag] {
    def apply(min: A, max: A): UniformDistribution[A]
  }

  object StandardBuilder {
    import spire.std.double._

    implicit val doubleBuilder = new StandardBuilder[Double] {
      def apply() = new StandardUniformDistributionImpl()
    }
  }

  object Builder {
    import spire.std.double._

    implicit val doubleBuilder = new Builder[Double] {
      def apply(min: Double, max: Double) = new UniformDistributionImpl(min, max)
    }
  }

  class StandardUniformDistributionImpl[A: Field: IsReal: TypeTag] extends UniformDistribution[A] {
    import spire.syntax.isReal._
    import RichGenerator._

    val min = Field[A].zero
    val max = Field[A].one

    private[stats] val transform: (Generator => A) = (gen => gen.next0[A])

    def pdf(x: A): A = x match {
      case x if (Field[A].zero <= x) && (x <= Field[A].one) => Field[A].one
      case _ => Field[A].zero
    }

    def cdf(x: A): A = x match {
      case x if (x <  Field[A].zero) => Field[A].zero
      case x if (x >= Field[A].one)  => Field[A].one
      case _ => x
    }
  }

  class UniformDistributionImpl[A: Field: IsReal: TypeTag: StandardBuilder](val min: A, val max: A) extends UniformDistribution[A] {
    import spire.syntax.field._

    val family = new LocationScaleFamily[A] {
      val stdDist = Standard[A]
      val location = min
      val scale = max - min
    }

    private[stats] val transform = family.transform
    def pdf(x: A) = family.pdf(x)
    def cdf(x: A) = family.cdf(x)
  }
}

