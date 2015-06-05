package spire.stats.distribution

import spire.algebra._
import spire.random.Generator
import spire.syntax.field._

/** In a location-scale family a general distribution can be written as
  * a function of the standard distribution (zero location and unit
  * scale) and its own location and scale.
  *
  * @see [[http://en.wikipedia.org/wiki/Location-scale_family]]
  */
abstract class LocationScaleFamily[A: Field] {
  val stdDist: ContinuousDistribution[A]
  val location: A
  val scale: A

  def pdf(x: A): A =
    Field[A].one / scale * stdDist.pdf((x - location) / scale)

  def cdf(x: A): A =
    stdDist.cdf((x - location) / scale)

  lazy val transform: (Generator => A) =
    stdDist.transform andThen (_ * scale + location)
}

