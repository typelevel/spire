package spire.algebra

import scala.{ specialized => spec }

trait MetricSpace[V, @spec(Int, Long, Float, Double) R] {
  def distance(v: V, w: V): R
}

object MetricSpace extends MetricSpace0 with MetricSpaceFunctions {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: MetricSpace[V, R]) = V
}

trait MetricSpace0 {
  implicit def editDistance: MetricSpace[String, Int] = string.LevenshteinDistance

  implicit def NormedVectorSpaceIsMetricSpace[V, @spec(Float,Double) R](implicit
      V: NormedVectorSpace[V, R]): MetricSpace[V, R] = V
}

trait MetricSpaceFunctions {
  def distance[V, @spec(Int,Long,Float,Double) R](v: V, w: V)(implicit
    metric: MetricSpace[V, R]): R = metric.distance(v, w)

  def closeTo[V, @spec(Int,Long,Float,Double) R](x: V, y: V, tolerance: Double)(implicit
      R: IsReal[R], metric: MetricSpace[V, R]): Boolean = {
    val d = R.toDouble(metric.distance(x, y))
    d <= tolerance
  }
}
