package spire.algebra

import scala.{ specialized => spec }

trait MetricSpace[V, @spec(Int, Long, Float, Double) R] {
  def distance(v: V, w: V): R
}

object MetricSpace extends MetricSpace0 with MetricSpaceFunctions

trait MetricSpace0 {
  implicit def NormedVectorSpaceIsMetricSpace[V, @spec(Float,Double) R](implicit
      V: NormedVectorSpace[V, R]): MetricSpace[V, R] = V
}

trait MetricSpaceFunctions {
  def closeTo[V, @spec(Int,Long,Float,Double) R](x: V, y: V, tolerance: Double)(implicit
      R: IsReal[R], metric: MetricSpace[V, R]): Boolean = {
    val d = R.toDouble(metric.distance(x, y))
    d <= tolerance
  }
}
