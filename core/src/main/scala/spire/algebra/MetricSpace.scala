package spire.algebra

import scala.{ specialized => spec }

/**
 * This type class models a metric space `V`. The distance between 2 points in
 * `V` is measured in `R`, which should be real (ie. `IsReal[R]` exists).
 */
trait MetricSpace[V, @spec(Int, Long, Float, Double) R] {
  def distance(v: V, w: V): R
}

object MetricSpace extends MetricSpace0 {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: MetricSpace[V, R]) = V

  def distance[V, @spec(Int,Long,Float,Double) R](v: V, w: V)(implicit
    metric: MetricSpace[V, R]): R = metric.distance(v, w)

  /**
   * Returns `true` iff the distance between `x` and `y` is less than or equal
   * to `tolerance`.
   */
  def closeTo[V, @spec(Int,Long,Float,Double) R](x: V, y: V, tolerance: Double)(implicit
      R: IsReal[R], metric: MetricSpace[V, R]): Boolean = {
    val d = R.toDouble(metric.distance(x, y))
    d <= tolerance
  }
}

private[algebra] trait MetricSpace0 {
  implicit def realMetricSpace[@spec(Int,Long,Float,Double) R](implicit
      R0: IsReal[R], R1: Rng[R]) = new MetricSpace[R, R] {
    def distance(v: R, w: R): R = R0.abs(R1.minus(v, w))
  }
}
