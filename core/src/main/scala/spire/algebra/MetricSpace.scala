package spire
package algebra


/**
 * This type class models a metric space `V`. The distance between 2 points in
 * `V` is measured in `R`, which should be real (ie. `IsReal[R]` exists).
 */
trait MetricSpace[V, @sp(Int, Long, Float, Double) R] extends Any {
  def distance(v: V, w: V): R
}

object MetricSpace extends MetricSpace0 {
  @inline final def apply[V, @sp(Int,Long,Float,Double) R](implicit V: MetricSpace[V, R]): MetricSpace[V, R] = V

  def distance[V, @sp(Int,Long,Float,Double) R](v: V, w: V)(implicit
    metric: MetricSpace[V, R]): R = metric.distance(v, w)

  /**
   * Returns `true` iff the distance between `x` and `y` is less than or equal
   * to `tolerance`.
   */
  def closeTo[V, @sp(Int,Long,Float,Double) R](x: V, y: V, tolerance: Double)(implicit
      R: IsReal[R], metric: MetricSpace[V, R]): Boolean =
    R.toDouble(metric.distance(x, y)) <= tolerance
}

private[algebra] trait MetricSpace0 {
  implicit def realMetricSpace[@sp(Int,Long,Float,Double) R](implicit
      R0: IsReal[R], R1: Rng[R]): MetricSpace[R, R] = new MetricSpace[R, R] {
    def distance(v: R, w: R): R = R0.abs(R1.minus(v, w))
  }
}
