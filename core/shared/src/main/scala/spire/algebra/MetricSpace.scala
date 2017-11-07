package spire
package algebra


/**
 * This type class models a metric space `V`. The distance between 2 points in
 * `V` is measured in `M`, which should be real.
 */
trait MetricSpace[V, @sp(Int, Long, Float, Double) M] extends Any {
  def distance(v: V, w: V): M
}

object MetricSpace {
  @inline final def apply[V, @sp(Int,Long,Float,Double) M](implicit V: MetricSpace[V, M]): MetricSpace[V, M] = V

  def distance[V, @sp(Int,Long,Float,Double) M](v: V, w: V)(implicit
    metric: MetricSpace[V, M]): M = metric.distance(v, w)

  /**
   * Returns `true` iff the distance between `x` and `y` is less than or equal
   * to `tolerance`.
   */
  def closeTo[V, @sp(Int,Long,Float,Double) M](x: V, y: V, tolerance: Double)(implicit
      R: IsReal[M], metric: MetricSpace[V, M]): Boolean =
    R.toDouble(metric.distance(x, y)) <= tolerance
}
