package spire.algebra

import scala.{ specialized => spec }

trait MetricSpace[V, @spec(Int, Long, Float, Double) R] {
  def distance(v: V, w: V): R

  def close(v: V, w: V)(implicit o: Order[R], e: MetricSpace.Epsilon[V, R]): Boolean =
    o.lteqv(distance(v, w), e.value)
}

object MetricSpace extends MetricSpace0 with MetricSpaceFunctions {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: MetricSpace[V, R]) = V

  trait Epsilon[V, R] {
    def value: R
  }

  def Epsilon[V, R](e: R)(implicit ms: MetricSpace[V, R]): Epsilon[V, R] = new Epsilon[V, R] {
    val value = e
  }
}

trait MetricSpace0 {
  implicit def simpleMetricSpace[@spec(Int,Long,Float,Double) R](implicit
      s: IsReal[R], rng: Rng[R]): MetricSpace[R, R] = new MetricSpace[R, R] {
    def distance(v: R, w: R): R = s.abs(rng.minus(v, w))
  }
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
