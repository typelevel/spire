package spire.algebra

import scala.{ specialized => spec }

trait MetricSpace[V, @spec(Int, Long, Float, Double) R] {
  def distance(v: V, w: V): R
}

object MetricSpace

//final class MetricSpaceOps[V, S](lhs: V)(implicit metric: MetricSpace[V, S]) {
//  def closeTo(rhs: A, tolerance: Double)(implicit real: IsReal[A], g: Monoid[A]) = {
//    val lsz = metric.distance(g.id, lhs)
//    val rsz = metric.distance(g.id, rhs)
//    val eps = math.max(real.toDouble(lsz), real.toDouble(rsz)) * tolerance
//    val dist = metric.distance(lhs, rhs)
//    dist < eps
//  }
//}
