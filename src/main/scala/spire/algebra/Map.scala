package spire.algebra

import scala.{ specialized => spec }

trait MapSemigroup[K, V] extends Semigroup[Map[K, V]] {
  implicit def semigroup: Semigroup[V]

  def op(x: Map[K, V], y: Map[K, V]): Map[K, V] = MapSupport.op(x, y)
}

trait MapMonoid[K, V] extends Monoid[Map[K, V]] with MapSemigroup[K, V] {
  def id: Map[K, V] = Map.empty
}

private object MapSupport {
  @inline private final def add[K, V](x: Map[K, V], y: Map[K, V], flip: Boolean)(implicit
      semigroup: Semigroup[V]): Map[K, V] = {
    y.foldLeft(x) { case (z, kv) =>
      z + ((kv._1, (x get kv._1) match {
        case Some(u) => if (flip) semigroup.op(kv._2, u) else semigroup.op(u, kv._2)
        case None => kv._2
      }))
    }
  }

  final def op[K, V](x: Map[K, V], y: Map[K, V])(implicit
      semigroup: Semigroup[V]): Map[K, V] = {
    if (x.size < y.size) add(y, x, true) else add(x, y, false)
  }
}
