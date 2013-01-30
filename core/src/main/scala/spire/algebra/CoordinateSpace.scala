package spire.algebra

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait CoordinateSpace[V, @spec F] extends NormedVectorSpace[V, F] {
  def dimensions: Int

  def get(v: V, i: Int): F
}

object CoordinateSpace {
  @inline final def apply[V, @spec(Float,Double) F](implicit V: CoordinateSpace[V, F]) = V

  def seq[A, CC[A] <: SeqLike[A, CC[A]]](dimensions0: Int)(implicit field0: Field[A],
      nroot0: NRoot[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]) = new SeqCoordinateSpace[A, CC[A]] {
    val scalar = field0
    val nroot = nroot0
    val cbf = cbf0
    val dimensions = dimensions0
  }
}

final class CoordinateSpaceOps[V](v: V) {
  def _x[F](implicit ev: CoordinateSpace[V, F]): F = ev.get(v, 0)
  def _y[F](implicit ev: CoordinateSpace[V, F]): F = ev.get(v, 1)
  def _z[F](implicit ev: CoordinateSpace[V, F]): F = ev.get(v, 2)

  def coord[F](i: Int)(implicit ev: CoordinateSpace[V, F]): F = ev.get(v, i)

  def dimensions[F](implicit ev: CoordinateSpace[V, F]): Int = ev.dimensions
}
