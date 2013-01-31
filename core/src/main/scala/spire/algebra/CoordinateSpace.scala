package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait CoordinateSpace[V, @spec F] extends NormedVectorSpace[V, F] {
  def dimensions: Int

  def coord(v: V, i: Int): F

  def _x(v: V): F = coord(v, 0)
  def _y(v: V): F = coord(v, 1)
  def _z(v: V): F = coord(v, 2)
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

  def array[@spec(Float, Double) A](dimensions0: Int)(implicit field0: Field[A],
      nroot0: NRoot[A], classTag0: ClassTag[A]) = new ArrayCoordinateSpace[A] {
    val scalar = field0
    val nroot = nroot0
    val dimensions = dimensions0
    val classTag = classTag0
  }
}

final class CoordinateSpaceOps[V](v: V) {
  def _x[F](implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.unopWithEv[CoordinateSpace[V, F], F]

  def _y[F](implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.unopWithEv[CoordinateSpace[V, F], F]

  def _z[F](implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.unopWithEv[CoordinateSpace[V, F], F]

  def coord[F](rhs: Int)(implicit ev: CoordinateSpace[V, F]): F =
    macro Ops.binopWithEv[Int, CoordinateSpace[V, F], F]

  def dimensions[F](implicit ev: CoordinateSpace[V, F]): Int =
    macro Ops.unopWithEv[CoordinateSpace[V, F], Int]
}
