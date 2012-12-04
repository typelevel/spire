package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }

trait NormedVectorSpace[V, @spec(Int, Long, Float, Double) F]
extends VectorSpace[V, F] with MetricSpace[V, F] {
  def distance(v: V, w: V): F = norm(minus(v, w))
  def norm(v: V): F
}

trait NormedVectorSpace0 {
  implicit def InnerProductSpaceIsNormedVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    vectorSpace: InnerProductSpace[V, F]): NormedVectorSpace[V, F] = vectorSpace
}

object NormedVectorSpace extends NormedVectorSpace0

final class NormedVectorSpaceOps[V](lhs: V) {
  def norm[F](implicit ev: NormedVectorSpace[V, F]): F =
    macro Ops.unopWithEv[NormedVectorSpace[V, F], F]
}
