package spire.algebra

import spire.macrosk.Ops
import spire.math.{ UInt, Order }

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

trait NormedVectorSpace[V, @spec(Int, Long, Float, Double) F]
extends VectorSpace[V, F] with MetricSpace[V, F] {
  def norm(v: V): F

  def normalize(v: V): V = divr(v, norm(v))
  def distance(v: V, w: V): F = norm(minus(v, w))
}

object NormedVectorSpace extends NormedVectorSpace0 with NormedVectorSpaceFunctions {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: NormedVectorSpace[V, R]) = V
}

trait NormedVectorSpace0 {
  implicit def InnerProductSpaceIsNormedVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    vectorSpace: InnerProductSpace[V, F]): NormedVectorSpace[V, F] = vectorSpace
}

trait NormedVectorSpaceFunctions {
  def max[A, CC[A] <: SeqLike[A, CC[A]]](implicit field0: Field[A], order0: Order[A],
      signed0: Signed[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]): NormedVectorSpace[CC[A], A] = {
    new SeqMaxNormedVectorSpace[A, CC[A]] {
      val scalar = field0
      val order = order0
      val signed = signed0
      val cbf = cbf0
    }
  }

  def Lp[A, CC[A] <: SeqLike[A, CC[A]]](p0: UInt)(implicit field0: Field[A], nroot0: NRoot[A],
      signed0: Signed[A], cbf0: CanBuildFrom[CC[A], A, CC[A]]): NormedVectorSpace[CC[A], A] = {
    require(p0 > UInt(0), "p must be > 0")

    new SeqLpNormedVectorSpace[A, CC[A]] {
      val scalar = field0
      val nroot = nroot0
      val signed = signed0
      val cbf = cbf0

      val p = p0.toInt
    }
  }
}

final class NormedVectorSpaceOps[V](lhs: V) {
  def norm[F](implicit ev: NormedVectorSpace[V, F]): F =
    macro Ops.unopWithEv[NormedVectorSpace[V, F], F]

  def normalize[F](implicit ev: NormedVectorSpace[V, F]): V =
    macro Ops.unopWithEv[NormedVectorSpace[V, F], V]
}
