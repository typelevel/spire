package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }

trait InnerProductSpace[V, @spec(Int, Long, Float, Double) F] extends NormedVectorSpace[V, F] {
  implicit def nroot: NRoot[F]

  def norm(v: V): F = nroot.sqrt(dot(v, v))
  def dot(v: V, w: V): F
}

object InnerProductSpace {
  implicit def Tuple2IsInnerProductSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], nroot0: NRoot[A]) = new Tuple2IsInnerProductSpace[A] {
    val scalar = scalar0
    val nroot = nroot0
  }
}

final class InnerProductSpaceOps[V](lhs: V) {
  def dot[F](rhs: V)(implicit ev: InnerProductSpace[V, F]): F =
    macro Ops.binopWithEv[V, InnerProductSpace[V, F], F]
  def ⋅[F](rhs: V)(implicit ev: InnerProductSpace[V, F]): F =
    macro Ops.binopWithEv[V, InnerProductSpace[V, F], F]
}

trait Tuple2IsInnerProductSpace[@spec(Int,Long,Float,Double) A]
extends InnerProductSpace[(A, A), A] {
  def zero: (A, A) = (scalar.zero, scalar.zero)
  def plus(x: (A, A), y: (A, A)): (A, A) =
    (scalar.plus(x._1, y._1), scalar.plus(x._2, y._2))
  def negate(x: (A, A)): (A, A) =
    (scalar.negate(x._1), scalar.negate(x._2))
  override def minus(x: (A, A), y: (A, A)): (A, A) =
    (scalar.minus(x._1, y._1), scalar.minus(x._2, y._2))
  def timesl(r: A, v: (A, A)): (A, A) = (scalar.times(r, v._1), scalar.times(r, v._2))
  def dot(v: (A, A), w: (A, A)): A =
    scalar.plus(scalar.times(v._1, w._1), scalar.times(v._2, w._2))
}
