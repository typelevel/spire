package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }
import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

trait VectorSpace[V, @spec(Int, Long, Float, Double) F] extends Module[V, F] {
  implicit def scalar: Field[F]

  def divr(v: V, f: F): V = timesl(scalar.reciprocal(f), v)
}

object VectorSpace extends VectorSpace2

trait VectorSpace0 {
  implicit def seqVectorSpace[A: Field]: VectorSpace[Seq[A], A] = SeqVectorSpace[A, Seq[A]]
}

trait VectorSpace1 extends VectorSpace0 {
  implicit def ListVectorSpace[A: Field]: VectorSpace[List[A], A] = SeqVectorSpace[A, List[A]]
  implicit def VectorVectorSpace[A: Field]: VectorSpace[Vector[A], A] = SeqVectorSpace[A, Vector[A]]
}

trait VectorSpace2 extends VectorSpace1 {
  implicit def NormedVectorSpaceIsVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    vectorSpace: NormedVectorSpace[V, F]): VectorSpace[V, F] = vectorSpace
}

final class VectorSpaceOps[V, F](rhs: V)(implicit ev: VectorSpace[V, F]) {
  def :/ (rhs:F): V = macro Ops.binop[F, V]
}

trait SeqVectorSpace[A, SA <: SeqLike[A, SA]] extends SeqModule[A, SA] with VectorSpace[SA, A]

object SeqVectorSpace {
  def apply[A, SA <: SeqLike[A, SA]](implicit A: Field[A], cbf0: CanBuildFrom[SA, A, SA]) = {
    new SeqVectorSpace[A, SA] {
      val scalar = A
      val cbf = cbf0
    }
  }
}
