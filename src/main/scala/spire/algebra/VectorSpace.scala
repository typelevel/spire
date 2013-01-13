package spire.algebra

import spire.macrosk.Ops
import spire.NoImplicit

import scala.{ specialized => spec }
import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait VectorSpace[V, @spec(Int, Long, Float, Double) F] extends Module[V, F] {
  implicit def scalar: Field[F]

  def divr(v: V, f: F): V = timesl(scalar.reciprocal(f), v)
}

object VectorSpace extends VectorSpace2

trait VectorSpace0 {
  implicit def seq[A, CC[A] <: SeqLike[A, CC[A]]](implicit field0: Field[A],
      cbf0: CanBuildFrom[CC[A], A, CC[A]],
      ev: NoImplicit[NormedVectorSpace[CC[A], A]]) = new SeqVectorSpace[A, CC[A]] {
    val scalar = field0
    val cbf = cbf0
  }
}

trait VectorSpace1 extends VectorSpace0 {
  implicit def ArrayVectorSpace[@spec(Int,Long,Float,Double) A](implicit
      scalar0: Field[A], classTag0: ClassTag[A]): VectorSpace[Array[A], A] = new ArrayVectorSpace[A] {
    val scalar = scalar0
    val classTag = classTag0
  }
}

trait VectorSpace2 extends VectorSpace1 {
  implicit def NormedVectorSpaceIsVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    vectorSpace: NormedVectorSpace[V, F]): VectorSpace[V, F] = vectorSpace
}

final class VectorSpaceOps[V, F](rhs: V)(implicit ev: VectorSpace[V, F]) {
  def :/ (rhs:F): V = macro Ops.binop[F, V]
}

trait SeqVectorSpace[A, SA <: SeqLike[A, SA]] extends SeqModule[A, SA] with VectorSpace[SA, A]

trait ArrayVectorSpace[@spec(Int,Long,Float,Double) A] extends ArrayModule[A] with VectorSpace[Array[A], A]
