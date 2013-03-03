package spire.algebra

import spire.macrosk.Ops
import spire.NoImplicit

import scala.{ specialized => spec }
import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait VectorSpace[V, @spec(Int, Long, Float, Double) F] extends Module[V, F] {
  implicit def scalar: Field[F]

  def divr(v: V, f: F): V = timesl(scalar.reciprocal(f), v)
}

object VectorSpace {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: VectorSpace[V, R]) = V
}

final class VectorSpaceOps[V, F](rhs: V)(implicit ev: VectorSpace[V, F]) {
  def :/ (rhs:F): V = macro Ops.binop[F, V]
}
