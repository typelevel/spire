package spire.algebra

import scala.{ specialized => spec }

trait VectorSpace[V, @spec(Int, Long, Float, Double) F] extends Module[V, F] {
  implicit def scalar: Field[F]

  def divr(v: V, f: F): V = timesl(scalar.reciprocal(f), v)
}

object VectorSpace {
  @inline final def apply[V, @spec(Int,Long,Float,Double) R](implicit V: VectorSpace[V, R]) = V
}
