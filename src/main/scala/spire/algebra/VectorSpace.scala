package spire.algebra

import spire.macrosk.Ops

import scala.{ specialized => spec }

trait VectorSpace[V, @spec(Int, Long, Float, Double) F] extends Module[V, F] {
  implicit def scalar: Field[F]
}

trait VectorSpace0 {
  implicit def NormedVectorSpaceIsVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    vectorSpace: NormedVectorSpace[V, F]): VectorSpace[V, F] = vectorSpace
}

object VectorSpace extends VectorSpace0
