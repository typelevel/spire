package spire.optional

import scala.collection.SeqLike
import scala.{specialized => sp}

import spire.algebra.{ Eq, Order, VectorSpace, AdditiveMonoid }
import spire.std.{ SeqVectorEq, SeqVectorOrder }
import spire.std.{ ArrayVectorEq, ArrayVectorOrder }
import spire.std.MapVectorEq

trait VectorOrderLow {
  implicit def seqEq[A, CC[A] <: SeqLike[A, CC[A]]](implicit
    order: Eq[A],
    vectorSpace: VectorSpace[CC[A], A],
    scalar: AdditiveMonoid[A]
  ): Eq[CC[A]] =
    new SeqVectorEq[A, CC[A]]()(order, scalar)

  implicit def arrayEq[@sp(Int,Long,Float,Double) A](implicit
    order: Eq[A],
    vectorSpace: VectorSpace[Array[A], A],
    scalar: AdditiveMonoid[A]
  ): Eq[Array[A]] =
    new ArrayVectorEq[A]()(order, scalar)

  implicit def mapEq[K, V](implicit
    order: Eq[V],
    vectorSpace: VectorSpace[Map[K, V], V],
    scalar: AdditiveMonoid[V]
  ): Eq[Map[K, V]] =
    new MapVectorEq[K, V]()(order, scalar)
}

/**
 * This object provides implicit instances of Eq and Order for Seq-likes
 * that will behave like infinite vectors. Essentially all this means is that
 * `Seq(0, 0, 0) === Seq()`, since both are infinite vectors of zeros. Any
 * element not explicitly set is implied to be 0.
 */
object vectorOrder extends VectorOrderLow {
  implicit def seqOrder[A, CC[A] <: SeqLike[A, CC[A]]](implicit
    order: Order[A],
    vectorSpace: VectorSpace[CC[A], A],
    scalar: AdditiveMonoid[A]
  ): Order[CC[A]] =
    new SeqVectorOrder[A, CC[A]]()(order, scalar)

  implicit def arrayOrder[@sp(Int,Long,Float,Double) A](implicit
    order: Order[A],
    vectorSpace: VectorSpace[Array[A], A],
    scalar: AdditiveMonoid[A]
  ): Order[Array[A]] =
    new ArrayVectorOrder[A]()(order, scalar)
}
