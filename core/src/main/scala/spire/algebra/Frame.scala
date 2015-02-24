package spire.algebra

import scala.{ specialized => spec }

/**
 * A `Frame` is a finite, ordered [[Basis]]. A `Frame` must iterate over
 * elements in the order of the indices.
 *
 * TODO: It would be nice if we could just say that we only need a Monoid
 *       for the foldMap-like methods, and have it replace those methods.
 *       However, this doesn't seem to work right when I need to "override"
 *       them. Hrmm.
 */
trait Frame[V, K] extends IndexedBasis[V, K, Int]

object Frame {
  final def apply[V, @spec(Int, Long, Float, Double) K](implicit basis: Frame[V, K]): Frame[V, K] = basis

  case class Free[I, K](val basis: Vector[I])(implicit K: AdditiveMonoid[K]) extends Frame[Vector[K], K] {
    val builder = new VectorBuilder[Vector[K], K, Int] {
      type State = Vector[K]
      def init: State = Vector.fill(size)(K.zero)
      def update(s: State, i: Int, k: K): State = s.updated(i, k)
      def result(s: State): Vector[K] = s
    }
    def hasKnownSize: Boolean = true
    def size: Int = basis.size
    def coord(v: Vector[K], i: Int): K = v(i)
    def foreachWithIndex[U](v: Vector[K])(f: (Int, K) => U): Unit =
      (0 until size).foreach { i => f(i, v(i)) }
    def zipForeachWithIndex[U](v: Vector[K], w: Vector[K])(f: (Int, K, K) => U): Unit =
      (0 until size).foreach { i => f(i, v(i), w(i)) }
  }
}
