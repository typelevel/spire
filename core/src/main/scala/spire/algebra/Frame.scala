package spire.algebra

/**
 * A `Frame` is a finite, ordered [[Basis]].
 */
trait Frame[V, K] extends IndexedBasis[V, K, Int]

object Frame {
  def Free[I, K](basis: Set[I]): Free[I, K] = new Free(basis.toVector)

  class Free[I, K](val basis: Vector[I]) extends Frame[Vector[K], K] {
    val hasKnownSize: Boolean = true
    val size: Int = basis.size

    def coord(v: Vector[K], i: Int): K = v(i)

    def map(v: Vector[K])(f: K => K): Vector[K] =
      v map f

    def mapWithIndex(v: Vector[K])(f: (Int, K) => K): Vector[K] =
      v.zipWithIndex map { case (k, i) => f(i, k) }

    def zipMap(v: Vector[K], w: Vector[K])(f: (K, K) => K): Vector[K] =
      v zip w map f.tupled

    def zipMapWithIndex(v: Vector[K], w: Vector[K])(f: (Int, K, K) => K): Vector[K] =
      v.zipWithIndex zip w map { case ((k0, i), k1) => f(i, k0, k1) }

    def foreachNonZero[U](v: Vector[K])(f: K => U): Unit =
      v foreach f

    def foreachNonZeroWithIndex[U](v: Vector[K])(f: (Int, K) => U): Unit =
      v.zipWithIndex foreach { case (k, i) => f(i, k) }
  }
}
