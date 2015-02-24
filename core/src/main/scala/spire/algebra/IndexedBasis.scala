package spire.algebra

import scala.{ specialized => spec }

import spire.std.MapBasis

/**
 * An `IndexedBasis` of a [[VectorSpace]] `V` provides a coordinate system on `V`
 * with elements of type `K` indexed by `I`.
 *
 * If you don't care about the index type `I`, then you can use the type alias
 * [[Basis]] instead, which just has 2 type parameters (`V` and `K`) and the
 * index is left as an existential.
 *
 * If you want an `IndexedBasis` where `I = Int`, then you use [[Frame]]
 * instead.
 *
 * All basis operations generally are only required to work with non-zero
 * entries in a vector. This is because many representations of vectors
 * may omit zero-valued elements (eg. a map), perhaps for performance or
 * perhaps because the index is too large to fit in memory (eg.
 * `Map[String, Int]`).
 */
trait IndexedBasis[V, @spec(Int, Long, Float, Double) K, @spec(Int, Long) I] {
  val builder: VectorBuilder[V, K, I]

  /**
   * Returns `true` if [[size]] is defined.
   */
  // def hasKnownSize: Boolean

  /**
   * Returns the number of elements in a vector, but only if [[hasKnownSize]]
   * returns `true`, otherwise the result is undefined.
   */
  // def size: Int

  /**
   * Returns the element in the `i`-th coordinate of `v`. Note that `i` must
   * be a valid index, otherwise the results are undefined.
   */
  def coord(v: V, i: I): K

  /**
   * Iterates over at least all non-zero elements of `v`, with their index,
   * applying the function `f` to each.
   */
  def foreachWithIndex[U](v: V)(f: (I, K) => U): Unit

  /**
   * Iterates over at least all non-zero elements of `v`, applying the
   * function `f` to each.
   */
  def zipForeachWithIndex[U](v: V, w: V)(f: (I, K, K) => U): Unit

  /**
   * Creates a vector with the given elements at the given indices. Any
   * omitted index is treated as a zero. If an index appears more than once
   * then the last value associated with that index is used.
   */
  def apply(elems: (I, K)*): V =
    builder.result(elems.foldLeft(builder.init) {
      case (acc, (i, k)) => builder.update(acc, i, k)
    })

  /**
   * Iterates over at least all non-zero elements of `v`, applying the
   * function `f` to each.
   */
  def foreach[U](v: V)(f: K => U): Unit =
    foreachWithIndex(v) { (i, k) => f(k) }

  /**
   * Maps the elements of `v` using the function `f`. This is only guaranteed
   * to map over non-zero elements. Zero elements (as defined by the
   * [[AdditiveMonoid]] for `K`) may be missed.
   */
  def map(v: V)(f: K => K): V =
    mapWithIndex(v)((i, k) => f(k))

  /**
   * Maps the elements of `v`, along with its index, using the function `f`.
   * This is only guaranteed to map over non-zero elements. Zero elements (as
   * defined by the [[AdditiveMonoid]] for `K`) may be missed.
   */
  def mapWithIndex(v: V)(f: (I, K) => K): V = {
    var s = builder.init
    foreachWithIndex(v) { (i, k) =>
      s = builder.update(s, i, f(i, k))
    }
    builder.result(s)
  }

  /**
   * Maps at least the non-zero elements of `v` using `f` and sums up the
   * results using the [[CMonoid]] for `A`.
   *
   * A [[CMonoid]] is required since an `IndexedBasis` is not necessarily
   * ordered.
   */
  def foldMap[A](v: V)(f: K => A)(implicit A: CMonoid[A]): A =
    foldMapWithIndex(v) { (i, k) => f(k) }

  /**
   * Maps at least the non-zero elements of `v` with it's index, using `f`
   * and sums up the results. The result type must be a commutative monoid,
   * since an `IndexedBasis` may not be ordered.
   */
  def foldMapWithIndex[A](v: V)(f: (I, K) => A)(implicit A: CMonoid[A]): A = {
    var sum = A.id
    foreachWithIndex(v) { (i, k) => sum = A.op(sum, f(i, k)) }
    sum
  }

  /**
   * Iterates over at least all non-zero elements of `v`, applying the
   * function `f` to each.
   */
  def zipForeach[U](v: V, w: V)(f: (K, K) => U): Unit =
    zipForeachWithIndex(v, w) { (i, k0, k1) => f(k0, k1) }

  /**
   * Applies the function `f` over all pairs of elements, where at least one
   * is non-zero, that have the same index. If both pairs of elements are zero
   * then it may be skipped (and treated as 0 in the resulting vector).
   */
  def zipMap(v: V, w: V)(f: (K, K) => K): V =
    zipMapWithIndex(v, w)((i, x, y) => f(x, y))

  /**
   * Applies the function `f` over all pairs of elements, where at least one
   * is non-zero, that have the same index. The index is also provided as an
   * argument. If both pairs of elements are zero then it may be skipped (and
   * treated as 0 in the resulting vector).
   */
  def zipMapWithIndex(v: V, w: V)(f: (I, K, K) => K): V = {
    var s = builder.init
    zipForeachWithIndex(v, w) { (i, x, y) =>
      builder.update(s, i, f(i, x, y))
    }
    builder.result(s)
  }

  /**
   * Zips `v` and `w` together, applies the function `f` to the elements, then
   * sums up all the results using the commutative monoid for `A`.
   *
   * A [[CMonoid]] is required since an `IndexedBasis` is not necessarily
   * ordered.
   */
  def zipFoldMap[A](v: V, w: V)(f: (K, K) => A)(implicit A: CMonoid[A]): A =
    zipFoldMapWithIndex(v, w) { (i, x, y) => f(x, y) }

  /**
   * Zips `v` and `w` together, applies the function `f` to the elements and
   * their index, then sums up all the results using the commutative monoid
   * for `A`.
   *
   * A [[CMonoid]] is required since an `IndexedBasis` is not necessarily
   * ordered.
   */
  def zipFoldMapWithIndex[A](v: V, w: V)(f: (I, K, K) => A)(implicit A: CMonoid[A]): A = {
    var sum = A.id
    zipForeachWithIndex(v, w) { (i, x, y) =>
      sum = A.op(sum, f(i, x, y))
    }
    sum
  }
}

object IndexedBasis {
  final def apply[V, @spec(Int, Long, Float, Double) K, @spec(Int, Long) I](implicit basis: IndexedBasis[V, K, I]): IndexedBasis[V, K, I] = basis

  case class Free[I, K: AdditiveMonoid](val basis: Set[I]) extends MapBasis[I, K] {
    override val hasKnownSize: Boolean = true
    val size: Int = basis.size
  }
}
