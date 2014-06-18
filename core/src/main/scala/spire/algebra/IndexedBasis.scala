package spire.algebra

import scala.{ specialized => spec }

import spire.std.map.MapBasis

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
 */
trait IndexedBasis[V, @spec(Int, Long, Float, Double) K, @spec(Int, Long) I] {

  /**
   * Returns `true` if [[size]] is defined.
   */
  def hasKnownSize: Boolean

  /**
   * Returns the number of elements in a vector, but only if [[hasKnownSize]]
   * returns `true`, otherwise the result is undefined.
   */
  def size: Int

  def coord(v: V, i: I): K

  def map(v: V)(f: K => K): V
  def mapWithIndex(v: V)(f: (I, K) => K): V

  def zipMap(v: V, w: V)(f: (K, K) => K): V
  def zipMapWithIndex(v: V, w: V)(f: (I, K, K) => K): V
  def zipSum(v: V, w: V)(f: (K, K) => K)(implicit K: AdditiveMonoid[K]): K = {
    var sum = K.zero
    foreachNonZero(zipMap(v, w)(f))(k => sum = K.plus(sum, k))
    sum
  }

  def foreachNonZero[U](v: V)(f: K => U): Unit
  def foreachNonZeroWithIndex[U](v: V)(f: (I, K) => U): Unit
}
