package spire.algebra

import scala.{ specialized => spec }

/**
 * A builder for vectors `V` with elements of `K`, indexed by `I`.
 *
 * TODO: Move to a different package?
 */
trait VectorBuilder[V, @spec(Int, Long, Float, Double) K, @spec(Int, Long) I] {
  type State
  def init: State
  def update(s: State, i: I, k: K): State
  def result(s: State): V
}

