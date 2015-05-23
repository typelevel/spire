package spire.util

/**
 * A class that can perform joins on maps. When joining maps there are 3 main
 * cases:
 *
 * 1. An element exists in both maps at index `i`.
 * 2. An element exists in the left map at index `i`, but not the right.
 * 3. An element exists in the right map at index `i`, but not the left.
 *
 * While a [[MapJoin.Custom]] join can handle all of these cases, it is also
 * the least efficient. [[MapJoin.Keep]] and [[MapJoin.Drop]] provide a simple
 * behaviour for cases 2 and 3 that is common in arithmetic operations, namely
 * either keeping the element that exists or dropping it (resp). These can be
 * performed more efficiently, and should be used if either of those behaviours
 * is acceptable (eg. adding maps as vectors can use `Keep`, while multiplying
 * maps as vectors can use `Drop`).
 */
sealed abstract class MapJoin[I, K] {
  import MapJoin._

  /**
   * Returns a `MapJoin[I, K]` that swaps the order of the arguments provided
   * to it. That is, `this.swapped(w, v) === this(v, w)`.
   */
  def swapped: MapJoin[I, K] = this match {
    case Drop(f) => Drop { (i, k0, k1) => f(i, k1, k0) }
    case Keep(f) => Keep { (i, k0, k1) => f(i, k1, k0) }
    case Custom(l, r, f) => Custom(r, l, { (i, k0, k1) => f(i, k1, k0) })
  }

  /**
   * Apply the join to the maps `v` and `w`.
   */
  def apply(v: Map[I, K], w: Map[I, K]): Map[I, K] = {
    if (v.size < w.size) // TODO: Set a minimum treshold for this.
      return swapped(w, v)

    def inner(init: Map[I, K], f: (I, K, K) => K): Map[I, K] =
      w.foldLeft(init) { case (u, (i, k0)) =>
        v.get(i).map(k1 => u.updated(i, f(i, k0, k1))).getOrElse(u)
      }

    def outer(l: (I, K) => K, r: (I, K) => K, f: (I, K, K) => K): Map[I, K] = {
      val u0 = v.foldLeft(Map.empty[I, K]) { case (u, (i, k0)) =>
        val k2 = w.get(i).map(k1 => f(i, k0, k1)).getOrElse(l(i, k0))
        u.updated(i, k2)
      }
      w.foldLeft(u0) { case (u, (i, k1)) =>
        if (v contains i) u else u.updated(i, r(i, k1))
      }
    }

    this match {
      case Custom(l, r, f) => outer(l, r, f)
      case Keep(f) => inner(v, f)
      case Drop(f) => inner(Map.empty, f)
    }
  }
}

object MapJoin {

  /** An inner join that drops unmatched elements on either side. */
  case class Drop[I, K](f: (I, K, K) => K) extends MapJoin[I, K]

  /** An inner join that keeps the unmatched element without change. */
  case class Keep[I, K](f: (I, K, K) => K) extends MapJoin[I, K]

  /** An inner join that handles each case separately. */
  case class Custom[I, K](l: (I, K) => K, r: (I, K) => K, f: (I, K, K) => K) extends MapJoin[I, K]
}
