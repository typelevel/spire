/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package optional

import scala.collection.Factory
import scala.collection.Seq
import scala.collection.SeqOps

import cats.kernel.Eq
import spire.algebra.{Action, Group}
import spire.algebra.partial.PartialAction
import spire.syntax.cfor._
import spire.util._

/**
 * Represents a permutation encoded as a map from preimages to images, including only pairs that are moved by the
 * permutation (so the identity is Map.empty).
 *
 * Note that although the preimage and image contain only moved points, the domain and range of a Perm is all integers.
 * This acts as the identity for integers not in the image.
 */
class Perm private (private val mapping: Map[Int, Int]) extends (Int => Int) {

  /**
   * Apply this permutation to an `Int`.
   */
  override def apply(k: Int): Int = mapping.getOrElse(k, k)

  /**
   * A `Perm` constructed by cycling args so each n,,i,, maps to n,,i+1,,.
   *
   * This new cycle is composed with the current permutation to yield a new `Perm`. Cycles provided to this constructor
   * must be disjoint. See [[Perm$.apply(n0:Int*]].
   */
  def apply(n0: Int, n1: Int, ns: Int*): Perm = {
    val cycle = n0 +: n1 +: ns
    require(!(cycle.exists(image)), "Cycle must be disjoint.")
    this.compose(Perm(n0, n1, ns: _*))
  }

  override def toString: String = {
    mapping.toSeq.sorted
      .map { case (k, v) => s"$k -> $v" }
      .mkString("Perm(", ", ", ")")
  }

  private lazy val inverseMapping = mapping.map(_.swap)

  /**
   * The preimage of `k` (so that `apply(invert(k)) == k`).
   */
  def invert(k: Int): Int = inverseMapping.getOrElse(k, k)

  /**
   * The inverse permutation, which composes with this to yield the identity. Note that this uses a lazy inverse map, so
   * the first call may be O(n) rather than O(1).
   */
  def inverse: Perm = new Perm(inverseMapping)

  /**
   * The set of points moved by this permutation (equivalent to preimage).
   */
  def image: Set[Int] = mapping.keySet

  /**
   * Permute a seq as long as all the moved points are valid indices.
   */
  def permute[A, SA](seq: SeqOps[A, Seq, SA])(implicit cbf: Factory[A, SA]): Opt[SA] = {
    if (image.isEmpty) return Opt(cbf.fromSpecific(seq))
    if (image.max >= seq.size) return Opt.empty[SA]
    val builder = cbf.newBuilder
    cforRange(0 until seq.size) { k =>
      builder += seq(invert(k))
    }
    Opt(builder.result())
  }

  /**
   * Compose this with another `Perm` with this permutation applied last.
   */
  def compose(that: Perm): Perm = new Perm(
    (this.image | that.image)
      .map(k => k -> this(that(k)))
      .filter(Function.tupled(_ != _))
      .toMap
  )

  /**
   * Compose this with another `Perm` with this permutation applied first.
   */
  def andThen(that: Perm): Perm = that.compose(this)
}

object Perm {

  /**
   * A `Perm` given a `Map[Int, Int]` from preimage to image.
   */
  def apply(mapping: Map[Int, Int]): Perm = {
    require(mapping.values.toSet == mapping.keySet, "Image and preimage must be the same.")
    require(mapping.keys.forall(_ >= 0), "Perm indices must be non-negative.")
    new Perm(mapping.filter(Function.tupled(_ != _)))
  }

  /**
   * A `Perm` given a collection of preimage/image pairs.
   */
  def apply(pairs: (Int, Int)*): Perm = apply(Map(pairs: _*))

  /**
   * A `Perm` constructed by cycling args so each n,,i,, maps to n,,i+1,,.
   *
   * [[Perm#apply(n0:Int*]] can be called subsequently to express any `Perm` as the product of cycles rather than
   * explicit preimage/image pairs. This is provided as a convenience for constructing permutations. eg.,
   * `Perm(1,3)(2,4)` is shorthand for `Perm(1 -> 3, 2 -> 4, 3 -> 1, 4 -> 2)`. At least two args are required in each
   * parameter list.
   */
  def apply(n0: Int, n1: Int, ns: Int*): Perm = {
    val cycles = n0 +: n1 +: ns
    require(cycles.size == cycles.distinct.size, "Cycle must not repeat elements")
    apply((n0 +: n1 +: ns).zip(n1 +: ns :+ n0).toMap)
  }

  implicit val PermEq: Eq[Perm] = new Eq[Perm] {
    def eqv(p: Perm, r: Perm): Boolean = p.mapping == r.mapping
  }
  implicit val PermIntAction: Action[Int, Perm] = new PermIntAction
  implicit val PermGroup: Group[Perm] = new PermGroup
  implicit def PermSeqPartialAction[A, CC[A] <: SeqOps[A, Seq, CC[A]]](implicit
    cbf: Factory[A, CC[A]]
  ): PartialAction[CC[A], Perm] = new PermSeqPartialAction[A, CC[A]]
}

final class PermIntAction extends Action[Int, Perm] {
  def actl(perm: Perm, k: Int): Int = perm(k)
  def actr(k: Int, perm: Perm): Int = perm.invert(k)
}

final class PermGroup extends Group[Perm] {
  def empty: Perm = Perm(Map.empty[Int, Int])
  def combine(x: Perm, y: Perm): Perm = x.compose(y)
  def inverse(a: Perm): Perm = a.inverse
}

final class PermSeqPartialAction[A, SA <: SeqOps[A, Seq, SA]](implicit cbf: Factory[A, SA])
    extends PartialAction[SA, Perm] {
  def partialActl(perm: Perm, sa: SA): Opt[SA] = perm.permute[A, SA](sa)
  def partialActr(sa: SA, perm: Perm): Opt[SA] = partialActl(perm.inverse, sa)
}
