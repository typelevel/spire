package spire
package optional

import spire.scalacompat.{Factory, FactoryCompatOps, SeqLike}

import cats.kernel.Eq
import spire.algebra.{Action, Group}
import spire.algebra.partial.PartialAction
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util._

/**
 * Represents a permutation encoded as a map from preimages to images, including
 * only pairs that are moved by the permutation (so the identity is Map.empty).
 */
case class Perm(mapping: Map[Int, Int])

object Perm {
  implicit val PermEq: Eq[Perm] = Eq.fromUniversalEquals
  implicit val PermIntAction: Action[Int, Perm] = new PermIntAction
  implicit val PermGroup: Group[Perm] = new PermGroup
  implicit def PermSeqPartialAction[A, CC[A] <: SeqLike[A, CC[A]]](implicit cbf: Factory[A, CC[A]]): PartialAction[CC[A], Perm] = new PermSeqPartialAction[A, CC[A]]
}

final class PermIntAction extends Action[Int, Perm] {
  def actr(k: Int, perm: Perm): Int = perm.mapping.getOrElse(k, k)
  def actl(perm: Perm, k: Int): Int = perm.mapping.find(_._2 == k).fold(k)(_._1)
}

final class PermGroup extends Group[Perm] {
  def empty: Perm = Perm(Map.empty[Int, Int])
  def combine(x: Perm, y: Perm): Perm = Perm {
    val preimages = x.mapping.keys ++ y.mapping.keys

    preimages.foldLeft(Map.empty[Int, Int]) {
      case (prevMap, preimage) =>
        val imageX = x.mapping.getOrElse(preimage, preimage)
        val image = y.mapping.getOrElse(imageX, imageX)
        if (preimage != image) prevMap + ((preimage, image)) else prevMap
    }
  }
  def inverse(a: Perm): Perm = Perm(a.mapping.map(_.swap))
}

final class PermSeqPartialAction[A, SA <: SeqLike[A, SA]](implicit cbf: Factory[A, SA]) extends PartialAction[SA, Perm] {
  def partialActl(perm: Perm, sa: SA): Opt[SA] = {
    if (perm.mapping.isEmpty) return Opt(sa)
    if (perm.mapping.keys.max >= sa.size) return Opt.empty[SA]
    val builder = cbf.newBuilder
    cforRange(0 until sa.size) { k =>
      builder += sa(perm.mapping.getOrElse(k, k))
    }
    Opt(builder.result())
  }
  def partialActr(sa: SA, perm: Perm): Opt[SA] =
    partialActl(perm.inverse, sa)
}
