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
class Perm private(val mapping: Map[Int, Int]) extends (Int => Int) {

  override def apply(k: Int): Int = mapping.getOrElse(k, k)

  def apply(n0: Int, n1: Int, ns: Int*): Perm = Group[Perm].combine(this, Perm(n0, n1, ns: _*))

  override def toString: String = {
    mapping.toSeq.sorted
      .map { case (k, v) => s"$k -> $v"}
      .mkString("Perm(", ", ", ")")
  }
}

object Perm {
  def apply(mapping: Map[Int, Int]): Perm = {
    require(mapping.values.toSet == mapping.keySet, "Image and preimage must be the same.")
    require(mapping.keys.forall(_ >= 0), "Perm indices must be non-negative.")
    new Perm(mapping.filter(Function.tupled(_ != _)))
  }

  def apply(pairs: (Int, Int)*): Perm = apply(Map(pairs: _*))

  def apply(n0: Int, n1: Int, ns: Int*): Perm = {
    apply((n0 +: n1 +: ns).zip(n1 +: ns :+ n0).toMap)
  }

  implicit val PermEq: Eq[Perm] = new Eq[Perm] {
    def eqv(p: Perm, r: Perm): Boolean = p.mapping == r.mapping
  }
  implicit val PermIntAction: Action[Int, Perm] = new PermIntAction
  implicit val PermGroup: Group[Perm] = new PermGroup
  implicit def PermSeqPartialAction[A, CC[A] <: SeqLike[A, CC[A]]](implicit cbf: Factory[A, CC[A]]): PartialAction[CC[A], Perm] = new PermSeqPartialAction[A, CC[A]]
}

final class PermIntAction extends Action[Int, Perm] {
  def actr(k: Int, perm: Perm): Int = perm(k)
  def actl(perm: Perm, k: Int): Int = perm.mapping.find(_._2 == k).fold(k)(_._1)
}

final class PermGroup extends Group[Perm] {
  def empty: Perm = Perm(Map.empty[Int, Int])
  def combine(x: Perm, y: Perm): Perm = Perm {
    val preimages = x.mapping.keys ++ y.mapping.keys

    preimages.foldLeft(Map.empty[Int, Int]) {
      case (prevMap, preimage) =>
        val image = y(x(preimage))
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
      builder += sa(perm(k))
    }
    Opt(builder.result())
  }
  def partialActr(sa: SA, perm: Perm): Opt[SA] =
    partialActl(perm.inverse, sa)
}
