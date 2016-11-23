package spire
package optional

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

import spire.algebra.{Action, Group}
import spire.algebra.partial.PartialAction
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util._

final class MapIntIntIntAction extends Action[Int, Map[Int, Int]] {
  def actr(k: Int, perm: Map[Int, Int]): Int = perm.getOrElse(k, k)
  def actl(perm: Map[Int, Int], k: Int): Int = perm.find(_._2 == k).fold(k)(_._1)
}

final class MapIntIntGroup extends Group[Map[Int, Int]] {
  def id: Map[Int, Int] = Map.empty[Int, Int]
  def combine(x: Map[Int, Int], y: Map[Int, Int]): Map[Int, Int] = {
    val preimages = x.keys ++ y.keys

    (Map.empty[Int, Int] /: preimages) {
      case (prevMap, preimage) =>
        val imageX = x.getOrElse(preimage, preimage)
        val image = y.getOrElse(imageX, imageX)
        if (preimage != image) prevMap + ((preimage, image)) else prevMap
    }
  }
  def inverse(a: Map[Int, Int]): Map[Int, Int] = a.map(_.swap).toMap
}

final class MapIntIntSeqPartialAction[A, SA <: SeqLike[A, SA]](implicit cbf: CanBuildFrom[SA, A, SA]) extends PartialAction[SA, Map[Int, Int]] {
  import mapIntIntPermutation._
  def partialActl(perm: Map[Int, Int], sa: SA): Opt[SA] = {
    if (perm.isEmpty) return Opt(sa)
    if (perm.keys.max >= sa.size) return Opt.empty[SA]
    val builder = cbf()
    cforRange(0 until sa.size) { k =>
      builder += sa(perm.getOrElse(k, k))
    }
    Opt(builder.result())
  }
  def partialActr(sa: SA, perm: Map[Int, Int]): Opt[SA] =
    partialActl(perm.inverse, sa)
}

object mapIntIntPermutation {
  implicit val MapIntIntIntAction: Action[Int, Map[Int, Int]] = new MapIntIntIntAction
  implicit val MapIntIntGroup: Group[Map[Int, Int]] = new MapIntIntGroup
  implicit def MapIntIntSeqPartialAction[A, CC[A] <: SeqLike[A, CC[A]]](implicit cbf: CanBuildFrom[CC[A], A, CC[A]]): PartialAction[CC[A], Map[Int, Int]] = new MapIntIntSeqPartialAction[A, CC[A]]
}
