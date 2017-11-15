package spire
package optional

import spire.algebra.{Action, Group}

final class MapIntIntIntAction extends Action[Int, Map[Int, Int]] {
  def actr(k: Int, perm: Map[Int, Int]): Int = perm.getOrElse(k, k)
  def actl(perm: Map[Int, Int], k: Int): Int = perm.find(_._2 == k).fold(k)(_._1)
}

final class MapIntIntGroup extends Group[Map[Int, Int]] {
  def empty: Map[Int, Int] = Map.empty[Int, Int]
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

object mapIntIntPermutation {
  implicit val MapIntIntIntAction: Action[Int, Map[Int, Int]] = new MapIntIntIntAction
  implicit val MapIntIntGroup: Group[Map[Int, Int]] = new MapIntIntGroup
}
