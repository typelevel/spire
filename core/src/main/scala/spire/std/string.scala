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
package std

import spire.algebra.{MetricSpace, Monoid, Order}

@SerialVersionUID(0L)
class StringMonoid extends Monoid[String] with Serializable {
  def empty: String = ""
  def combine(x: String, y: String): String = x + y

  override def combineAll(xs: IterableOnce[String]): String = {
    val sb = new StringBuilder
    xs.iterator.foreach(sb ++= _)
    sb.result()
  }
}

@SerialVersionUID(0L)
class StringOrder extends Order[String] with Serializable {
  override def eqv(x: String, y: String): Boolean = x == y
  override def neqv(x: String, y: String): Boolean = x != y
  def compare(x: String, y: String): Int = x.compareTo(y)
}

@SerialVersionUID(0L)
object LevenshteinDistance extends MetricSpace[String, Int] with Serializable {
  import spire.syntax.cfor._

  def distance(a: String, b: String): Int = {
    var row0 = new Array[Int](b.length + 1)
    var row1 = new Array[Int](b.length + 1)

    cfor(0)(_ < row0.length, _ + 1)(j => row0(j) = j)

    cfor(0)(_ < a.length, _ + 1) { i =>
      row1(0) = i + 1
      val c = a.charAt(i)
      cfor(1)(_ < row1.length, _ + 1) { j =>
        val d = row0(j - 1) + (if (c == b.charAt(j - 1)) 0 else 1)
        val h = row1(j - 1) + 1
        val v = row0(j) + 1

        row1(j) = if (d < h) {
          if (v < d) v else d
        } else {
          if (v < h) v else h
        }
      }

      val tmp = row0; row0 = row1; row1 = tmp
    }

    row0(b.length)
  }
}

trait StringInstances0 {
  implicit def levenshteinDistance: MetricSpace[String, Int] = LevenshteinDistance
}

trait StringInstances extends StringInstances0 {
  implicit final val StringAlgebra: Monoid[String] = new StringMonoid
  implicit final val StringOrder: Order[String] = new StringOrder
}
