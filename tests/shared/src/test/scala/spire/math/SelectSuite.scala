/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
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
package math

import spire.algebra._
import spire.std.int._

trait SelectSuite extends munit.FunSuite {
  def selector: Select

  final def select[@sp A: Order: ClassTag](data: Array[A], k: Int) =
    selector.select(data, k)

  def shuffle[A: ClassTag](as: Array[A]): Array[A] =
    scala.util.Random.shuffle(as.toList).toArray

  test("selection in 0-length array") {
    // Shouldn't throw an exception.
    select(new Array[Int](0), 0)
  }

  test("select in 1-length array") {
    val as = Array(1)
    select(as, 0)
    assertEquals(as(0), 1)
  }

  test("select from multiple equal elements") {
    val as = Array(0, 0, 1, 1, 2, 2)
    select(as, 0); assertEquals(as(0), 0)
    select(as, 1); assertEquals(as(1), 0)
    select(as, 2); assertEquals(as(2), 1)
    select(as, 3); assertEquals(as(3), 1)
    select(as, 4); assertEquals(as(4), 2)
    select(as, 5); assertEquals(as(5), 2)
  }

  test("arbitrary selection") {
    (1 to 10).foreach { len =>
      val as = Array.range(0, len)

      (0 until len).foreach { i =>
        (1 to 5).foreach { _ =>
          val bs = shuffle(as)
          val orig = bs.clone()
          select(bs, i)
          assertEquals(bs(i), i, "Select %d on %s failed.".format(i, orig.mkString("[ ", ", ", " ]")))
        }
      }
    }
  }
}

class LinearSelectSuite extends SelectSuite {
  val selector = LinearSelect
}

class QuickSelectSuite extends SelectSuite {
  val selector = QuickSelect
}
