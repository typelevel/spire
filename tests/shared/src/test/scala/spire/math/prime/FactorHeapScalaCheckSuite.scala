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
package math.prime

import spire.math.SafeLong

import spire.implicits._
import org.scalacheck.Prop._

class FactorHeapScalaCheckSuite extends munit.ScalaCheckSuite {

  import SieveUtil._

  property("heap property") {
    forAll { (ns0: List[Int]) =>
      val ns = ns0.map(n => Factor(n, n))
      val sorted = ns.sorted.map(_.next)

      val h = new FactorHeap
      ns.foreach(h += _)
      val sizeCheck = h.size == ns.length

      var result = List.empty[SafeLong]
      while (h.nonEmpty) result = h.dequeue().next :: result

      sizeCheck && result == sorted
    }
  }
}
