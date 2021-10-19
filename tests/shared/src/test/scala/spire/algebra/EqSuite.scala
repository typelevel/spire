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
package algebra

import spire.std.int._

import org.scalacheck.Prop.forAll

class EqSuite extends munit.ScalaCheckSuite {

  property("Eq.by") {
    final case class A(asInt: Int)
    val eqA: Eq[A] = Eq.by(_.asInt)
    forAll { (x: Int, y: Int) =>
      eqA.eqv(A(x), A(y)) == (x == y)
    }
  }
}
