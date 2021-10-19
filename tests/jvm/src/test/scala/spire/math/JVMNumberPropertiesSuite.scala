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
package math

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class JVMNumberPropertiesSuite extends munit.ScalaCheckSuite {
  def bothEq[A, B](a: A, b: B) = {
    a == b && b == a
  }

  property("RationalNumber == Double") {
    forAll { (n: Double) => bothEq(Number(Rational(n)), n) }
  }

}
