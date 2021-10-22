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

package spire.math

import java.math.BigInteger

import org.scalacheck.{Arbitrary, Gen, Prop}
import spire.algebra.NRoot
import spire.implicits._
import spire.compat._
import spire.laws.arb._

class BigIntegerNRootScalaCheckSuite extends munit.ScalaCheckSuite {

  private val rootGen: Gen[Int] = Gen.posNum[Int] :| "Root"
  private val bigIntegerGen: Gen[BigInteger] = Arbitrary.arbitrary[BigInteger].map(_.abs()) :| "Base"

  property("NRoot.nroot(n, k) yields the largest number whose k-th power is smaller than or equal to n.") {
    Prop.forAllNoShrink(bigIntegerGen, rootGen) { (x: BigInteger, k: Int) =>
      testRootProperApproximation(x, k, NRoot[BigInteger].nroot(x, k))
    }
  }

  def testRootProperApproximation(x: BigInteger, k: Int, rootX: BigInteger): Boolean =
    rootX.pow(k) <= x && x < (1 + rootX).pow(k)
}
