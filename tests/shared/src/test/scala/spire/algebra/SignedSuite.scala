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
package algebra

import spire.math.{Algebraic, Rational}
import spire.implicits._

class SignedSuite extends munit.FunSuite {
  def runWith[@sp(Int, Long, Float, Double) A: Signed: ClassTag](neg: A, pos: A, zero: A): Unit = {
    val m = implicitly[ClassTag[A]]

    // // the name to use for this A
    // val cls = m.typeArguments match {
    //  case Nil => m.erasure.getSimpleName
    //  case args => "%s[%s]" format (m.erasure.getSimpleName, args.mkString(","))
    // }

    // the name to use for this A
    val cls = m.runtimeClass.getName

    // test runner which constructs a unique name for each test we run.
    def runTest(name: String)(f: => Unit) = test("%s:%s".format(cls, name))(f)

    runTest("-neg.abs === pos")(assertEquals(neg.abs, pos))
    runTest("pos.abs === pos")(assertEquals(pos.abs, pos))
    runTest("neg.sign == Negative")(assertEquals(neg.sign, Signed.Negative))
    runTest("pos.sign == Positive")(assertEquals(pos.sign, Signed.Positive))
    runTest("zero.sign == Zero")(assertEquals(zero.sign, Signed.Zero))
    runTest("neg.signum < 0")(assert(neg.signum < 0))
    runTest("pos.signum > 0")(assert(pos.signum > 0))
    runTest("zero.signum == 0")(assertEquals(zero.signum, 0))
    runTest("zero.isSignZero")(assert(zero.isSignZero))
    runTest("neg.isSignNegative")(assert(neg.isSignNegative))
    runTest("pos.isSignPositive")(assert(pos.isSignPositive))
    runTest("neg.isSignNonZero")(assert(neg.isSignNonZero))
    runTest("pos.isSignNonZero")(assert(pos.isSignNonZero))
    runTest("pos.isSignNonNegative")(assert(pos.isSignNonNegative))
    runTest("zero.isSignNonNegative")(assert(zero.isSignNonNegative))
    runTest("neg.isSignNonPositive")(assert(neg.isSignNonPositive))
    runTest("zero.isSignNonPositive")(assert(zero.isSignNonPositive))
  }

  runWith[Int](-3, 3, 0)
  runWith[Long](-3, 3, 0)
  runWith[Float](-3, 3, 0)
  runWith[Double](-3, 3, 0)
  runWith[BigInt](-3, 3, 0)
  runWith[BigDecimal](-3, 3, 0)
  runWith[Rational](-3, 3, 0)
  runWith[Algebraic](-3, 3, 0)
}
