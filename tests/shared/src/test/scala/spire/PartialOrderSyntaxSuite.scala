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

import spire.algebra._
import spire.math.Searching

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object IntDivisibility extends PartialOrder[Int] {
  def partialCompare(a: Int, b: Int) =
    if (a == b) 0.0
    else if (b % a == 0) -1.0
    else if (a % b == 0) 1.0
    else Double.NaN
}

class PartialOrderSyntaxSuite extends munit.ScalaCheckSuite {

  test("minimal elements of {2,3,6,9,12} are {2,3} under divisibility") {
    import spire.syntax.std.seq._
    assertEquals(Seq(2, 3, 6, 9, 12).pmin(IntDivisibility).toSet, Set(2, 3))
  }

  test("maximal elements of {2,3,6,9,12} are {2,3} under divisibility") {
    import spire.syntax.std.seq._
    assertEquals(Seq(2, 3, 6, 9, 12).pmax(IntDivisibility).toSet, Set(9, 12))
  }

  case class PosInt(x: Int)

  implicit def ArbPosInt: Arbitrary[PosInt] =
    Arbitrary(Gen.choose(1, 30).map(PosInt.apply))

  def isMinimal(seq: Seq[Int], i: Int): Boolean =
    seq.forall(j => !(IntDivisibility.partialCompare(i, j) > 0))
  def isMaximal(seq: Seq[Int], i: Int): Boolean =
    seq.forall(j => !(IntDivisibility.partialCompare(i, j) < 0))

  property("pmin") {
    forAll { (posSeq: Seq[PosInt]) =>
      import spire.syntax.std.seq._
      val seq = posSeq.map(_.x)
      val result = seq.pmin(IntDivisibility).toSet
      assertEquals(result, Searching.minimalElements(seq)(IntDivisibility).toSet)
      assertEquals(result, seq.filter(i => isMinimal(seq, i)).toSet)
    }
  }

  property("pmax") {
    forAll { (posSeq: Seq[PosInt]) =>
      import spire.syntax.std.seq._
      val seq = posSeq.map(_.x)
      val result = seq.pmax(IntDivisibility).toSet
      assertEquals(result, Searching.minimalElements(seq)(PartialOrder.reverse(IntDivisibility)).toSet)
      assertEquals(result, seq.filter(i => isMaximal(seq, i)).toSet)
    }
  }
}
