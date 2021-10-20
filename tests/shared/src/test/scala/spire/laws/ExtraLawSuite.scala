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

package spire.laws

import spire.math.extras.interval.{IntervalSeq, IntervalSeqArbitrary, IntervalTrie}
import spire.math.extras.interval.IntervalTrieArbitrary._
import spire.implicits._

class ExtraLawSuite extends munit.DisciplineSuite {
  import IntervalSeqArbitrary._

  checkAll("Bool[IntervalSeq[Int]]", LogicLaws[IntervalSeq[Int]].bool)
  checkAll("Bool[IntervalTrie[Long]]", LogicLaws[IntervalTrie[Long]].bool)
}
