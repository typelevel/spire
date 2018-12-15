/************************************************************************\
** Project                                                              **
**       ______  ______   __    ______    ____                          **
**      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2014        **
**     / /__   / /_/ /  / /   / /_/ /   / /_                            **
**    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
**   ____/ / / /      / /   / / | |   / /__                             **
**  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
**                                                                      **
**      Redistribution and use permitted under the MIT license.         **
**                                                                      **
\************************************************************************/


package spire
package benchmark

import spire.implicits._
/*
/**
 * This is a benchmark comparing Marsaglias Polar Method implementation with the implementation of his Ziggurat algorithm.
 *
 * <p><b>Reference: </b>
 * George Marsaglia, Wai Wan Tsang:
 * "The Ziggurat Method for Generating Random Variables"
 * <i>Journal of Statistical Software,</i> Vol. 5, Issue 8, October 2000.
 *
 * @see <a href="http://www.jstatsoft.org/v05/i08">Ziggurat Paper</a>
 * @see <a href="http://en.wikipedia.org/wiki/Ziggurat_algorithm">Ziggurat algorithm @ Wikipedia</a>
 * @author <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */
object ZigguratBenchmarks extends MyRunner(classOf[ZigguratBenchmarks])

class ZigguratBenchmarks extends MyBenchmark with BenchmarkData {

  val well512aRng: spire.random.Generator = spire.random.rng.Well512a.fromTime()
  val mg = new spire.random.MarsagliaGaussian[Double]
  val gaussDist = mg(0d, 1d)

  @inline final def nextLen = 10000000

  def timePolarRNORGenerator(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0d
    cfor(0)(_ < nextLen, _ + 1)(_ => t += rng.nextGaussian())
  }

  def timePolarRNORDist(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0d
    cfor(0)(_ < nextLen, _ + 1)(_ => t += gaussDist(rng))
  }

  def timeZigguratRNOR(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0d
    cfor(0)(_ < nextLen, _ + 1)(_ => t += spire.random.Ziggurat.rnor(rng))
  }

  def timeZigguratREXP(reps: Int) = run(reps) {
    val rng = well512aRng
    var t = 0d
    cfor(0)(_ < nextLen, _ + 1)(_ => t += spire.random.Ziggurat.rexp(rng))
  }
}
*/