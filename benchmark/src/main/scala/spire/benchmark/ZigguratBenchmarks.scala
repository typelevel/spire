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
package benchmark
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import spire.implicits._

/**
 * This is a benchmark comparing Marsaglias Polar Method implementation with the implementation of his Ziggurat
 * algorithm.
 *
 * <p><b>Reference: </b> George Marsaglia, Wai Wan Tsang: "The Ziggurat Method for Generating Random Variables"
 * <i>Journal of Statistical Software,</i> Vol. 5, Issue 8, October 2000.
 *
 * @see
 *   <a href="http://www.jstatsoft.org/v05/i08">Ziggurat Paper</a>
 * @see
 *   <a href="http://en.wikipedia.org/wiki/Ziggurat_algorithm">Ziggurat algorithm @ Wikipedia</a>
 * @author
 *   <a href="mailto:dusan.kysel@gmail.com">Du&#x0161;an Kysel</a>
 */

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class ZigguratBenchmarks {
  val well512aRng: spire.random.Generator = spire.random.rng.Well512a.fromTime()
  val mg = new spire.random.MarsagliaGaussian[Double]
  val gaussDist = mg(0d, 1d)
  @inline final def len = 10000000

  @Benchmark
  def timePolarRNORGenerator(): Double = {
    var total = 0d
    var i = 0
    while (i < len) {
      total += well512aRng.nextGaussian(0d, 1d)
      i += 1
    }
    total
  }

  @Benchmark
  def timePolarRNORDist(): Double = {
    val rng = well512aRng
    var total = 0d
    var i = 0
    while (i < len) {
      total += gaussDist(rng)
      i += 1
    }
    total
  }

  @Benchmark
  def timeZigguratRNOR(): Double = {
    val rng = well512aRng
    var total = 0d
    var i = 0
    while (i < len) {
      total += spire.random.Ziggurat.rnor(rng)
    }
    total
  }

  @Benchmark
  def timeZigguratREXP(): Double = {
    val rng = well512aRng
    var total = 0d
    var i = 0
    while (i < len) {
      total += spire.random.Ziggurat.rexp(rng)
    }
    total
  }
}
