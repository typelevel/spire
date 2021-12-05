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

import scala.util.Random
import Random._
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import Arrays.init

import spire.implicits._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class PowBenchmarks {

  var longs: Array[Long] = null
  var ints: Array[Int] = null
  var doubles: Array[Double] = null

  @Setup
  def setUp(): Unit = {
    val l = 200000
    ints = init(l)(nextInt)
    longs = init(l)(nextLong)
    doubles = init(l)(nextDouble)
  }

  @Benchmark
  def timeLongPowForInt(): Int = {
    var t = 0
    ints.foreach { n =>
      t += spire.math.pow(n.toLong, 2.toLong).toInt
    }
    t
  }

  @Benchmark
  def timeDoublePowForInt(): Int = {
    var t = 0
    ints.foreach { n =>
      t += spire.math.pow(n.toDouble, 2.0).toInt
    }
    t
  }

  @Benchmark
  def timeBigIntPowForInt(): Int = {
    var t = 0
    ints.foreach { n =>
      t += BigInt(n).pow(2).toInt
    }
    t
  }

  @Benchmark
  def timeLongPowForLong(): Long = {
    var t = 0L
    longs.foreach { n =>
      t += spire.math.pow(n, 2L)
    }
    t
  }

  @Benchmark
  def timeDoublePowForLong(): Long = {
    var t = 0L
    longs.foreach { n =>
      t += spire.math.pow(n.toDouble, 2.0).toLong
    }
    t
  }

  @Benchmark
  def timeBigIntPowForLong(): Long = {
    var t = 0L
    longs.foreach { n =>
      t += BigInt(n).pow(2).toLong
    }
    t
  }

  @Benchmark
  def timeDoublePowForDouble(): Double = {
    var t = 0.0
    doubles.foreach { n =>
      t += spire.math.pow(n, 2.0)
    }
    t
  }
}
