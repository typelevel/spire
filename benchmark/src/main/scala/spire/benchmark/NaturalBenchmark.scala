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

import org.openjdk.jmh.annotations._
import spire.implicits._
import spire.math._

import java.lang.Math
import java.util.concurrent.TimeUnit
import scala.util.Random

import Arrays.init

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class NaturalBenchmarks {
  @Param(Array("8", "16", "32", "64", "128"))
  var bits: Int = 0

  // @Param(Array("10", "15", "20"))
  @Param(Array("10"))
  var pow: Int = 0

  var nats: Array[Natural] = null
  var bigints: Array[BigInt] = null
  var safes: Array[SafeLong] = null

  @Setup
  def setUp(): Unit = {
    val size = Math.pow(2, pow).toInt
    bigints = init(size)(BigInt(bits, Random))
    nats = bigints.map(Natural(_))
    safes = bigints.map(SafeLong(_))
  }

  @Benchmark
  def timeNaturalSum() = nats.qsum

  @Benchmark
  def timeBigIntSum() = bigints.qsum
  @Benchmark
  def timeSafeLongSums() = safes.qsum

  @Benchmark
  def timeNaturalSumDoubles() = nats.map(n => n << 1).qsum

  @Benchmark
  def timeBigIntSumDoubles() = bigints.map(n => n << 1).qsum
  @Benchmark
  def timeSafeLongSumDoubles() = safes.map(n => n * 2).qsum
  @Benchmark
  def timeNaturalSumSquares() = nats.map(n => n * n).qsum
  @Benchmark
  def timeBigIntSumSquares() = bigints.map(n => n * n).qsum
  @Benchmark
  def timeSafeLongSumSquares() = safes.map(n => n * n).qsum

  @Benchmark
  def timeNaturalSumNormalized() = nats.map(n => n / UInt(10)).qsum
  @Benchmark
  def timeBigIntSumNormalized() = bigints.map(n => n / 10).qsum
  @Benchmark
  def timeSafeLongSumNormalized() = safes.map(n => n / 10).qsum
  @Benchmark
  def timeNaturalMin() = nats.qmin
  @Benchmark
  def timeBigIntMin() = bigints.qmin
  @Benchmark
  def timeSafeLongMin() = safes.qmin
}
