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
package benchmark.jmh
import java.util.concurrent.TimeUnit

import scala.util.Random
import Random._
import org.openjdk.jmh.annotations._
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.benchmark.Arrays.init
import spire.benchmark.Arrays.FloatState

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class ComplexAddBenchmarks {
  @Param(Array("500000", "1000000", "2000000", "4000000", "8000000", "16000000"))
  var size: Int = 0

  var complexes: Array[Complex[Float]] = null
  var longs: Array[Long] = null
  var fcs: Array[FloatComplex] = null

  @Setup
  def setup(): Unit = {
    complexes = init(size)(Complex(nextFloat(), nextFloat()))
    longs = init(size)(FastComplex(nextFloat(), nextFloat()))
    fcs = init(size)(FloatComplex(nextFloat(), nextFloat()))
  }

  def addGeneric[@sp(Float) A: Ring](data: Array[A]): A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
    total
  }

  @Benchmark
  def addFloatGeneric(state: FloatState): Float = addGeneric(state.values)

  @Benchmark
  def addComplexesDirect(): Complex[Float] = {
    var total = Complex.zero[Float]
    var i = 0
    val len = complexes.length
    while (i < len) { total += complexes(i); i += 1 }
    total
  }

  @Benchmark
  def addFastComplexes(): Long = {
    var total = FastComplex(0.0f, 0.0f)
    var i = 0
    val len = longs.length
    while (i < len) { total = FastComplex.add(total, longs(i)); i += 1 }
    total
  }

  @Benchmark
  def addFloatComplexesBoxed(): FloatComplex = {
    var total = FloatComplex(0.0f, 0.0f)
    var i = 0
    val len = fcs.length
    while (i < len) { total += fcs(i); i += 1 }
    total
  }

  @Benchmark
  def addFloatComplexesUnboxed(): FloatComplex = {
    var total = FloatComplex(0.0f, 0.0f)
    var i = 0
    val len = fcs.length
    while (i < len) { total += new FloatComplex(longs(i)); i += 1 }
    total
  }

}
