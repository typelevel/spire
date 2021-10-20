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
import spire.algebra._
import spire.benchmark.Arrays._
import spire.implicits._
import spire.math._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class AddBenchmarks {

  def addGeneric[@sp(Int, Long, Float, Double) A: Ring](data: Array[A]): A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
    total
  }

  def addFastComplex(data: Array[Long]): Long = {
    var total = FastComplex(0.0f, 0.0f)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.add(total, data(i)); i += 1 }
    total
  }

  @Benchmark
  def addIntDirect(state: IntState): Int = {
    val data = state.values
    var total = 0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }
  @Benchmark
  def addIntGeneric(state: IntState): Int = addGeneric(state.values)

  @Benchmark
  def addLongDirect(state: LongState): Long = {
    val data = state.values
    var total = 0L
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }
  @Benchmark
  def addLongGeneric(state: LongState): Long = addGeneric(state.values)

  @Benchmark
  def addFloatDirect(state: FloatState): Float = {
    val data = state.values
    var total = 0.0f
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total

  }
  @Benchmark
  def addFloatGeneric(state: FloatState): Float = addGeneric(state.values)

  @Benchmark
  def addDoubleDirect(state: DoubleState): Double = {
    val data = state.values
    var total = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }
  @Benchmark
  def addDoublesGeneric(state: DoubleState): Double = addGeneric(state.values)

  @Benchmark
  def addComplexFloatStateDirect(state: ComplexFloatState): Complex[Float] = {
    val data = state.values
    var total = Complex.zero[Float]
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }
  @Benchmark
  def addComplexFloatStateGeneric(state: ComplexFloatState): Complex[Float] = addGeneric(state.values)

  @Benchmark
  def addFastComplexDirect(state: FastComplexState): Long = {
    val data = state.values
    var total = FastComplex(0.0f, 0.0f)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.add(total, data(i)); i += 1 }
    total
  }

  @Benchmark
  def addComplexDoubleStateDirect(state: ComplexDoubleState): Complex[Double] = {
    val data = state.values
    var total = Complex.zero[Double]
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  @Benchmark
  def addComplexDoubleStateGeneric(state: ComplexDoubleState): Complex[Double] = addGeneric(state.values)

}
