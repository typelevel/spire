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
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class AddBenchmarks {

  def addGeneric[@sp(Int, Long, Float, Double) A: Ring](data: Array[A], bh: Blackhole): Unit = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) {
      bh.consume { total = Ring[A].plus(total, data(i)) }
      i += 1
    }
  }

  def addFastComplex(data: Array[Long], bh: Blackhole): Unit = {
    var total = FastComplex(0.0f, 0.0f)
    var i = 0
    val len = data.length
    while (i < len) {
      bh.consume { total = FastComplex.add(total, data(i)) }
      i += 1
    }
  }

  @Benchmark
  def addIntDirect(state: IntState, bh: Blackhole): Unit = {
    val data = state.values
    var total = 0
    var i = 0
    val len = data.length
    while (i < len) {
      bh.consume { total += data(i) }
      i += 1
    }
  }
  @Benchmark
  def addIntGeneric(state: IntState, bh: Blackhole): Unit = addGeneric(state.values, bh)

  @Benchmark
  def addLongDirect(state: LongState, bh: Blackhole): Unit = {
    val data = state.values
    var total = 0L
    var i = 0
    val len = data.length
    while (i < len) {
      bh.consume { total += data(i) }
      i += 1
    }
  }
  @Benchmark
  def addLongGeneric(state: LongState, bh: Blackhole): Unit = addGeneric(state.values, bh)

  @Benchmark
  def addFloatDirect(state: FloatState, bh: Blackhole): Float = {
    val data = state.values
    var total = 0.0f
    var i = 0
    val len = data.length
    while (i < len) { bh.consume { total += data(i) }; i += 1 }
    total

  }
  @Benchmark
  def addFloatGeneric(state: FloatState, bh: Blackhole): Unit = addGeneric(state.values, bh)

  @Benchmark
  def addDoubleDirect(state: DoubleState, bh: Blackhole): Double = {
    val data = state.values
    var total = 0.0
    var i = 0
    val len = data.length
    while (i < len) { bh.consume { total += data(i) }; i += 1 }
    total
  }
  @Benchmark
  def addDoublesGeneric(state: DoubleState, bh: Blackhole): Unit = addGeneric(state.values, bh)

  @Benchmark
  def addComplexFloatStateDirect(state: ComplexFloatState, bh: Blackhole): Complex[Float] = {
    val data = state.values
    var total = Complex.zero[Float]
    var i = 0
    val len = data.length
    while (i < len) { bh.consume { total += data(i) }; i += 1 }
    total
  }

  @Benchmark
  def addComplexFloatStateGeneric(state: ComplexFloatState, bh: Blackhole): Unit = addGeneric(state.values, bh)

  @Benchmark
  def addComplexesDirect(state: ComplexFloatState, bh: Blackhole): Complex[Float] = {
    var total = Complex.zero[Float]
    var i = 0
    val len = state.values.length
    while (i < len) { bh.consume { total += state.values(i) }; i += 1 }
    total
  }

  @Benchmark
  def addFastComplexDirect(state: FastComplexState, bh: Blackhole): Long = {
    val data = state.values
    var total = FastComplex(0.0f, 0.0f)
    var i = 0
    val len = data.length
    while (i < len) { bh.consume { total = FastComplex.add(total, data(i)) }; i += 1 }
    total
  }

  @Benchmark
  def addComplexDoubleStateDirect(state: ComplexDoubleState, bh: Blackhole): Complex[Double] = {
    val data = state.values
    var total = Complex.zero[Double]
    var i = 0
    val len = data.length
    while (i < len) { bh.consume { total += data(i) }; i += 1 }
    total
  }

  @Benchmark
  def addComplexDoubleStateGeneric(state: ComplexDoubleState, bh: Blackhole): Unit = addGeneric(state.values, bh)

}
