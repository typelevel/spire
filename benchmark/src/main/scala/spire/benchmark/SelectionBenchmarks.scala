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
import spire.algebra._
import spire.implicits._
import spire.math._
import Arrays._
import spire.benchmark.ArrayOrder

import java.util.concurrent.TimeUnit
import scala.util.Random

import Random._
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class SelectionBenchmarks {
  implicit val lexicographic: Order[Complex[Double]] = new Order[Complex[Double]] {
    override def eqv(a: Complex[Double], b: Complex[Double]) = a == b
    def compare(a: Complex[Double], b: Complex[Double]): Int = {
      if (a.real < b.real) -1
      else if (a.real > b.real) 1
      else if (a.imag < b.imag) -1
      else if (a.imag > b.imag) 1
      else 0
    }
  }

  @Param(Array("3", "4", "6", "9", "13", "18"))
  var pow: Int = 0

  // @Param(Array("random", "sorted", "reversed"))
  @Param(Array("random"))
  var layout: String = null

  var is: Array[Int] = null
  var js: Array[Long] = null
  var fs: Array[Float] = null
  var ds: Array[Double] = null
  var cs: Array[Complex[Double]] = null
  var cs2: Array[FakeComplex[Double]] = null

  def nextComplex: Complex[Double] = Complex(nextDouble(), nextDouble())

  @Setup
  def setUp(): Unit = {
    val size = spire.math.pow(2, pow).toInt
    def complexToFake(c: Complex[Double]) = new FakeComplex(c.real, c.imag)

    is = mkarray(size, ArrayOrder.Random)(nextInt)
    js = mkarray(size, ArrayOrder.Random)(nextLong)
    fs = mkarray(size, ArrayOrder.Random)(nextFloat)
    ds = mkarray(size, ArrayOrder.Random)(nextDouble)
    cs = mkarray(size, ArrayOrder.Random)(nextComplex)
    cs2 = cs.map(complexToFake)
  }
  @Benchmark
  def timeSpireQuickSelectInt() = {
    spire.math.Selection.quickSelect(is, is.length / 2)
  }

  @Benchmark
  def timeSpireQuickSelectLong() = {
    spire.math.Selection.quickSelect(js, js.length / 2)
  }

  @Benchmark
  def timeSpireQuickSelectFloat() = {
    spire.math.Selection.quickSelect(fs, fs.length / 2)
  }

  @Benchmark
  def timeSpireQuickSelectDouble() = {
    spire.math.Selection.quickSelect(ds, ds.length / 2)
  }

  @Benchmark
  def timeSpireQuickSelectComplex() = {
    spire.math.Selection.quickSelect(cs, cs.length / 2)
  }

  @Benchmark
  def timeSpireLinearSelectInt() = {
    spire.math.Selection.linearSelect(is, is.length / 2)
  }

  @Benchmark
  def timeSpireLinearSelectLong() = {
    spire.math.Selection.linearSelect(js, js.length / 2)
  }

  @Benchmark
  def timeSpireLinearSelectFloat() = {
    spire.math.Selection.linearSelect(fs, fs.length / 2)
  }

  @Benchmark
  def timeSpireLinearSelectDouble() = {
    spire.math.Selection.linearSelect(ds, ds.length / 2)
  }

  @Benchmark
  def timeSpireLinearSelectComplex() = {
    spire.math.Selection.linearSelect(cs, cs.length / 2)
  }

}
