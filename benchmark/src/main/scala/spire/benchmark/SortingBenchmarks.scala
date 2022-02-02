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

import spire.algebra._
import spire.math._
import spire.implicits._
import Arrays._
import spire.benchmark.ArrayOrder

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

final class FakeComplex[@sp(Float, Double) T](val real: T, val imag: T)(implicit f: Fractional[T], t: Trig[T])
    extends Ordered[FakeComplex[T]] {
  def compare(b: FakeComplex[T]): Int = {
    if (f.lt(real, b.real)) -1
    else if (f.gt(real, b.real)) 1
    else if (f.lt(imag, b.imag)) -1
    else if (f.gt(imag, b.imag)) 1
    else 0
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class SortingBenchmarks {
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

  // @Param(Array("4", "6", "8", "10", "12", "14", "16", "18", "20"))
  @Param(Array("3", "4", "6", "9", "13", "18"))
  var pow: Int = 0

  var is: Array[Int] = null
  var js: Array[Long] = null
  var fs: Array[Float] = null
  var ds: Array[Double] = null
  var cs: Array[Complex[Double]] = null
  var cs2: Array[FakeComplex[Double]] = null

  @Setup
  def setUp(): Unit = {
    def complexToFake(c: Complex[Double]) = new FakeComplex(c.real, c.imag)

    is = mkarray(size, ArrayOrder.Random)(nextInt)
    js = mkarray(size, ArrayOrder.Random)(nextLong)
    fs = mkarray(size, ArrayOrder.Random)(nextFloat)
    ds = mkarray(size, ArrayOrder.Random)(nextDouble)
    cs = mkarray(size, ArrayOrder.Random)(nextComplexDouble())
    cs2 = cs.map(complexToFake)
  }

  @Benchmark
  def timeJavaSortInt() = {
    java.util.Arrays.sort(is.clone)
  }

  @Benchmark
  def timeJavaSortLong() = {
    java.util.Arrays.sort(js.clone)
  }

  @Benchmark
  def timeJavaSortFloat() = {
    java.util.Arrays.sort(fs.clone)
  }

  @Benchmark
  def timeJavaSortDouble() = {
    java.util.Arrays.sort(ds.clone)
  }

  @Benchmark
  def timeJavaSortComplex() = {
    java.util.Arrays.sort(cs2.clone.asInstanceOf[Array[Object]])
  }

  @Benchmark
  def timeScalaQuicksortInt() = {
    is.clone; scala.util.Sorting.quickSort(is.clone)
  }

  @Benchmark
  def timeScalaQuicksortLong() = {
    is.clone; scala.util.Sorting.quickSort(js.clone)
  }
  @Benchmark
  def timeScalaQuicksortFloat() = {
    is.clone; scala.util.Sorting.quickSort(fs.clone)
  }
  @Benchmark
  def timeScalaQuicksortDouble() = {
    is.clone; scala.util.Sorting.quickSort(ds.clone)
  }

  @Benchmark
  def timeScalaQuicksortComplex() = {
    implicit val ordering = lexicographic.toOrdering
    scala.util.Sorting.quickSort(cs.clone)
  }

  @Benchmark
  def timeSpireInsertionsortInt() = {
    val n = if (pow > 13) 2 else spire.math.pow(2, pow).toInt
    spire.math.InsertionSort.sort(is.clone, 0, n)
  }

  @Benchmark
  def timeSpireInsertionsortLong() = {
    val n = if (pow > 13) 2 else spire.math.pow(2, pow).toInt

    spire.math.InsertionSort.sort(js.clone, 0, n)
  }
  @Benchmark
  def timeSpireInsertionsortFloat() = {
    val n = if (pow > 13) 2 else spire.math.pow(2, pow).toInt

    spire.math.InsertionSort.sort(fs.clone, 0, n)
  }
  @Benchmark
  def timeSpireInsertionsortDouble() = {
    val n = if (pow > 13) 2 else spire.math.pow(2, pow).toInt

    spire.math.InsertionSort.sort(ds.clone, 0, n)
  }

  @Benchmark
  def timeSpireInsertionsortComplex() = {
    val n = if (pow > 13) 2 else spire.math.pow(2, pow).toInt

    spire.math.InsertionSort.sort(cs.clone, 0, n)
  }

  @Benchmark
  def timeSpireMergesortInt() = {
    spire.math.Sorting.mergeSort(is.clone)
  }

  @Benchmark
  def timeSpireMergesortLong() = {
    spire.math.Sorting.mergeSort(js.clone)
  }
  @Benchmark
  def timeSpireMergesortFloat() = {
    spire.math.Sorting.mergeSort(fs.clone)
  }
  @Benchmark
  def timeSpireMergesortDouble() = {
    spire.math.Sorting.mergeSort(ds.clone)
  }

  @Benchmark
  def timeSpireMergesortComplex() = {
    spire.math.Sorting.mergeSort(cs.clone)
  }

  @Benchmark
  def timeSpireQuicksortInt() = {
    spire.math.Sorting.quickSort(is.clone)
  }

  @Benchmark
  def timeSpireQuicksortLong() = {
    spire.math.Sorting.quickSort(js.clone)
  }
  @Benchmark
  def timeSpireQuicksortFloat() = {
    spire.math.Sorting.quickSort(fs.clone)
  }
  @Benchmark
  def timeSpireQuicksortDouble() = {
    spire.math.Sorting.quickSort(ds.clone)
  }

  @Benchmark
  def timeSpireQuicksortComplex() = {
    spire.math.Sorting.quickSort(cs.clone)
  }
}
