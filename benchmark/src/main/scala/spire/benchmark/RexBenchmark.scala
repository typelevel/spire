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
import spire.implicits._
import spire.math._
import spire.benchmark.ArrayOrder
import Arrays.mkarray
import scala.util.Random._
import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class RexBenchmarks {
  @Param(Array("10", "12", "14", "16", "18"))
  var pow: Int = 0

  var fs: Array[Float] = null
  var ds: Array[Double] = null

  @Setup
  def setUp(): Unit = {
    val size = spire.math.pow(2, pow).toInt
    fs = mkarray(size, ArrayOrder.Random)(nextGaussian.toFloat)
    ds = mkarray(size, ArrayOrder.Random)(nextGaussian)
  }

  @Benchmark
  def timeDirect(): Unit = runDirect(fs, ds, 20)

  @Benchmark
  def timeGeneric(): Unit = runGeneric(fs, ds, 20)

  def runDirect(a: Array[Float], b: Array[Double], n: Int): Double = {
    (for (i <- 2 to n by 2) yield nearlyMaxF(a, n) + nearlyMaxD(b, n)).sum
  }

  def runGeneric(a: Array[Float], b: Array[Double], n: Int): Double = {
    (for (i <- 2 to n by 2) yield nearlyMaxG(a, n) + nearlyMaxG(b, n)).sum
  }

  def nearlyMaxF(a: Array[Float], k: Int, start: Int = 0, end: Int = -1): Float = {
    val i0 = if (start >= 0) start else a.length + start
    val i1 = if (end >= 0) end else a.length + end + 1
    val ai = new Array[Float](max(k, 0) + 1)
    var i = i0 + 1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) > ai(j)) {
        var h = j - 1
        if (j < k) { ai(j + 1) = ai(j); j += 1 }
        while (h >= 0 && a(i) > ai(h)) { ai(h + 1) = ai(h); h -= 1 }
        ai(h + 1) = a(i)
      } else if (j < k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }

  def nearlyMaxD(a: Array[Double], k: Int, start: Int = 0, end: Int = -1): Double = {
    val i0 = if (start >= 0) start else a.length + start
    val i1 = if (end >= 0) end else a.length + end + 1
    val ai = new Array[Double](max(k, 0) + 1)
    var i = i0 + 1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) > ai(j)) {
        var h = j - 1
        if (j < k) { ai(j + 1) = ai(j); j += 1 }
        while (h >= 0 && a(i) > ai(h)) { ai(h + 1) = ai(h); h -= 1 }
        ai(h + 1) = a(i)
      } else if (j < k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }

  def nearlyMaxG[@sp A: Numeric: ClassTag](a: Array[A], k: Int, start: Int = 0, end: Int = -1): A = {
    val i0 = if (start >= 0) start else a.length + start
    val i1 = if (end >= 0) end else a.length + end + 1
    val ai = new Array[A](max(k, 0) + 1)
    var i = i0 + 1
    var j = 0
    ai(0) = a(i0)
    while (i < i1) {
      if (a(i) > ai(j)) {
        var h = j - 1
        if (j < k) { ai(j + 1) = ai(j); j += 1 }
        while (h >= 0 && a(i) > ai(h)) { ai(h + 1) = ai(h); h -= 1 }
        ai(h + 1) = a(i)
      } else if (j < k) {
        j += 1
        ai(j) = a(i)
      }
      i += 1
    }
    ai(k)
  }
}
