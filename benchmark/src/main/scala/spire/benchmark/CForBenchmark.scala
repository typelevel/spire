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

import scala.util.Random
import Random._

import spire.syntax.cfor._

import Arrays.init

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class CForBenchmarks {
  // @Param(Array("10", "15", "20", "25"))
  // @Param(Array("1000", "10000", "100000", "1000000"))
  @Param(Array("1000000"))
  var size: Int = 0

  var arr: Array[Long] = null

  @Setup
  def setup(): Unit = {
    arr = init(size)(scala.math.abs(nextInt() - 1).toInt + 1)
  }

  @tailrec final def gcd(a: Long, b: Long): Long = if (a % b == 0) b else gcd(b, a % b)
  final def min(a: Long, b: Long): Long = if (a < b) a else b
  final def or(a: Long, b: Long): Long = a | b

  // TODO: what about the lengths and array indices below?

  @Benchmark
  def doWhileOr: Long = {
    var t: Long = 0L
    var i: Int = 0
    val len = size - 1
    while (i < len) { t = t ^ or(arr(i), arr(i + 1)); i += 1 }

    i = 0
    val len2 = size / 2
    while (i < len2) { t = t ^ or(arr(i + 3), arr(i + 2)); i += 1 }

    i = 0
    val len3 = size / 3
    while (i < len3) { t = t ^ or(arr(i + 1), arr(i + 2)); i += 1 }
    t
  }

  @Benchmark
  def doWhileMin: Long = {
    var t: Long = 0L
    var i: Int = 0
    val len = size - 1
    while (i < len) { t = t ^ min(arr(i), arr(i + 1)); i += 1 }

    i = 0
    val len2 = size / 2
    while (i < len2) { t = t ^ min(arr(i + 3), arr(i + 2)); i += 1 }

    i = 0
    val len3 = size / 3
    while (i < len3) { t = t ^ min(arr(i + 1), arr(i + 2)); i += 1 }

    t
  }

  @Benchmark
  def doWhileGcd: Long = {
    var t: Long = 0L
    var i: Int = 0
    val len = size - 1
    while (i < len) { t = t ^ gcd(arr(i), arr(i + 1)); i += 1 }

    i = 0
    val len2 = size / 2
    while (i < len2) { t = t ^ gcd(arr(i + 3), arr(i + 2)); i += 1 }

    i = 0
    val len3 = size / 3
    while (i < len3) { t = t ^ gcd(arr(i + 1), arr(i + 2)); i += 1 }

    t
  }

  @Benchmark
  def doWhileIntArrayMultiply: Array[Long] = {
    val arr2 = arr.clone
    val len = size - 1
    var i = 0
    while (i < len) {
      val value = arr2(i)
      arr2(i) = value * 2
      i = i + 1
    }
    arr2
  }

  @Benchmark
  def doTailrecOr: Long = {
    var t: Long = 0L
    val len = size - 1
    @tailrec def loop1(i: Int): Unit = {
      if (i < len) { t = t ^ or(arr(i), arr(i + 1)); loop1(i + 1) }
    }
    loop1(0)

    val len2 = size / 2
    @tailrec def loop2(i: Int): Unit = {
      if (i < len2) { t = t ^ or(arr(i + 3), arr(i + 2)); loop2(i + 1) }
    }
    loop2(0)

    val len3 = size / 3
    @tailrec def loop3(i: Int): Unit = {
      if (i < len3) { t = t ^ or(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)

    t
  }

  @Benchmark
  def doTailrecMin: Long = {
    var t: Long = 0L
    val len = size - 1
    @tailrec def loop1(i: Int): Unit = {
      if (i < len) { t = t ^ min(arr(i), arr(i + 1)); loop1(i + 1) }
    }
    loop1(0)

    val len2 = size / 2
    @tailrec def loop2(i: Int): Unit = {
      if (i < len2) { t = t ^ min(arr(i + 3), arr(i + 2)); loop2(i + 1) }
    }
    loop2(0)

    @tailrec def loop3(i: Int): Unit = {
      if (i < len2) { t = t ^ min(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)

    t
  }

  @Benchmark
  def doTailrecGcd: Long = {
    var t: Long = 0L
    val len = size - 1
    @tailrec def loop1(i: Int): Unit = {
      if (i < len) { t = t ^ gcd(arr(i), arr(i + 1)); loop1(i + 1) }
    }
    loop1(0)

    val len2 = size / 2
    @tailrec def loop2(i: Int): Unit = {
      if (i < len2) { t = t ^ gcd(arr(i + 3), arr(i + 2)); loop2(i + 1) }
    }
    loop2(0)

    @tailrec def loop3(i: Int): Unit = {
      if (i < len2) { t = t ^ gcd(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)

    t
  }

  @Benchmark
  def doTailrecIntArrayMultiply: Array[Long] = {
    val arr2 = arr.clone
    val len = size
    @tailrec def loop(i: Int): Unit = {
      if (i < len) {
        val value = arr2(i)
        arr2(i) = value * 2
        loop(i + 1)
      }
    }
    loop(0)

    arr2
  }

  @Benchmark
  def doForeachOr: Long = {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t = t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t = t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t = t ^ or(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doForeachMin: Long = {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t = t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t = t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t = t ^ min(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doForeachGcd: Long = {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t = t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t = t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t = t ^ gcd(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doForeachIntArrayMultiply: Array[Long] = {
    val arr2 = arr.clone
    val len = size
    (0 until len).foreach { i =>
      val value = arr2(i)
      arr2(i) = value * 2
    }
    arr2
  }

  @Benchmark
  def doForOr: Long = {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t = t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t = t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t = t ^ or(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doForMin: Long = {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t = t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t = t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t = t ^ min(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doForGcd: Long = {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t = t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t = t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t = t ^ gcd(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doForIntArrayMultiply: Array[Long] = {
    val arr2 = arr.clone
    val len = size
    for (i <- 0 until len) {
      val value = arr2(i)
      arr2(i) = value * 2
    }
    arr2
  }

  @Benchmark
  def doCForOr: Long = {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t = t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t = t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t = t ^ or(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doCForMin: Long = {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t = t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t = t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t = t ^ min(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doCForGcd: Long = {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t = t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t = t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t = t ^ gcd(arr(i + 1), arr(i + 2)) }

    t
  }

  @Benchmark
  def doCForIntArrayMultiply: Array[Long] = {
    val arr2 = arr.clone
    val len = size
    cfor(0)(_ < len, _ + 1) { i =>
      {
        val value = arr2(i)
        arr2(i) = value * 2
      }
    }
    arr2
  }

}
