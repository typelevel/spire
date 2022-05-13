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

import spire.algebra._
import spire.implicits._

import scala.math.{Numeric => ScalaN}
import Arrays.init

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class ScalaVsSpireBenchmarks {
  // @Param(Array("10", "15", "20", "25"))
  @Param(Array("20"))
  var pow: Int = 0

  var size: Int = 0

  var as: Array[Int] = null
  var bs: Array[Int] = null
  var cs: Array[Int] = null

  @Setup
  def setup(): Unit = {
    size = spire.math.pow(2, pow).toInt
    as = init(size)(scala.math.abs(nextInt()) % 100000 + 1)
    bs = init(size)(scala.math.abs(nextInt()) % 100000 + 1)
    cs = new Array(size)
  }

  @Benchmark
  def timePairwiseDirect: Array[Int] = {
    doPairwiseDirect(as, bs, cs)
    cs
  }

  @Benchmark
  def timePairwiseGeneric: Array[Int] = {
    doPairwiseGeneric(as, bs, cs)
    cs
  }

  @Benchmark
  def timePairwiseSpire: Array[Int] = {
    doPairwiseSpire(as, bs, cs)
    cs
  }

  @Benchmark
  def timeIncrementDirect: Int = doIncrementDirect(0, size)

  @Benchmark
  def timeIncrementGeneric: Int = doIncrementGeneric(0, size)

  @Benchmark
  def timeIncrementSpire: Int = doIncrementSpire(0, size)

  @Benchmark
  def timeMinMaxDirect: (Int, Int) = doMinMaxDirect(as)

  @Benchmark
  def timeMinMaxGeneric: (Int, Int) = doMinMaxGeneric(as)

  @Benchmark
  def timeMinMaxSpire: (Int, Int) = doMinMaxSpire(as)

  @Benchmark
  def timeGcdDirect: Array[Int] = {
    doGcdDirect(as, bs, cs)
    cs
  }

  @Benchmark
  def timeGcdGeneric: Array[Int] = {
    doGcdGeneric(as, bs, cs)
    cs
  }

  @Benchmark
  def timeGcdSpire: Array[Int] = {
    doGcdSpire(as, bs, cs)
    cs
  }

  @Benchmark
  def timeScaleDirect: Array[Int] = {
    doScaleDirect(as, 9, 4, cs)
    cs
  }

  @Benchmark
  def timeScaleGeneric: Array[Int] = {
    doScaleGeneric(as, 9, 4, cs)
    cs
  }

  @Benchmark
  def timeScaleSpire: Array[Int] = {
    doScaleSpire(as, 9, 4, cs)
    cs
  }

  /**
   * Pairwise addition between arrays
   */
  def doPairwiseDirect(as: Array[Int], bs: Array[Int], cs: Array[Int]): Unit = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) + bs(i); i += 1 }
  }

  def doPairwiseGeneric[A: ScalaN](as: Array[A], bs: Array[A], cs: Array[A]): Unit = {
    import ScalaN.Implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) + bs(i); i += 1 }
  }

  def doPairwiseSpire[@sp(Int) A: Ring](as: Array[A], bs: Array[A], cs: Array[A]): Unit = {
    import spire.implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) + bs(i); i += 1 }
  }

  /**
   * Simple incrementing counter
   */
  def doIncrementDirect(start: Int, n: Int): Int = {
    var t = start
    var i = 0
    while (i < n) { t += 1; i += 1 }
    t
  }

  def doIncrementGeneric[A: ScalaN](start: A, n: A): A = {
    import ScalaN.Implicits._
    val ev = implicitly[ScalaN[A]]
    import ev.mkOrderingOps
    var t = start
    var i = ev.zero
    while (i < n) { t += ev.one; i += ev.one }
    t
  }

  def doIncrementSpire[@sp(Int) A: Ring: Order](start: A, n: A): A = {
    import spire.implicits._
    val ev = Ring[A]
    var t = start
    var i = ev.zero
    while (i < n) { t += ev.one; i += ev.one }
    t
  }

  /**
   * Find min/max values.
   */
  def doMinMaxDirect(ns: Array[Int]): (Int, Int) = {
    var zmin = ns(0)
    var zmax = ns(0)
    var i = 1
    val len = ns.length
    while (i < len) {
      val z = ns(i)
      if (z < zmin) zmin = z
      else if (z > zmax) zmax = z
      i += 1
    }
    (zmin, zmax)
  }

  def doMinMaxGeneric[A: ScalaN](ns: Array[A]): (A, A) = {
    val ev = implicitly[ScalaN[A]]
    import ev.mkOrderingOps

    var zmin = ns(0)
    var zmax = ns(0)
    var i = 1
    val len = ns.length
    while (i < len) {
      val z = ns(i)
      if (z < zmin) zmin = z
      else if (z > zmax) zmax = z
      i += 1
    }
    (zmin, zmax)
  }

  def doMinMaxSpire[@sp(Int) A: Ring: Order](ns: Array[A]): (A, A) = {
    import spire.implicits._

    var zmin = ns(0)
    var zmax = ns(0)
    var i = 1
    val len = ns.length
    while (i < len) {
      val z = ns(i)
      if (z < zmin) zmin = z
      else if (z > zmax) zmax = z
      i += 1
    }
    (zmin, zmax)
  }

  /**
   * Find GCD.
   */
  @tailrec final def gcdDirect(a: Int, b: Int): Int =
    if (a % b == 0) b else gcdDirect(b, a % b)

  def doGcdDirect(as: Array[Int], bs: Array[Int], cs: Array[Int]): Unit = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = gcdDirect(as(i), bs(i)); i += 1 }
  }

  import scala.math.{Integral => ScalaI}
  @tailrec final def gcdGeneric[A](a: A, b: A)(implicit ev: ScalaI[A]): A = {
    import ScalaI.Implicits._
    if (a % b == ev.zero) b else gcdGeneric(b, a % b)
  }

  def doGcdGeneric[A: ScalaI](as: Array[A], bs: Array[A], cs: Array[A]): Unit = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = gcdGeneric(as(i), bs(i)); i += 1 }
  }

  @tailrec final def gcdSpire[@sp(Int) A](a: A, b: A)(implicit ev1: EuclideanRing[A], ev2: Eq[A]): A = {
    if (a.emod(b) === ev1.zero) b else gcdSpire(b, a.emod(b))
  }

  def doGcdSpire[@sp(Int) A: EuclideanRing: Eq](as: Array[A], bs: Array[A], cs: Array[A]): Unit = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = gcdSpire(as(i), bs(i)); i += 1 }
  }

  /**
   * Scale array.
   */
  def doScaleDirect(as: Array[Int], n: Int, d: Int, cs: Array[Int]): Unit = {
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) * n / d; i += 1 }
  }

  def doScaleGeneric[A: ScalaI](as: Array[A], n: A, d: A, cs: Array[A]): Unit = {
    import ScalaI.Implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = as(i) * n / d; i += 1 }
  }

  def doScaleSpire[@sp(Int) A: EuclideanRing](as: Array[A], n: A, d: A, cs: Array[A]): Unit = {
    import spire.implicits._
    var i = 0
    val len = as.length
    while (i < len) { cs(i) = (as(i) * n).equot(d); i += 1 }
  }
}

//
//object Direct {
//  @tailrec final def gcd(a: Int, b: Int): Int =
//    if (a % b == 0) b else gcd(b, a % b)
//}
//
//object Spire {
//  @tailrec final def gcd[@sp(Int) A: Integral](a: A, b: A): A =
//    if (a % b === Integral[A].zero) b else gcd(b, a % b)
//}
//
//object Scala {
//  @tailrec final def gcd[A: Integral](a: A, b: A): A =
//    if (a % b == implicitly[Integra[A]].zero) b else gcd(b, a % b)
//}
