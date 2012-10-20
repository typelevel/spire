package spire.benchmark

import scala.{specialized => spec}
import scala.annotation.tailrec

import scala.util.Random
import Random._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import spire.syntax._
import spire.math.fun._

object CForBenchmarks extends MyRunner(classOf[CForBenchmarks])

class CForBenchmarks extends MyBenchmark {
  //@Param(Array("10", "15", "20", "25"))
  //@Param(Array("1000", "10000", "100000", "1000000"))
  @Param(Array("1000000"))
  var size: Int = 0

  var arr: Array[Long] = null

  override protected def setUp() {
    arr = init(size)(scala.math.abs(nextInt - 1).toInt + 1)
  }

  @tailrec final def gcd(a: Long, b: Long): Long = if (a % b == 0) b else gcd(b, a % b)
  final def min(a: Long, b: Long): Long = if (a < b) a else b
  final def or(a: Long, b: Long): Long = a | b

  def timeWhileOr(reps:Int) = run(reps)(doWhileOr)
  def timeTailrecOr(reps:Int) = run(reps)(doTailrecOr)
  def timeForeachOr(reps:Int) = run(reps)(doForeachOr)
  def timeForOr(reps:Int) = run(reps)(doForOr)
  def timeCForOr(reps:Int) = run(reps)(doCForOr)

  def timeWhileMin(reps:Int) = run(reps)(doWhileMin)
  def timeTailrecMin(reps:Int) = run(reps)(doTailrecMin)
  def timeForeachMin(reps:Int) = run(reps)(doForeachMin)
  def timeForMin(reps:Int) = run(reps)(doForMin)
  def timeCForMin(reps:Int) = run(reps)(doCForMin)

  def timeWhileGcd(reps:Int) = run(reps)(doWhileGcd)
  def timeTailrecGcd(reps:Int) = run(reps)(doTailrecGcd)
  def timeForeachGcd(reps:Int) = run(reps)(doForeachGcd)
  def timeForGcd(reps:Int) = run(reps)(doForGcd)
  def timeCForGcd(reps:Int) = run(reps)(doCForGcd)

  def doWhileOr {
    var t: Long = 0L
    var i: Int = 0
    val len = size - 1
    while (i < len) { t ^ or(arr(i), arr(i + 1)); i += 1 }

    i = 0
    val len2 = size / 2
    while (i < len2) { t ^ or(arr(i + 3), arr(i + 2)); i += 1 }

    i = 0
    val len3 = size / 3
    while (i < len3) { t ^ or(arr(i + 1), arr(i + 2)); i += 1 }
  }

  def doWhileMin {
    var t: Long = 0L
    var i: Int = 0
    val len = size - 1
    while (i < len) { t ^ min(arr(i), arr(i + 1)); i += 1 }

    i = 0
    val len2 = size / 2
    while (i < len2) { t ^ min(arr(i + 3), arr(i + 2)); i += 1 }

    i = 0
    val len3 = size / 3
    while (i < len3) { t ^ min(arr(i + 1), arr(i + 2)); i += 1 }
  }

  def doWhileGcd {
    var t: Long = 0L
    var i: Int = 0
    val len = size - 1
    while (i < len) { t ^ gcd(arr(i), arr(i + 1)); i += 1 }

    i = 0
    val len2 = size / 2
    while (i < len2) { t ^ gcd(arr(i + 3), arr(i + 2)); i += 1 }

    i = 0
    val len3 = size / 3
    while (i < len3) { t ^ gcd(arr(i + 1), arr(i + 2)); i += 1 }
  }

  def doTailrecOr {
    var t: Long = 0L
    val len = size - 1
    @tailrec def loop1(i: Int) {
      if (i < len) { t ^ or(arr(i), arr(i + 1)); loop1(i + 1) }
    }
    loop1(0)

    val len2 = size / 2
    @tailrec def loop2(i: Int) {
      if (i < len2) { t ^ or(arr(i + 3), arr(i + 2)); loop2(i + 1) }
    }
    loop2(0)

    val len3 = size / 3
    @tailrec def loop3(i: Int) {
      if (i < len2) { t ^ or(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)
  }

  def doTailrecMin {
    var t: Long = 0L
    val len = size - 1
    @tailrec def loop1(i: Int) {
      if (i < len) { t ^ min(arr(i), arr(i + 1)); loop1(i + 1) }
    }
    loop1(0)

    val len2 = size / 2
    @tailrec def loop2(i: Int) {
      if (i < len2) { t ^ min(arr(i + 3), arr(i + 2)); loop2(i + 1) }
    }
    loop2(0)

    val len3 = size / 3
    @tailrec def loop3(i: Int) {
      if (i < len2) { t ^ min(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)
  }

  def doTailrecGcd {
    var t: Long = 0L
    val len = size - 1
    @tailrec def loop1(i: Int) {
      if (i < len) { t ^ gcd(arr(i), arr(i + 1)); loop1(i + 1) }
    }
    loop1(0)

    val len2 = size / 2
    @tailrec def loop2(i: Int) {
      if (i < len2) { t ^ gcd(arr(i + 3), arr(i + 2)); loop2(i + 1) }
    }
    loop2(0)

    val len3 = size / 3
    @tailrec def loop3(i: Int) {
      if (i < len2) { t ^ gcd(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)
  }

  def doForeachOr {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t ^ or(arr(i + 1), arr(i + 2)) }
  }

  def doForeachMin {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t ^ min(arr(i + 1), arr(i + 2)) }
  }

  def doForeachGcd {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t ^ gcd(arr(i + 1), arr(i + 2)) }
  }

  def doForOr {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t ^ or(arr(i + 1), arr(i + 2)) }
  }

  def doForMin {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t ^ min(arr(i + 1), arr(i + 2)) }
  }

  def doForGcd {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t ^ gcd(arr(i + 1), arr(i + 2)) }
  }

  def doCForOr {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t ^ or(arr(i + 1), arr(i + 2)) }
  }

  def doCForMin {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t ^ min(arr(i + 1), arr(i + 2)) }
  }

  def doCForGcd {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t ^ gcd(arr(i + 1), arr(i + 2)) }
  }
}
