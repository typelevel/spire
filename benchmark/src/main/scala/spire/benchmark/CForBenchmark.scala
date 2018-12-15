package spire
package benchmark
/*
import scala.util.Random
import Random._

import com.google.caliper.Param

import spire.syntax.cfor._

object CForBenchmarks extends MyRunner(classOf[CForBenchmarks])

class CForBenchmarks extends MyBenchmark {
  //@Param(Array("10", "15", "20", "25"))
  //@Param(Array("1000", "10000", "100000", "1000000"))
  @Param(Array("1000000"))
  var size: Int = 0

  var arr: Array[Long] = null

  override protected def setUp(): Unit = {
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

  def timeWhileIntArrayMultiply(reps:Int) = run(reps)(doWhileIntArrayMultiply)
  def timeTailrecIntArrayMultiply(reps:Int) = run(reps)(doTailrecIntArrayMultiply)
  def timeForeachIntArrayMultiply(reps:Int) = run(reps)(doForeachGcd)
  def timeForIntArrayMultiply(reps:Int) = run(reps)(doForGcd)
  def timeCForIntArrayMultiply(reps:Int) = run(reps)(doCForIntArrayMultiply)

  def doWhileOr(): Unit = {
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
  }

  def doWhileMin(): Unit = {
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
  }

  def doWhileGcd(): Unit = {
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
  }

  def doWhileIntArrayMultiply(): Unit = {
    val arr2 = arr.clone
    val len = size - 1
    var i = 0
    while (i < len) {
      val value = arr2(i)
      arr2(i) = value * 2
      i = i + 1
    }
  }

  def doTailrecOr(): Unit = {
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
      if (i < len2) { t = t ^ or(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)
  }

  def doTailrecMin(): Unit = {
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

    val len3 = size / 3
    @tailrec def loop3(i: Int): Unit = {
      if (i < len2) { t = t ^ min(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)
  }

  def doTailrecGcd(): Unit = {
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

    val len3 = size / 3
    @tailrec def loop3(i: Int): Unit = {
      if (i < len2) { t = t ^ gcd(arr(i + 1), arr(i + 2)); loop3(i + 1) }
    }
    loop3(0)
  }

  def doTailrecIntArrayMultiply(): Unit = {
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
  }


  def doForeachOr(): Unit = {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t = t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t = t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t = t ^ or(arr(i + 1), arr(i + 2)) }
  }

  def doForeachMin(): Unit = {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t = t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t = t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t = t ^ min(arr(i + 1), arr(i + 2)) }
  }

  def doForeachGcd(): Unit = {
    var t: Long = 0L
    val len = size - 1
    (0 until len).foreach { i => t = t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    (0 until len2).foreach { i => t = t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    (0 until len3).foreach { i => t = t ^ gcd(arr(i + 1), arr(i + 2)) }
  }

  def doForeachIntArrayMultiply(): Unit = {
    val arr2 = arr.clone
    val len = size
    (0 until len).foreach { i =>
      val value = arr2(i)
      arr2(i) = value * 2
    }
  }

  def doForOr(): Unit = {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t = t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t = t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t = t ^ or(arr(i + 1), arr(i + 2)) }
  }

  def doForMin(): Unit = {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t = t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t = t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t = t ^ min(arr(i + 1), arr(i + 2)) }
  }

  def doForGcd(): Unit = {
    var t: Long = 0L
    val len = size - 1
    for (i <- 0 until len) { t = t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    for (i <- 0 until len2) { t = t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    for (i <- 0 until len3) { t = t ^ gcd(arr(i + 1), arr(i + 2)) }
  }

  def doForIntArrayMultiply(): Unit = {
    val arr2 = arr.clone
    val len = size
    for (i <- 0 until len) {
      val value = arr2(i)
      arr2(i) = value * 2
    }
  }

  def doCForOr(): Unit = {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t = t ^ or(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t = t ^ or(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t = t ^ or(arr(i + 1), arr(i + 2)) }
  }

  def doCForMin(): Unit = {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t = t ^ min(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t = t ^ min(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t = t ^ min(arr(i + 1), arr(i + 2)) }
  }

  def doCForGcd(): Unit = {
    var t: Long = 0L
    val len = size - 1
    cfor(0)(_ < len, _ + 1) { i => t = t ^ gcd(arr(i), arr(i + 1)) }

    val len2 = size / 2
    cfor(0)(_ < len2, _ + 1) { i => t = t ^ gcd(arr(i + 3), arr(i + 2)) }

    val len3 = size / 3
    cfor(0)(_ < len3, _ + 1) { i => t = t ^ gcd(arr(i + 1), arr(i + 2)) }
  }

  def doCForIntArrayMultiply(): Unit = {
    val arr2 = arr.clone
    val len = size
    cfor(0)(_ < len, _ + 1) {
      i => {
        val value = arr2(i)
        arr2(i) = value * 2
      }
    }
  }
}
*/