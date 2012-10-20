package spire.benchmark

import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.implicits._
import fpf._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import java.lang.Math
import java.math.BigInteger

object GcdBenchmarks extends MyRunner(classOf[GcdBenchmarks])

class GcdBenchmarks extends MyBenchmark {

  var longs: Array[Long] = null
  var bigs: Array[BigInteger] = null

  override def setUp() {
    longs = init(200000)(nextLong)
    bigs = init(200000)(new BigInteger(nextLong.toString))
  }

  def timeXorEuclidGcdLong(reps:Int) = run(reps)(xorEuclidGcdLong(longs))
  def timeXorBinaryGcdLong(reps:Int) = run(reps)(xorBinaryGcdLong(longs))

  def timeXorEuclidGcdBigInteger(reps:Int) = run(reps)(xorEuclidGcdBigInteger(bigs))
  def timeXorBinaryGcdBigInteger(reps:Int) = run(reps)(xorBinaryGcdBigInteger(bigs))

  def xorEuclidGcdLong(data:Array[Long]):Long = {
    var t = 0L
    var i = 0
    val len = data.length - 1
    while (i < len) {
      t ^= euclidGcdLong(data(i), data(i + 1))
      i += 1
    }
    t
  }

  def xorBinaryGcdLong(data:Array[Long]):Long = {
    var t = 0L
    var i = 0
    val len = data.length - 1
    while (i < len) {
      t ^= binaryGcdLong(data(i), data(i + 1))
      i += 1
    }
    t
  }

  def xorEuclidGcdBigInteger(data:Array[BigInteger]):BigInteger = {
    var t = BigInteger.ZERO
    var i = 0
    val len = data.length - 1
    while (i < len) {
      t = t.xor(euclidGcdBigInteger(data(i).abs, data(i + 1).abs))
      i += 1
    }
    t
  }

  def xorBinaryGcdBigInteger(data:Array[BigInteger]):BigInteger = {
    var t = BigInteger.ZERO
    var i = 0
    val len = data.length - 1
    while (i < len) {
      t = t.xor(binaryGcdBigInteger(data(i), data(i + 1)))
      i += 1
    }
    t
  }

  @tailrec
  final def euclidGcdLong(a: Long, b: Long): Long = {
    if (b == 0L) Math.abs(a) else euclidGcdLong(b, a % b)
  }

  @tailrec
  final def euclidGcdBigInteger(a: BigInteger, b: BigInteger): BigInteger = {
    if (b.signum == 0) a.abs else euclidGcdBigInteger(b, a.mod(b))
  }

  // iterative version of binary gcd stolen from wikipedia
  def binaryGcdLong(_a: Long, _b: Long): Long = {
    var a = Math.abs(_a)
    var b = Math.abs(_b)
    var shifts = 0

    while (((a | b) & 1) == 0) {
      a >>= 1
      b >>= 1
      shifts += 1
    }

    while ((a & 1) == 0) a >>= 1

    do {
      while ((b & 1) == 0) b >>= 1
      if (a > b) {
        val t = b
        b = a
        a = t
      }
      b = b - a
    } while (b != 0)
    a << shifts
  }

  def binaryGcdBigInteger(_a: BigInteger, _b: BigInteger): BigInteger = {
    var a = _a.abs
    var b = _b.abs
    var shifts = 0

    while (!a.testBit(0) && !b.testBit(0)) {
      a = a.shiftRight(1)
      b = b.shiftRight(1)
      shifts += 1
    }

    while (!a.testBit(0)) a = a.shiftRight(1)

    do {
      while (!b.testBit(0)) b = b.shiftRight(1)
      if (a.compareTo(b) > 0) {
        val t = b
        b = a
        a = t
      }
      b = b.subtract(a)
    } while (b.signum != 0)
    a.shiftLeft(shifts)
  }
}

