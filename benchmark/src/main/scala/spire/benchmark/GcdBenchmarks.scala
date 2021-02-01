package spire
package benchmark

import scala.util.Random
import Random._

import spire.implicits._

import java.lang.Math
import java.math.BigInteger
import java.lang.Long.numberOfTrailingZeros

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import Arrays.init

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
class GcdBenchmarks {

  var longs: Array[Long] = null
  var bigs: Array[BigInteger] = null

  @Setup
  def setup(): Unit = {
    longs = init(200000)(nextLong())
    bigs = init(200000)(new BigInteger(nextLong().toString))
  }

  @Benchmark
  def timeXorEuclidGcdLong: Long = xorEuclidGcdLong(longs)

  @Benchmark
  def timeXorBinaryGcdLong: Long = xorBinaryGcdLong(longs)

  @Benchmark
  def timeXorBuiltinGcdBigInteger: BigInteger = xorBuiltinGcdBigInteger(bigs)

  def xorEuclidGcdLong(data: Array[Long]): Long = {
    var t = 0L
    var i = 0
    val len = data.length - 1
    while (i < len) {
      t ^= euclidGcdLong(data(i), data(i + 1))
      i += 1
    }
    t
  }

  def xorBinaryGcdLong(data: Array[Long]): Long = {
    var t = 0L
    var i = 0
    val len = data.length - 1
    while (i < len) {
      t ^= binaryGcdLong(data(i), data(i + 1))
      i += 1
    }
    t
  }

  def xorBuiltinGcdBigInteger(data: Array[BigInteger]): BigInteger = {
    var t = BigInteger.ZERO
    var i = 0
    val len = data.length - 1
    while (i < len) {
      t = t.xor(data(i).gcd(data(i + 1)))
      i += 1
    }
    t
  }

  @tailrec
  final def euclidGcdLong(x: Long, y: Long): Long = {
    if (y == 0L) Math.abs(x) else euclidGcdLong(y, x % y)
  }

  def binaryGcdLong(_x: Long, _y: Long): Long = {
    if (_x == 0L) return _y
    if (_y == 0L) return _x

    var x = Math.abs(_x)
    val xz = numberOfTrailingZeros(x)
    x >>= xz

    var y = Math.abs(_y)
    val yz = numberOfTrailingZeros(y)
    y >>= yz

    while (x != y) {
      if (x > y) {
        x -= y
        x >>= numberOfTrailingZeros(x)
      } else {
        y -= x
        y >>= numberOfTrailingZeros(y)
      }
    }

    if (xz < yz) x << xz else x << yz
  }

}
