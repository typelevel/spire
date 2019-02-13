package spire
package benchmark

/*
import scala.util.Random

import spire.math._
import spire.implicits._

import com.google.caliper.Param

object RationalBenchmarks extends MyRunner(classOf[RationalBenchmarks])

class RationalBenchmarks extends MyBenchmark with BenchmarkData {
  @Param(Array("8", "16", "24", "32", "40", "48", "56", "64",
               "80", "96", "112", "128",
               "160", "192", "224", "256"))
  var bits: Int = 0

  private var rats: Array[Rational] = _
  private var bigRats: Array[BigIntRational] = _
  private var longRats: Array[LongRational] = _

  override protected def setUp(): Unit = {
    rats = init(size)(Rational(BigInt(bits, Random), BigInt(bits, Random) + 1))
    bigRats = init(size)(BigIntRational(BigInt(bits, Random), BigInt(bits, Random) + 1))
    if (bits <= 32) {
      longRats = init(size)(LongRational(BigInt(bits, Random).toLong, BigInt(bits, Random).toLong + 1L))
    } else {
      longRats = Array[LongRational]()
    }
  }

  def bigSum(rats: Array[BigIntRational]): Int = {
    var sign = 1
    var i = 0
    var len = rats.length - 1

    while (i < len) {
      sign *= (rats(i) + rats(i + 1)).signum
      i += 1
    }

    sign
  }

  def longSum(rats: Array[LongRational]): Int = {
    var sign = 1
    var i = 0
    var len = rats.length - 1

    while (i < len) {
      sign *= (rats(i) + rats(i + 1)).signum
      i += 1
    }

    sign
  }

  def sum(rats: Array[Rational]): Int = {
    var sign = 1
    var i = 0
    var len = rats.length - 1

    while (i < len) {
      sign *= (rats(i) + rats(i + 1)).signum
      i += 1
    }

    sign
  }

  def bigProd(rats: Array[BigIntRational]): Int = {
    var sign = 1
    var i = 0
    var len = rats.length - 1

    while (i < len) {
      sign *= (rats(i) * rats(i + 1)).signum
      i += 1
    }

    sign
  }

  def longProd(rats: Array[LongRational]): Int = {
    var sign = 1
    var i = 0
    var len = rats.length - 1

    while (i < len) {
      sign *= (rats(i) * rats(i + 1)).signum
      i += 1
    }

    sign
  }

  def prod(rats: Array[Rational]): Int = {
    var sign = 1
    var i = 0
    var len = rats.length - 1

    while (i < len) {
      sign *= (rats(i) * rats(i + 1)).signum
      i += 1
    }

    sign
  }


  def timeRationalSum(reps: Int) = run(reps)(sum(rats))
  def timeRationalProd(reps: Int) = run(reps)(prod(rats))

  def timeBigIntRationalSum(reps: Int) = run(reps)(bigSum(bigRats))
  def timeBigIntRationalProd(reps: Int) = run(reps)(bigProd(bigRats))

  def timeLongRationalSum(reps: Int) = run(reps)(longSum(longRats))
  def timeLongRationalProd(reps: Int) = run(reps)(longProd(longRats))
}
*/