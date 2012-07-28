package spire.benchmark

import com.google.caliper.Param
import scala.util.Random._

import java.lang.Math
import scala.annotation.tailrec

object GcdBenchmark extends MyRunner2(classOf[GcdBenchmark])

class GcdBenchmark extends MyBenchmark2 {
  @Param(Array("18"))
  var pow:Int = 0

  var longs:Array[Long] = null

  override def setUp() {
    val n = scala.math.pow(2, pow).toInt
    longs = init(n)(nextLong)
  }

  def timeSumEuclidGcds(reps:Int) = run(reps)(sumEuclidGcds(longs))
  def timeSumBinaryGcds(reps:Int) = run(reps)(sumBinaryGcds(longs))

  def sumEuclidGcds(data:Array[Long]):Long = {
    var total = 0L
    var i = 0
    val len = data.length - 1
    while (i < len) {
      total += euclidGcd(data(i), data(i + 1))
      i += 1
    }
    total
  }

  @tailrec final def euclidGcd(a: Long, b: Long): Long = {
    if (b == 0L) Math.abs(a)
    else euclidGcd(b, a % b)
  }

  def sumBinaryGcds(data:Array[Long]):Long = {
    var total = 0L
    var i = 0
    val len = data.length - 1
    while (i < len) {
      total += binaryGcd(data(i), data(i + 1))
      i += 1
    }
    total
  }

  final def binaryGcd(a:Long, b:Long) = _binaryGcd(Math.abs(a), Math.abs(b), 0)

  @tailrec final def _binaryGcd(a:Long, b:Long, shifts:Int):Long = {
    // these are the terminal cases
    if (a == b) a << shifts
    else if (a == 0L) b << shifts
    else if (b == 0L) a << shifts

    // if a is even
    else if ((a & 1L) == 0L) {
      // factor out 2 from a
      if ((b & 1L) == 1L) _binaryGcd(a >> 1L, b, shifts)
      // remove out 2 from both, remembering to reapply it at the end.
      else _binaryGcd(a >> 1L, b >> 1L, shifts + 1)
    }

    // if b is even, factor out 2
    else if ((b & 1L) == 0L) _binaryGcd(a, b >> 1L, shifts)

    // reduce whichever argument is larger
    else if (a > b) _binaryGcd((a - b) >> 1L, b, shifts)
    else _binaryGcd((b - a) >> 1L, a, shifts)
  }
}
