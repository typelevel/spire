package spire.benchmark

import scala.reflect.ClassTag

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

/**
 * Extend this to create an actual benchmarking class.
 */
trait MyBenchmark extends SimpleBenchmark {

  /**
   * Sugar for building arrays using a per-cell init function.
   */
  def init[A:ClassTag](size:Int)(init: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  /**
   * Sugar to run 'f' for 'reps' number of times.
   */
  def run(reps:Int)(f: => Unit) = for(i <- 0 until reps)(f)
}

/**
 * Extend this to create a main object which will run 'cls' (a benchmark).
 */
trait MyRunner {
  val cls:java.lang.Class[_ <: com.google.caliper.Benchmark]
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}

trait BenchmarkData extends MyBenchmark {
  //val size = 10 * 1000
  //val size = 100 * 1000
  val size = 200 * 1000
  //val size = 1 * 1000 * 1000
  //val size = 4 * 1000 * 1000
  //val size = 20 * 1000 * 1000

  lazy val ints = init(size)(nextInt)
  lazy val longs = init(size)(nextLong)
  lazy val floats = init(size)(nextFloat)
  lazy val doubles = init(size)(nextDouble)
  lazy val maybeDoubles = init(size)(MaybeDouble(nextDouble))
  lazy val maybeFloats = init(size)(FastMaybeFloat(nextFloat))

  lazy val complexes = init(size)(Complex(nextDouble(), nextDouble()))
  lazy val fcomplexes = init(size)(FastComplex(nextFloat(), nextFloat()))
}

object AddBenchmarks extends MyRunner { val cls = classOf[AddBenchmarks] }
class AddBenchmarks extends MyBenchmark with BenchmarkData {
  def addGeneric[@spec(Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = Ring[A].zero
    var i = 0
    val len = data.length
    while (i < len) { total = Ring[A].plus(total, data(i)); i += 1 }
    total
  }

  def addIntsDirect(data:Array[Int]):Int = {
    var total = 0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addLongsDirect(data:Array[Long]):Long = {
    var total = 0L
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFloatsDirect(data:Array[Float]):Float = {
    var total = 0.0F
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addDoublesDirect(data:Array[Double]):Double = {
    var total = 0.0
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addMaybeDoublesDirect(data: Array[MaybeDouble]): MaybeDouble = {
    var total = MaybeDouble(0.0)
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFastMaybeFloatsDirect(data: Array[Long]): Long = {
    var total = FastMaybeFloat(0f)
    var i = 0
    val len = data.length - 1

    // This is slightly different from the others, because it'll overflow the
    // FastMaybeFloat and apparently adding NaNs and Infinities is quite a bit
    // slower than regular fp ops.
    
    while (i < len) { total += FastMaybeFloat.plus(data(i), data(i + 1)); i += 1 }
    total
  }

  def addComplexesDirect(data:Array[Complex[Double]]):Complex[Double] = {
    var total = Complex.zero[Double]
    var i = 0
    val len = data.length
    while (i < len) { total += data(i); i += 1 }
    total
  }

  def addFastComplexes(data:Array[Long]):Long = {
    var total = FastComplex(0.0F, 0.0F)
    var i = 0
    val len = data.length
    while (i < len) { total = FastComplex.add(total, data(i)); i += 1 }
    total
  }

  def timeAddIntsDirect(reps:Int) = run(reps)(addIntsDirect(ints))
  def timeAddIntsGeneric(reps:Int) = run(reps)(addGeneric(ints))
  
  def timeAddLongsDirect(reps:Int) = run(reps)(addLongsDirect(longs))
  def timeAddLongsGeneric(reps:Int) = run(reps)(addGeneric(longs))
  
  def timeAddFloatsDirect(reps:Int) = run(reps)(addFloatsDirect(floats))
  def timeAddFloatsGeneric(reps:Int) = run(reps)(addGeneric(floats))
  def timeAddFastMaybeFloatsDirect(reps:Int) = run(reps)(addFastMaybeFloatsDirect(maybeFloats))
  
  def timeAddDoublesDirect(reps:Int) = run(reps)(addDoublesDirect(doubles))
  def timeAddDoublesGeneric(reps:Int) = run(reps)(addGeneric(doubles))
  def timeAddMaybeDoublesDirect(reps:Int) = run(reps)(addMaybeDoublesDirect(maybeDoubles))

  def timeAddComplexesDirect(reps:Int) = run(reps)(addComplexesDirect(complexes))
  def timeAddComplexesGeneric(reps:Int) = run(reps)(addGeneric(complexes))
  def timeAddFastComplexes(reps:Int) = run(reps)(addFastComplexes(fcomplexes))
}

//object SortBenchmarks extends MyRunner { val cls = classOf[SortBenchmarks] }
//class SortBenchmarks extends MyBenchmark with BenchmarkData {
//  def quickSortInts(data:Array[Int]) = scala.util.Sorting.quickSort(data)
//  def quickSortLongs(data:Array[Long]) = scala.util.Sorting.quickSort(data)
//  def quickSortFloats(data:Array[Float]) = scala.util.Sorting.quickSort(data)
//  def quickSortDoubles(data:Array[Double]) = scala.util.Sorting.quickSort(data)
//  
//  def mergeSortGeneric[@spec T:Order:ClassTag](data:Array[T]) = Sorting.mergeSort(data)
//  def quickSortGeneric[@spec T:Order:ClassTag](data:Array[T]) = Sorting.quickSort(data)
//
//  def timeQuicksortInts(reps:Int) = run(reps)(quickSortInts(ints.clone))
//  def timeGenMergeSortInts(reps:Int) = run(reps)(mergeSortGeneric(ints.clone))
//  def timeGenQuickSortInts(reps:Int) = run(reps)(quickSortGeneric(ints.clone))
//  
//  def timeQuicksortLongs(reps:Int) = run(reps)(quickSortLongs(longs.clone))
//  def timeGenMergeSortLongs(reps:Int) = run(reps)(mergeSortGeneric(longs.clone))
//  def timeGenQuickSortLongs(reps:Int) = run(reps)(quickSortGeneric(longs.clone))
//  
//  def timeQuicksortFloats(reps:Int) = run(reps)(quickSortFloats(floats.clone))
//  def timeGenMergeSortFloats(reps:Int) = run(reps)(mergeSortGeneric(floats.clone))
//  def timeGenQuickSortFloats(reps:Int) = run(reps)(quickSortGeneric(floats.clone))
//  
//  def timeQuicksortDoubles(reps:Int) = run(reps)(quickSortDoubles(doubles.clone))
//  def timeGenMergeSortDoubles(reps:Int) = run(reps)(mergeSortGeneric(doubles.clone))
//  def timeGenQuickSortDoubles(reps:Int) = run(reps)(quickSortGeneric(doubles.clone))
//}

object GcdBenchmarks extends MyRunner { val cls = classOf[GcdBenchmarks] }
class GcdBenchmarks extends MyBenchmark with BenchmarkData {
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

  def timeSumEuclidGcds(reps:Int) = run(reps)(sumEuclidGcds(longs))
  def timeSumBinaryGcds(reps:Int) = run(reps)(sumBinaryGcds(longs))

  // TODO: return to tailrec method when SI-5788 is resolved
  import scala.annotation.tailrec
  @tailrec final def euclidGcd(a: Long, b: Long): Long = if (b == 0L) {
    Math.abs(a)
  } else {
    euclidGcd(b, a % b)
  }

  // iterative version of binary gcd stolen from wikipedia
  def binaryGcd(_a: Long, _b: Long): Long = {
    var a = Math.abs(_a)
    var b = Math.abs(_b)
    var shifts = 0

    while (((a | b) & 1) == 0) {
      a >>= 1
      b >>= 1
      shifts += 1
    }

    while ((a & 1) == 0) a >>= 1

    while (true) {
      while ((b & 1) == 0) b >>= 1
      if (a > b) {
        val t = b
        b = a
        a = t
      }
      b = b - a
      if (b == 0) return a << shifts
    }

    a // unused, required by scalac
  }

}

object RationalBenchmarks extends MyRunner { val cls = classOf[RationalBenchmarks] }
class RationalBenchmarks extends MyBenchmark with BenchmarkData {
  @Param(Array("8", "16", "24", "32", "40", "48", "56", "64",
               "80", "96", "112", "128",
               "160", "192", "224", "256")) 
  var bits: Int = 0

  private var rats: Array[Rational] = _
  private var bigRats: Array[BigIntRational] = _
  private var longRats: Array[LongRational] = _

  override protected def setUp() {
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


  /*
  val longs2 = ints.map(n => n.toLong)
  val bigInts2 = ints.map(n => BigInt(n))

  def combineLongRationals1(data:Array[Long])(f:(Rational, Rational) => Rational):Long = {
    val one:Rational = LongRationals.build(1L, 1L)
    var count = 0
    var i = 0
    val len = data.length - 3
    while (i < len) {
      val r1:Rational = LongRationals.build(data(i), data(i + 1))
      val r2:Rational = LongRationals.build(data(i + 2), data(i + 3))
      val r3:Rational = f(r1, r2)
      if (r3 > one) count += 1
      i += 1
    }
    count
  }

  def combineBigRationals1(data:Array[BigInt])(f:(Rational, Rational) => Rational):Long = {
    val one:Rational = BigRationals.build(1L, 1L)
    var count = 0
    var i = 0
    val len = data.length - 3
    while (i < len) {
      val r1:Rational = BigRationals.build(data(i), data(i + 1))
      val r2:Rational = BigRationals.build(data(i + 2), data(i + 3))
      val r3:Rational = f(r1, r2)
      if (r3 > one) count += 1
      i += 1
    }
    count
  }

  def timeLongRatAddBases(reps:Int) = run(reps)(combineLongRationals1(longs2)(_ + _))
  def timeBigRatAddBases(reps:Int) = run(reps)(combineBigRationals1(bigInts2)(_ + _))
  def timeLongRatSubtractBases(reps:Int) = run(reps)(combineLongRationals1(longs2)(_ - _))
  def timeBigRatSubtractBases(reps:Int) = run(reps)(combineBigRationals1(bigInts2)(_ - _))
  def timeLongRatMultiplyBases(reps:Int) = run(reps)(combineLongRationals1(longs2)(_ * _))
  def timeBigRatMultiplyBases(reps:Int) = run(reps)(combineBigRationals1(bigInts2)(_ * _))
  def timeLongRatDivideBases(reps:Int) = run(reps)(combineLongRationals1(longs2)(_ / _))
  def timeBigRatDivideBases(reps:Int) = run(reps)(combineBigRationals1(bigInts2)(_ / _))

  def combineLongRationals2(data:Array[Long])(f:(Rational, Rational) => Rational):Long = {
    val one:Rational = LongRationals.build(1L, 1L)
    val denom = data(0)
    var count = 0
    var i = 0
    val len = data.length - 1
    while (i < len) {
      val r1:Rational = LongRationals.build(data(i), denom)
      val r2:Rational = LongRationals.build(data(i + 1), denom)
      val r3:Rational = f(r1, r2)
      if (r3 > one) count += 1
      i += 1
    }
    count
  }

  def combineBigRationals2(data:Array[BigInt])(f:(Rational, Rational) => Rational):Long = {
    val one:Rational = BigRationals.build(1L, 1L)
    val denom = data(0)
    var count = 0
    var i = 0
    val len = data.length - 1
    while (i < len) {
      val r1:Rational = BigRationals.build(data(i), denom)
      val r2:Rational = BigRationals.build(data(i + 1), denom)
      val r3:Rational = f(r1, r2)
      if (r3 > one) count += 1
      i += 1
    }
    count
  }

  def timeLongRatAddSameBase(reps:Int) = run(reps)(combineLongRationals2(longs2)(_ + _))
  def timeBigRatAddSameBase(reps:Int) = run(reps)(combineBigRationals2(bigInts2)(_ + _))
  def timeLongRatSubtractSameBase(reps:Int) = run(reps)(combineLongRationals2(longs2)(_ - _))
  def timeBigRatSubtractSameBase(reps:Int) = run(reps)(combineBigRationals2(bigInts2)(_ - _))
  def timeLongRatMultiplySameBase(reps:Int) = run(reps)(combineLongRationals2(longs2)(_ * _))
  def timeBigRatMultiplySameBase(reps:Int) = run(reps)(combineBigRationals2(bigInts2)(_ * _))
  def timeLongRatDivideSameBase(reps:Int) = run(reps)(combineLongRationals2(longs2)(_ / _))
  def timeBigRatDivideSameBase(reps:Int) = run(reps)(combineBigRationals2(bigInts2)(_ / _))
  */
}

