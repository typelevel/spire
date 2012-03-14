package benchmark

import scala.{specialized => spec}
import scala.util.Random._

import numerics.math._
import numerics.math.Implicits._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark

/**
 * Extend this to create an actual benchmarking class.
 */
trait MyBenchmark extends SimpleBenchmark {

  /**
   * Sugar for building arrays using a per-cell init function.
   */
  def init[A:Manifest](size:Int)(init: => A) = {
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


/**
 * AddBenchmarks
 *
 * Benchmarks the speed of direct/generic addition.
 */
object AddBenchmarks extends MyRunner { val cls = classOf[AddBenchmarks] }

class AddBenchmarks extends MyBenchmark {
  //private val size = 10 * 1000
  //private val size = 100 * 1000
  //private val size = 1 * 1000 * 1000
  //private val size = 4 * 1000 * 1000
  private val size = 20 * 1000 * 1000

  val ints = init(size)(nextInt)
  val longs = init(size)(nextLong)
  val floats = init(size)(nextFloat)
  val doubles = init(size)(nextDouble)

  //val complexes = init(size)(Complex(nextDouble(), nextDouble()))
  //val fcomplexes = init(size)(FastComplex(nextFloat(), nextFloat()))

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

  //def timeCompareIntsDirect(reps:Int) = run(reps)(compareIntsDirect(ints))
  //def timeCompareIntsGeneric(reps:Int) = run(reps)(compareGeneric(ints))

  //def timeAddIntsDirect(reps:Int) = run(reps)(addIntsDirect(ints))
  //def timeAddIntsGeneric(reps:Int) = run(reps)(addGeneric(ints))
  //
  //def timeAddLongsDirect(reps:Int) = run(reps)(addLongsDirect(longs))
  //def timeAddLongsGeneric(reps:Int) = run(reps)(addGeneric(longs))
  //
  //def timeAddFloatsDirect(reps:Int) = run(reps)(addFloatsDirect(floats))
  //def timeAddFloatsGeneric(reps:Int) = run(reps)(addGeneric(floats))
  //
  //def timeAddDoublesDirect(reps:Int) = run(reps)(addDoublesDirect(doubles))
  //def timeAddDoublesGeneric(reps:Int) = run(reps)(addGeneric(doubles))

  //def timeAddComplexesDirect(reps:Int) = run(reps)(addComplexesDirect(complexes))
  //def timeAddComplexesGeneric(reps:Int) = run(reps)(addGeneric(complexes))
  //def timeAddFastComplexes(reps:Int) = run(reps)(addFastComplexes(fcomplexes))

  def quickSortInts(data:Array[Int]) = scala.util.Sorting.quickSort(data)
  def quickSortLongs(data:Array[Long]) = scala.util.Sorting.quickSort(data)
  def quickSortFloats(data:Array[Float]) = scala.util.Sorting.quickSort(data)
  def quickSortDoubles(data:Array[Double]) = scala.util.Sorting.quickSort(data)
  
  def mergeSortGeneric[@spec T:Order:Manifest](data:Array[T]) = Sorting.mergeSort(data)
  def quickSortGeneric[@spec T:Order:Manifest](data:Array[T]) = Sorting.quickSort(data)

  def timeQuicksortInts(reps:Int) = run(reps)(quickSortInts(ints.clone))
  def timeGenMergeSortInts(reps:Int) = run(reps)(mergeSortGeneric(ints.clone))
  def timeGenQuickSortInts(reps:Int) = run(reps)(quickSortGeneric(ints.clone))

  def timeQuicksortLongs(reps:Int) = run(reps)(quickSortLongs(longs.clone))
  def timeGenMergeSortLongs(reps:Int) = run(reps)(mergeSortGeneric(longs.clone))
  def timeGenQuickSortLongs(reps:Int) = run(reps)(quickSortGeneric(longs.clone))

  def timeQuicksortFloats(reps:Int) = run(reps)(quickSortFloats(floats.clone))
  def timeGenMergeSortFloats(reps:Int) = run(reps)(mergeSortGeneric(floats.clone))
  def timeGenQuickSortFloats(reps:Int) = run(reps)(quickSortGeneric(floats.clone))
  
  def timeQuicksortDoubles(reps:Int) = run(reps)(quickSortDoubles(doubles.clone))
  def timeGenMergeSortDoubles(reps:Int) = run(reps)(mergeSortGeneric(doubles.clone))
  def timeGenQuickSortDoubles(reps:Int) = run(reps)(quickSortGeneric(doubles.clone))
}
