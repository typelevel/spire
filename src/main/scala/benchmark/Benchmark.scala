package benchmark

import scala.{specialized => spec}
import scala.util.Random

import numerics.math._
import numerics.math.Implicits._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark

// extend this to create an actual benchmarking class
trait MyBenchmark extends SimpleBenchmark {
  // sugar for building arrays using a per-cell init function
  def init[A:Manifest](size:Int)(init: =>A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  // sugar for building randomized arrays of various types
  def initInts(size:Int) = init(size)(Random.nextInt())
  def initLongs(size:Int) = init(size)(Random.nextLong())
  def initFloats(size:Int) = init(size)(Random.nextFloat())
  def initDoubles(size:Int) = init(size)(Random.nextDouble())

  // sugar to run 'f' for 'reps' number of times
  def run(reps:Int)(f: =>Unit) = for(i <- 0 until reps)(f)
}

// extend this to create a main object which will run 'cls' (a benchmark)
trait MyRunner {
  val cls:java.lang.Class[_ <: com.google.caliper.Benchmark]
  def main(args:Array[String]) {
    println("starting benchmarks...")
    Runner.main(cls, args:_*)
    println("completed benchmarks.")
  }
}


/**
 * AddBenchmarks
 *
 * Benchmarks the speed of direct/generic addition.
 */
object AddBenchmarks extends MyRunner { val cls = classOf[AddBenchmarks] }
class AddBenchmarks extends MyBenchmark {
  private val size = 10 * 1000 * 1000

  val ints = initInts(size)
  val longs = initLongs(size)
  val floats = initFloats(size)
  val doubles = initDoubles(size)

  val complexes = init(100 * 1000) {
    Complex(Random.nextDouble(), Random.nextDouble())
  }

  def addGeneric[@spec(Int, Long, Float, Double) A:Ring](data:Array[A]):A = {
    var total = ring.zero
    var i = 0
    val len = data.length
    while (i < len) { total = ring.plus(total, data(i)); i += 1 }
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

  def timeAddIntsDirect(reps:Int) = run(reps)(addIntsDirect(ints))
  def timeAddIntsGeneric(reps:Int) = run(reps)(addGeneric(ints))

  def timeAddLongsDirect(reps:Int) = run(reps)(addLongsDirect(longs))
  def timeAddLongsGeneric(reps:Int) = run(reps)(addGeneric(longs))

  def timeAddFloatsDirect(reps:Int) = run(reps)(addFloatsDirect(floats))
  def timeAddFloatsGeneric(reps:Int) = run(reps)(addGeneric(floats))

  def timeAddDoublesDirect(reps:Int) = run(reps)(addDoublesDirect(doubles))
  def timeAddDoublesGeneric(reps:Int) = run(reps)(addGeneric(doubles))

  def timeAddComplexesDirect(reps:Int) = run(reps)(addComplexesDirect(complexes))
  def timeAddComplexesGeneric(reps:Int) = run(reps)(addGeneric(complexes))
}

