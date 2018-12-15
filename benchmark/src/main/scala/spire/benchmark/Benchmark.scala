package spire
package benchmark

/*
import scala.util.Random
import Random._

import spire.math._

import com.google.caliper.Runner
import com.google.caliper.SimpleBenchmark

/**
 * Extend this to create an actual benchmarking class.
 */
trait MyBenchmark extends SimpleBenchmark with FixtureSupport {

  /**
   * Sugar to run 'f' for 'reps' number of times.
   */
  def run[A](reps:Int)(f: => A): A = {
    def loop(a: A, i: Int): A = if (i < reps) loop(f, i + 1) else a
    if (reps < 1) sys.error("!") else loop(f, 1)
  }
}

/**
 * Extend this to create a main object which will run 'cls' (a benchmark).
 */
abstract class MyRunner(val cls:java.lang.Class[_ <: com.google.caliper.Benchmark]) {
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

  lazy val complexes = init(size)(nextComplex)
  lazy val fcomplexes = init(size)(FastComplex(nextFloat(), nextFloat()))
}
*/