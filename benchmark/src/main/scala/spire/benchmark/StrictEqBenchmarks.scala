package spire
package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.implicits._

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class LongEqualityCheckBenchmark {

  var a: Long = 0L

  var b: Long = 0L

  @Benchmark
  def compare(x: Blackhole): Unit = {
    x.consume(a.compare(b))
  }

  @Benchmark
  def eqeq(x: Blackhole): Unit = {
    x.consume(a == b)
  }

  @Benchmark
  def eqeqeq(x: Blackhole): Unit = {
    x.consume(a === b)
  }
}
