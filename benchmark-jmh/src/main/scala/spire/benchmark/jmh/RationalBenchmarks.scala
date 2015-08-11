package spire.benchmark.jmh

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.math.{Rational, SafeLong}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class RationalAddSubtractBenchmark {

  val pairs = Map(
    "integer" → ((Rational(12345), Rational(67890))),
    "biginteger" → ((Rational(12345) + Long.MaxValue, Rational(67890) + Long.MaxValue)),
    "small" →  ((Rational(12345,67891), Rational(67890,12347))),
    "medium" → ((Rational(Long.MaxValue,Int.MaxValue - 1), Rational(Long.MaxValue,Int.MaxValue - 3))),
    "large" → ((Rational(Long.MaxValue) + Rational(1,3), Rational(Long.MaxValue) + Rational(1,5))))

  @Param(Array("integer", "biginteger", "small", "medium", "large"))
  var kind: String = ""

  var a: Rational = Rational.zero

  var b: Rational = Rational.zero

  var c: Rational = Rational.zero

  @Setup
  def setup(): Unit = {
    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
    c = -b0
  }

  @Benchmark
  def sum(x: Blackhole): Unit = {
    x.consume(a + b)
  }

  @Benchmark
  def difference(x: Blackhole): Unit = {
    x.consume(a - b)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class RationalCompareBenchmark {
  val pairs = Map(
    "integer" → ((Rational(12345), Rational(67890))),
    "small" →  ((Rational(12345,67891), Rational(67890,12347))),
    "medium" → ((Rational(Long.MaxValue,Int.MaxValue - 1), Rational(Long.MaxValue,Int.MaxValue - 3))),
    "large" → ((Rational(Long.MaxValue) + Rational(1,3), Rational(Long.MaxValue) + Rational(1,5))))

  @Param(Array("integer", "small", "medium", "large"))
  var kind: String = ""

  var a: Rational = Rational.zero

  var b: Rational = Rational.zero

  @Setup
  def setup(): Unit = {
    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
  }

  @Benchmark
  def compare(x: Blackhole): Unit = {
    x.consume(a compare b)
  }
}