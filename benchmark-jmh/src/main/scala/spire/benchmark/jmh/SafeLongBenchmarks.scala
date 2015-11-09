package spire
package benchmark.jmh

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.math.SafeLong
import SafeLongUtil._

object SafeLongUtil {
  private def isBig(x: SafeLong) = x.getClass.getSimpleName.endsWith("BigInteger")

  def classify(a: SafeLong): String =
    if (isBig(a)) "b" else "l"

  def classify(a: SafeLong, op_a: SafeLong): String =
    classify(a) + "_" + classify(op_a)

  def classify(a: SafeLong, b: SafeLong, a_op_b: SafeLong): String =
    classify(a) + "_" + classify(b) + "_" + classify(a_op_b)

  def check(cases: Map[String, (SafeLong, SafeLong)]): Unit = {
    for ((kind, (a, b)) ← cases) {
      val c = classify(a, b)
      require(kind.startsWith(c), s"Unexpected class $c for case $kind")
    }
  }

  def check(cases: Map[String, (SafeLong, SafeLong)], op: (SafeLong, SafeLong) ⇒ SafeLong): Unit = {
    for ((kind, (a, b)) ← cases) {
      val c = classify(a, b, op(a, b))
      require(kind.startsWith(c), s"Unexpected class $c for case $kind")
    }
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SafeLongMultiplyBenchmark {

  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "l_l_l" → ((SafeLong.two, SafeLong.two)),
    "l_l_b" → ((SafeLong.two, SafeLong.safe64 - 1)),
    "l_b_l" → ((SafeLong.minusOne, SafeLong.safe64)),
    "l_b_b" → ((SafeLong.two, SafeLong.safe64)),
    "b_l_l" → ((SafeLong.safe64, SafeLong.minusOne)),
    "b_l_b" → ((SafeLong.safe64, SafeLong.two)),
    "b_b_b" → ((SafeLong.safe64, SafeLong.safe64))
  )
  check(pairs, _ * _)

  @Param(Array("l_l_l", "l_l_b", "l_b_l", "l_b_b", "b_l_l", "b_l_b", "b_b_b"))
  var kind: String = ""

  var a: SafeLong = 0L

  var b: SafeLong = 0L

  var c: SafeLong = 0L

  @Setup
  def setup(): Unit = {
    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
    c = -b0
  }

  @Benchmark
  def multiply(x: Blackhole): Unit = {
    x.consume(a * b)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SafeLongAddSubtractBenchmark {

  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "l_l_l" → ((SafeLong.one, SafeLong.one)),
    "l_l_b" → ((SafeLong.one, SafeLong.safe64 - 1)),
    "l_b_l" → ((SafeLong.minusOne, SafeLong.safe64)),
    "l_b_b" → ((SafeLong.one, SafeLong.safe64)),
    "b_l_l" → ((SafeLong.safe64, SafeLong.minusOne)),
    "b_l_b" → ((SafeLong.safe64, SafeLong.one)),
    "b_b_l" → ((SafeLong.safe64, -SafeLong.safe64 - 1)),
    "b_b_b" → ((SafeLong.safe64, SafeLong.safe64))
  )
  check(pairs, _ + _)
  check(pairs, _ - -_)

  @Param(Array("l_l_l", "l_l_b", "l_b_l", "l_b_b", "b_l_l", "b_l_b", "b_b_l", "b_b_b"))
  var kind: String = ""
  var a: SafeLong = 0L
  var b: SafeLong = 0L
  var c: SafeLong = 0L

  @Setup
  def setup(): Unit = {


    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
    c = -b0
  }

  @Benchmark
  def add(x: Blackhole): Unit = {
    x.consume(a + b)
  }

  def subtract(x: Blackhole): Unit = {
    x.consume(a - c)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SafeLongCompareBenchmark {

  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "l_l" → ((SafeLong.one, SafeLong.one + 1)),
    "b_b" → ((SafeLong.safe64, SafeLong.safe64 + 1))
  )
  check(pairs)

  @Param(Array("l_l", "b_b"))
  var kind: String = ""
  var a: SafeLong = 0L
  var b: SafeLong = 0L

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
