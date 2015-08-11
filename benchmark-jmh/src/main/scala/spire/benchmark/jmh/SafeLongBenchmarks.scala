package spire.benchmark.jmh

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.math.SafeLong

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SafeLongMultiplyBenchmark {

  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "lll" → ((SafeLong.two, SafeLong.two)),
    "llb" → ((SafeLong.two, SafeLong.safe64 - 1)),
    "lbl" → ((SafeLong.minusOne, SafeLong.safe64)),
    "lbb" → ((SafeLong.two, SafeLong.safe64)),
    "bll" → ((SafeLong.safe64, SafeLong.minusOne)),
    "blb" → ((SafeLong.safe64, SafeLong.one)),
    "bbb" → ((SafeLong.safe64, SafeLong.safe64))
  )

  @Param(Array("lll", "llb", "lbl", "lbb", "bll", "blb", "bbb"))
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
    "lll" → ((SafeLong.one, SafeLong.one)),
    "llb" → ((SafeLong.one, SafeLong.safe64 - 1)),
    "lbl" → ((SafeLong.minusOne, SafeLong.safe64)),
    "lbb" → ((SafeLong.one, SafeLong.safe64)),
    "bll" → ((SafeLong.safe64, SafeLong.minusOne)),
    "blb" → ((SafeLong.safe64, SafeLong.one)),
    "bbl" → ((SafeLong.safe64, -SafeLong.safe64)),
    "bbb" → ((SafeLong.safe64, SafeLong.safe64))
  )

  @Param(Array("lll", "llb", "lbl", "lbb", "bll", "blb", "bbl", "bbb"))
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