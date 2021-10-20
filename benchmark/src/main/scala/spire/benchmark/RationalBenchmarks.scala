/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.math.Rational
import RationalUtil._

object RationalUtil {
  private def isBig(x: Rational) = x.getClass.getSimpleName.endsWith("BigRational")

  def classify(a: Rational): String =
    (if (isBig(a)) "b" else "l") + (if (a.isWhole) "i" else "f")

  def classify(a: Rational, op_a: Rational): String =
    classify(a) + "_" + classify(op_a)

  def classify(a: Rational, b: Rational, a_op_b: Rational): String =
    classify(a) + "_" + classify(b) + "_" + classify(a_op_b)

  def check(cases: Map[String, (Rational, Rational)]): Unit = {
    for ((kind, (a, b)) <- cases) {
      val c = classify(a, b)
      require(kind.startsWith(c), s"Unexpected class $c for case $kind")
    }
  }

  def check(cases: Map[String, (Rational, Rational)], op: (Rational, Rational) => Rational): Unit = {
    for ((kind, (a, b)) <- cases) {
      val c = classify(a, b, op(a, b))
      require(kind.startsWith(c), s"Unexpected class $c for case $kind")
    }
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class RationalMultiplyDivideBenchmark {

  val pairs = Map(
    "li_li_li" -> ((Rational(12345), Rational(67890))),
    "bi_bi_bi" -> ((Rational(12345) + Long.MaxValue, Rational(67890) + Long.MaxValue)),
    "lf_lf_lf" -> ((Rational(12345, 67891), Rational(67890, 12347))),
    "lf_lf_bf" -> ((Rational(Long.MaxValue, Int.MaxValue - 1), Rational(Long.MaxValue, Int.MaxValue - 3))),
    "bf_bf_bf" -> ((Rational(Long.MaxValue) + Rational(1, 3), Rational(Long.MaxValue) + Rational(1, 5)))
  )
  check(pairs, _ * _)
  check(pairs, _ / _.inverse)

  @Param(Array("li_li_li", "bi_bi_bi", "lf_lf_lf", "lf_lf_bf", "bf_bf_bf"))
  var kind: String = ""

  var a: Rational = Rational.zero

  var b: Rational = Rational.zero

  var c: Rational = Rational.zero

  @Setup
  def setup(): Unit = {
    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
    c = b0.inverse
  }

  @Benchmark
  def product(x: Blackhole): Unit = {
    x.consume(a * b)
  }

  @Benchmark
  def quotient(x: Blackhole): Unit = {
    x.consume(a / b)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class RationalAddSubtractBenchmark {

  val pairs = Map(
    "li_li_li" -> ((Rational(12345), Rational(67890))),
    "bi_bi_bi" -> ((Rational(12345) + Long.MaxValue, Rational(67890) + Long.MaxValue)),
    "lf_lf_lf" -> ((Rational(12345, 67891), Rational(67890, 12347))),
    "lf_lf_bf" -> ((Rational(Long.MaxValue, Int.MaxValue - 1), Rational(Long.MaxValue, Int.MaxValue - 3))),
    "bf_bf_bf" -> ((Rational(Long.MaxValue) + Rational(1, 3), Rational(Long.MaxValue) + Rational(1, 5)))
  )
  check(pairs, _ + _)
  check(pairs, _ - -_)

  @Param(Array("li_li_li", "bi_bi_bi", "lf_lf_lf", "lf_lf_bf", "bf_bf_bf"))
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
    "li_li" -> ((Rational(12345), Rational(67890))),
    "lf_lf" -> ((Rational(12345, 67891), Rational(67890, 12347))),
    "lf_lf_intermediateBig" -> ((Rational(Long.MaxValue, Int.MaxValue - 1), Rational(Long.MaxValue, Int.MaxValue - 3))),
    "bf_bf" -> ((Rational(Long.MaxValue) + Rational(1, 3), Rational(Long.MaxValue) + Rational(1, 5)))
  )
  check(pairs)

  @Param(Array("li_li", "lf_lf", "lf_lf_intermediateBig", "bf_bf"))
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
    x.consume(a.compare(b))
  }
}
