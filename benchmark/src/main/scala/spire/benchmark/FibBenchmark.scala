package spire
package benchmark

import spire.algebra._
import spire.math._
import spire.implicits._

import java.math.BigInteger
/*
object BigIntegerIsRig extends Rig[BigInteger] {
  def zero: BigInteger = new BigInteger("0")
  def one: BigInteger = new BigInteger("1")
  def plus(x: BigInteger, y: BigInteger): BigInteger = x.add(y)
  def times(x: BigInteger, y: BigInteger): BigInteger = x.multiply(y)
}

object FibBenchmarks extends MyRunner(classOf[FibBenchmarks])

class FibBenchmarks extends MyBenchmark {

  implicit val bigIntegerIsRig = BigIntegerIsRig

  def timeIntFibJava(reps: Int) = run(reps) {
    val a = JavaFib.intfib(41)
    val b = JavaFib.intfib(42)
    val c = JavaFib.intfib(43)
    val d = JavaFib.intfib(44)
    a + b + c + d
  }
  def timeIntFibScala(reps: Int) = run(reps) {
    val a = scalaIntFib(41)
    val b = scalaIntFib(42)
    val c = scalaIntFib(43)
    val d = scalaIntFib(44)
    a + b + c + d
  }
  def timeIntFibGen(reps: Int) = run(reps) {
    val a = scalaGenFib[Int](41)
    val b = scalaGenFib[Int](42)
    val c = scalaGenFib[Int](43)
    val d = scalaGenFib[Int](44)
    a + b + c + d
  }

  def timeLongFibJava(reps: Int) = run(reps) {
    val a = JavaFib.longfib(81)
    val b = JavaFib.longfib(82)
    val c = JavaFib.longfib(83)
    val d = JavaFib.longfib(84)
    a + b + c + d
  }
  def timeLongFibScala(reps: Int) = run(reps) {
    val a = scalaLongFib(81)
    val b = scalaLongFib(82)
    val c = scalaLongFib(83)
    val d = scalaLongFib(84)
    a + b + c + d
  }
  def timeLongFibGen(reps: Int) = run(reps) {
    val a = scalaGenFib[Long](81)
    val b = scalaGenFib[Long](82)
    val c = scalaGenFib[Long](83)
    val d = scalaGenFib[Long](84)
    a + b + c + d
  }

  def timeBigIntegerFibJava(reps: Int) = run(reps) {
    val a = JavaFib.bigfib(201)
    val b = JavaFib.bigfib(202)
    val c = JavaFib.bigfib(203)
    val d = JavaFib.bigfib(204)
    a.add(b).add(c).add(d)
  }
  def timeBigIntegerFibGen(reps: Int) = run(reps) {
    val a = scalaGenFib[BigInt](201)
    val b = scalaGenFib[BigInt](202)
    val c = scalaGenFib[BigInt](203)
    val d = scalaGenFib[BigInt](204)
    a + b + c + d
  }

  def timeBigIntFibScala(reps: Int) = run(reps) {
    val a = scalaBigFib(201)
    val b = scalaBigFib(202)
    val c = scalaBigFib(203)
    val d = scalaBigFib(204)
    a + b + c + d
  }
  def timeBigIntFibGen(reps: Int) = run(reps) {
    val a = scalaGenFib[BigInt](201)
    val b = scalaGenFib[BigInt](202)
    val c = scalaGenFib[BigInt](203)
    val d = scalaGenFib[BigInt](204)
    a + b + c + d
  }

  def timeNaturalFibGen(reps: Int) = run(reps) {
    val a = scalaGenFib[Natural](201)
    val b = scalaGenFib[Natural](202)
    val c = scalaGenFib[Natural](203)
    val d = scalaGenFib[Natural](204)
    a + b + c + d
  }

  def timeSpireFib(reps: Int) = run(reps) {
    val a = spire.math.fib(201)
    val b = spire.math.fib(202)
    val c = spire.math.fib(203)
    val d = spire.math.fib(204)
    a + b + c + d
  }

  def scalaGenFib[@sp(Int, Long) A](n: Int)(implicit r: Rig[A]): A = {
    @tailrec def loop(n :Int, a: A, b: A): A =
      if (n == 0) a else loop(n - 1, b, a + b)
    loop(n, r.zero, r.one)
  }

  def scalaIntFib(n: Int) = {
    @tailrec def loop(n: Int, a: Int, b: Int): Int =
      if (n == 0) a else loop(n - 1, b, a + b)
    loop(n, 0, 1)
  }

  def scalaLongFib(n: Int) = {
    @tailrec def loop(n: Int, a: Long, b: Long): Long =
      if (n == 0) a else loop(n - 1, b, a + b)
    loop(n, 0L, 1L)
  }

  def scalaBigFib(n: Int) = {
    @tailrec def loop(n: Int, a: BigInt, b: BigInt): BigInt =
      if (n == 0) a else loop(n - 1, b, a + b)
    loop(n, BigInt(0), BigInt(1))
  }
}
*/