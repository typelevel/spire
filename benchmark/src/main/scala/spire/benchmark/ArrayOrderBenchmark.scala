package spire
package benchmark

/*
import scala.util.Random
import Random._

import spire.algebra._
import spire.implicits._

import com.google.caliper.Param

object ArrayOrderBenchmarks extends MyRunner(classOf[ArrayOrderBenchmarks])

class ArrayOrderBenchmarks extends MyBenchmark {
  @Param(Array("6", "10", "14", "18"))
  var pow: Int = 0

  var a: Array[Int] = null
  var b: Array[Int] = null
  var c: Array[Int] = null
  var d: Array[Int] = null
  var e: Array[Int] = null
  var f: Array[Int] = null

  override protected def setUp(): Unit = {
    val size = spire.math.pow(2, pow).toInt

    a = init(size)(nextInt)
    b = a.clone
    c = a.clone; c(1) += 1
    d = a.clone; d(size / 3) += 1
    e = a.clone; e(size - 7) += 1
    f = init(size + 10)(nextInt); System.arraycopy(a, 0, f, 0, a.length)
  }

  def directEq(x: Array[Int], y: Array[Int]): Boolean = {
    var i = 0
    if (x.length != y.length) return false
    while (i < x.length && i < y.length && x(i) === y(i)) i += 1
    i == x.length
  }

  def directCompare(x: Array[Int], y: Array[Int]): Int = {
    var i = 0
    val ev = Order[Int]
    while (i < x.length && i < y.length) {
      val cmp = ev.compare(x(i), y(i))
      if (cmp != 0) return cmp
      i += 1
    }
    x.length - y.length
  }

  def indirectAdd[@sp(Int) A: ClassTag: Ring](x: Array[A], y: Array[A]): Array[A] =
    spire.std.ArraySupport.plus(x, y)

  def directAdd(x: Array[Int], y: Array[Int]): Array[Int] = {
    val z = new Array[Int](spire.math.max(x.length, y.length))
    var i = 0
    while (i < x.length && i < y.length) { z(i) = x(i) + y(i); i += 1 }
    while (i < x.length) { z(i) = x(i); i += 1 }
    while (i < y.length) { z(i) = y(i); i += 1 }
    z
  }

  // def timeEqGeneric(reps: Int) = run(reps) { a === b }
  // def timeEqDirect(reps: Int) = run(reps) { directEq(a, b) }

  // def timeCompareGeneric(reps: Int) = run(reps) { a compare b }
  // def timeCompareDirect(reps: Int) = run(reps) { directCompare(a, b) }

  def timeAddGeneric(reps: Int) = run(reps) { a + b }
  def timeAddIndirect(reps: Int) = run(reps) { indirectAdd(a, b) }
  def timeAddDirect(reps: Int) = run(reps) { directAdd(a, b) }
}
*/