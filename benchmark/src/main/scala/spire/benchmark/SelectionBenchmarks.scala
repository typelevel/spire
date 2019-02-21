package spire
package benchmark

/*
import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.implicits._

import com.google.caliper.Param

object SelectionBenchmarks extends MyRunner(classOf[SelectionBenchmarks])

class SelectionBenchmarks extends MyBenchmark {
  implicit val lexicographic:Order[Complex[Double]] = new Order[Complex[Double]] {
    override def eqv(a: Complex[Double], b: Complex[Double]) = a == b
    def compare(a: Complex[Double], b: Complex[Double]): Int = {
      if (a.real < b.real) -1
      else if (a.real > b.real) 1
      else if (a.imag < b.imag) -1
      else if (a.imag > b.imag) 1
      else 0
    }
  }

  @Param(Array("3", "4", "6", "9", "13", "18"))
  var pow: Int = 0

  @Param(Array("int")) //, "long", "float", "double", "complex"))
  var typ: String = null

  //@Param(Array("random", "sorted", "reversed"))
  @Param(Array("random"))
  var layout: String = null

  var is: Array[Int] = null
  var js: Array[Long] = null
  var fs: Array[Float] = null
  var ds: Array[Double] = null
  var cs: Array[Complex[Double]] = null
  var cs2: Array[FakeComplex[Double]] = null

  def mkarray[A:ClassTag:Order](size:Int)(init: => A): Array[A] = {
    val data = Array.ofDim[A](size)
    var i = 0
    while (i < size) { data(i) = init; i += 1 }
    if (layout == "random") return data
    spire.math.Sorting.sort(data)
    if (layout == "sorted") data else data.reverse
  }

  override protected def setUp(): Unit = {
    val size = spire.math.pow(2, pow).toInt

    is = if (typ == "int") mkarray(size, layout)(nextInt) else null
    js = if (typ == "long") mkarray(size, layout)(nextLong) else null
    fs = if (typ == "float") mkarray(size, layout)(nextFloat) else null
    ds = if (typ == "double") mkarray(size, layout)(nextDouble) else null
    cs = if (typ == "complex") mkarray(size, layout)(nextComplex) else null
    cs2 = if (typ == "complex") cs.map(c => new FakeComplex(c.real, c.imag)) else null
  }

  def timeSpireQuickSelect(reps:Int) = run(reps) {
    if (typ == "int") {
      val arr = is.clone; spire.math.Selection.quickSelect(arr, arr.length / 2); arr.length
    } else if (typ == "long") {
      val arr = js.clone; spire.math.Selection.quickSelect(arr, arr.length / 2); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; spire.math.Selection.quickSelect(arr, arr.length / 2); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; spire.math.Selection.quickSelect(arr, arr.length / 2); arr.length
    } else if (typ == "complex") {
      val arr = cs.clone; spire.math.Selection.quickSelect(arr, arr.length / 2); arr.length
    }
  }

  def timeSpireLinearSelect(reps:Int) = run(reps) {
    if (typ == "int") {
      val arr = is.clone; spire.math.Selection.linearSelect(arr, arr.length / 2); arr.length
    } else if (typ == "long") {
      val arr = js.clone; spire.math.Selection.linearSelect(arr, arr.length / 2); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; spire.math.Selection.linearSelect(arr, arr.length / 2); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; spire.math.Selection.linearSelect(arr, arr.length / 2); arr.length
    } else if (typ == "complex") {
      val arr = cs.clone; spire.math.Selection.linearSelect(arr, arr.length / 2); arr.length
    }
  }
}
*/