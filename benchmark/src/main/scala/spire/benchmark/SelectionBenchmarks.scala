package spire.benchmark

import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.reflect.ClassTag

import scala.util.Random
import Random._

import spire.algebra._
import spire.math._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param


object SelectionBenchmarks extends MyRunner {
  val cls = classOf[SelectionBenchmarks]
}

class SelectionBenchmarks extends MyBenchmark with BenchmarkData {
  implicit val lexicographic:Order[Complex[Double]] = new Order[Complex[Double]] {
    def eqv(a: Complex[Double], b: Complex[Double]) = a == b
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

  override protected def setUp() {
    val size = fun.pow(2, pow).toInt

    is = if (typ == "int") mkarray(size)(nextInt) else null
    js = if (typ == "long") mkarray(size)(nextLong) else null
    fs = if (typ == "float") mkarray(size)(nextFloat) else null
    ds = if (typ == "double") mkarray(size)(nextDouble) else null
    cs = if (typ == "complex") mkarray(size)(Complex(nextDouble, nextDouble)) else null
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

object Mo5Benchmarks extends MyRunner {
  val cls = classOf[Mo5Benchmarks]
}

class Mo5Benchmarks extends MyBenchmark with BenchmarkData {
  val mo5_hb = new HighBranchingMedianOf5 { }
  val mo5_m = new MutatingMedianOf5 { }

  var as: Array[Int] = null

  val len = 5000000

  override protected def setUp() {
    as = new Array[Int](len)
    (0 until len) foreach { i =>
      as(i) = nextInt
    }
  }

  def timeHBMo5(reps:Int) = run(reps) {
    val a = as.clone()
    var i = 0
    while (i <= len - 5) {
      mo5_hb.mo5(a, i, 1)
      i += 5
    }
    a.length
  }

  def timeMMo5(reps:Int) = run(reps) {
    val a = as.clone()
    var i = 0
    while (i <= len - 5) {
      mo5_m.mo5(a, i, 1)
      i += 5
    }
    a.length
  }
}

