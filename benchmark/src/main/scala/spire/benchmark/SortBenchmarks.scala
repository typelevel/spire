package spire.benchmark

import scala.{specialized => spec}
import scala.annotation.tailrec

import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import fpf._

import spire.math.{Numeric => SpireN}
import scala.math.{Numeric => ScalaN}

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

object SortingBenchmarks extends MyRunner {
  val cls = classOf[SortingBenchmarks]
}

final class FakeComplex[@spec(Float, Double) T](val real:T, val imag:T)(implicit f:Fractional[T], t:Trig[T]) extends Ordered[FakeComplex[T]] {
  def compare(b: FakeComplex[T]): Int = {
    if (f.lt(real, b.real)) -1
    else if (f.gt(real, b.real)) 1
    else if (f.lt(imag, b.imag)) -1
    else if (f.gt(imag, b.imag)) 1
    else 0
  }
}


class SortingBenchmarks extends MyBenchmark with BenchmarkData {
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

  @Param(Array("int", "long", "float", "double", "complex"))
  var typ: String = null

  var is: Array[Int] = null
  var js: Array[Long] = null
  var fs: Array[Float] = null
  var ds: Array[Double] = null
  var cs: Array[Complex[Double]] = null
  var cs2: Array[FakeComplex[Double]] = null

  override protected def setUp() {
    val size = fun.pow(2, pow).toInt

    is = if (typ == "int") init(size)(nextInt) else null
    js = if (typ == "long") init(size)(nextLong) else null
    fs = if (typ == "float") init(size)(nextFloat) else null
    ds = if (typ == "double") init(size)(nextDouble) else null
    cs = if (typ == "complex") init(size)(Complex(nextDouble, nextDouble)) else null
    cs2 = if (typ == "complex") cs.map(c => new FakeComplex(c.real, c.imag)) else null
  }

  def timeJavaSort(reps:Int) = run(reps) {
    if (typ == "int") {
      val arr = is.clone; java.util.Arrays.sort(arr); arr.length
    } else if (typ == "long") {
      val arr = js.clone; java.util.Arrays.sort(arr); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; java.util.Arrays.sort(arr); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; java.util.Arrays.sort(arr); arr.length
    } else if (typ == "complex") {
      val arr = cs2.clone.asInstanceOf[Array[Object]]; java.util.Arrays.sort(arr); arr.length
    }
  }
  
  def timeScalaQuicksort(reps:Int) = run(reps) {
    if (typ == "int") {
      val arr = is.clone; scala.util.Sorting.quickSort(arr); arr.length
    } else if (typ == "long") {
      val arr = js.clone; scala.util.Sorting.quickSort(arr); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; scala.util.Sorting.quickSort(arr); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; scala.util.Sorting.quickSort(arr); arr.length
    } else if (typ == "complex") {
      implicit val ordering = Order.ordering(lexicographic)
      val arr = cs.clone; scala.util.Sorting.quickSort(arr); arr.length
    }
  }
  
  def timeSpireInsertionSort(reps:Int): Unit = run(reps) {
    val n = if (pow > 13) 8000 else fun.pow(2, pow).toInt

    if (typ == "int") {
      val arr = is.clone; spire.math.InsertionSort.sort(arr, 0, n); arr.length
    } else if (typ == "long") {
      val arr = js.clone; spire.math.InsertionSort.sort(arr, 0, n); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; spire.math.InsertionSort.sort(arr, 0, n); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; spire.math.InsertionSort.sort(arr, 0, n); arr.length
    } else if (typ == "complex") {
      val arr = cs.clone; spire.math.InsertionSort.sort(arr, 0, n); arr.length
    }
  }
  
  def timeSpireMergeSort(reps:Int) = run(reps) {
    if (typ == "int") {
      val arr = is.clone; spire.math.Sorting.mergeSort(arr); arr.length
    } else if (typ == "long") {
      val arr = js.clone; spire.math.Sorting.mergeSort(arr); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; spire.math.Sorting.mergeSort(arr); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; spire.math.Sorting.mergeSort(arr); arr.length
    } else if (typ == "complex") {
      val arr = cs.clone; spire.math.Sorting.mergeSort(arr); arr.length
    }
  }

  def timeSpireQuickSort(reps:Int) = run(reps) {
    if (typ == "int") {
      val arr = is.clone; spire.math.Sorting.quickSort(arr); arr.length
    } else if (typ == "long") {
      val arr = js.clone; spire.math.Sorting.quickSort(arr); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; spire.math.Sorting.quickSort(arr); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; spire.math.Sorting.quickSort(arr); arr.length
    } else if (typ == "complex") {
      val arr = cs.clone; spire.math.Sorting.quickSort(arr); arr.length
    }
  }
}
