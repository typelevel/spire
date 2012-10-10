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

object SortBenchmarks extends MyRunner {
  val cls = classOf[SortBenchmarks]
}

class SortBenchmarks extends MyBenchmark with BenchmarkData {
  //@Param(Array("6", "8", "11", "14", "17", "20"))
  @Param(Array("3", "4", "6", "9", "13"))
  var pow: Int = 0

  //@Param(Array("int", "long", "float", "double"))
  @Param(Array("int"))
  var typ: String = null

  var is: Array[Int] = null
  var js: Array[Long] = null
  var fs: Array[Float] = null
  var ds: Array[Double] = null

  override protected def setUp() {
    val size = fun.pow(2, pow).toInt

    is = if (typ == "int") init(size)(nextInt) else null
    js = if (typ == "long") init(size)(nextLong) else null
    fs = if (typ == "float") init(size)(nextFloat) else null
    ds = if (typ == "double") init(size)(nextDouble) else null
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
    }
  }
  
  def timeSpireInsertionSort(reps:Int) = run(reps) {
    if (typ == "int") {
      val arr = is.clone; spire.math.Sorting.insertionSort(arr); arr.length
    } else if (typ == "long") {
      val arr = js.clone; spire.math.Sorting.insertionSort(arr); arr.length
    } else if (typ == "float") {
      val arr = fs.clone; spire.math.Sorting.insertionSort(arr); arr.length
    } else if (typ == "double") {
      val arr = ds.clone; spire.math.Sorting.insertionSort(arr); arr.length
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
    }
  }
}
