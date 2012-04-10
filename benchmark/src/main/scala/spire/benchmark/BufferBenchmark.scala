package spire.benchmark

import scala.{specialized => spec}
import scala.util.Random
import Random._

import scala.collection.mutable.ArrayBuffer
import spire.buffer._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

object BufferBenchmarks extends MyRunner { val cls = classOf[BufferBenchmarks] }
class BufferBenchmarks extends MyBenchmark with BenchmarkData {
  @Param(
    Array(
      "10", "15", "17", "20", "21"//,
      //"22", "23",
      //"24", "25"
    )
  )
  var pow:Int = 0

  var data:Array[Long] = null
  var abuf:ArrayBuffer[Long] = null
  var sbuf:Buffer[Long] = null

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextLong)
    abuf = ArrayBuffer(data:_*)
    sbuf = Mutable.safe(data)
  }

  def appendArrayBuffer:Long = {
    val len = data.length
    val bf = scala.collection.mutable.ArrayBuffer.empty[Long]
    var i = 0
    while (i < len) {
      bf.append(data(i))
      i += 1
    }
    var total = 0L
    i = 0
    while (i < len) {
      total += bf(i)
      i += 1
    }
    total
  }

  def appendSpireBuffer:Long = {
    val len = data.length
    val bf = spire.buffer.Mutable.empty[Long]
    var i = 0
    while (i < len) {
      bf.append(data(i))
      i += 1
    }
    var total = 0L
    i = 0
    while (i < len) {
      total += bf(i)
      i += 1
    }
    total
  }

  def foreachArrayBuffer:Long = {
    var i = 0
    var total = 0L
    abuf.foreach(total += _)
    total
  }

  def foreachSpireBuffer:Long = {
    var i = 0
    var total = 0L
    sbuf.foreach(total += _)
    total
  }

  def foreachArray:Long = {
    var i = 0
    var total = 0L
    data.foreach(total += _)
    total
  }

  def whileArrayBuffer:Long = {
    var i = 0
    val len = abuf.length
    var total = 0L
    while (i < len) {
      total += abuf(i)
      i += 1
    }
    total
  }

  def whileSpireBuffer:Long = {
    var i = 0
    val len = sbuf.length
    var total = 0L
    while (i < len) {
      total += sbuf(i)
      i += 1
    }
    total
  }

  def whileArray:Long = {
    var i = 0
    val len = data.length
    var total = 0L
    while (i < len) {
      total += data(i)
      i += 1
    }
    total
  }

  def timeAppendArrayBuffer(reps:Int) = run(reps)(appendArrayBuffer)
  def timeAppendSpireBuffer(reps:Int) = run(reps)(appendSpireBuffer)

  def timeForeachArrayBuffer(reps:Int) = run(reps)(foreachArrayBuffer)
  def timeForeachSpireBuffer(reps:Int) = run(reps)(foreachSpireBuffer)
  def timeForeachArray(reps:Int) = run(reps)(foreachArray)

  def timeWhileArrayBuffer(reps:Int) = run(reps)(whileArrayBuffer)
  def timeWhileSpireBuffer(reps:Int) = run(reps)(whileSpireBuffer)
  def timeWhileArray(reps:Int) = run(reps)(whileArray)
}
