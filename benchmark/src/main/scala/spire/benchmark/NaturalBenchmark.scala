package spire.benchmark

import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.implicits._
import fpf._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import java.lang.Math

object NaturalBenchmarks extends MyRunner(classOf[NaturalBenchmarks])

class NaturalBenchmarks extends MyBenchmark {
  //@Param(Array("8", "16", "32", "64", "96", "128", "192", "256"))
  @Param(Array("8"))
  var bits: Int = 0

  var size: Int = 0

  //@Param(Array("10", "15", "20"))
  @Param(Array("10"))
  var pow: Int = 0

  var nats: Array[Natural] = _
  var bigints: Array[BigInt] = _
  var safes: Array[SafeLong] = _

  override def setUp() {
    size = Math.pow(2, pow).toInt
    bigints = init(size)(BigInt(bits, Random))
    nats = bigints.map(Natural(_))
    safes = bigints.map(SafeLong(_))
  }

  // def timeNaturalSum(reps: Int) = run(reps) {
  //   var sum = Natural(0)
  //   nats.foreach(n => sum += n)
  //   sum
  // }

  // def timeBigIntSum(reps: Int) = run(reps) {
  //   var sum = BigInt(0)
  //   bigints.foreach(n => sum += n)
  //   sum
  // }

  // def timeSafeLongSums(reps: Int) = run(reps) {
  //   var sum = SafeLong(0)
  //   safes.foreach(n => sum += n)
  //   sum
  // }

  def timeNaturalScale(reps: Int) = run(reps) {
    var sum = Natural(0)
    //nats.map(n => sum *= 2)
    nats.map(n => sum + 1)
    sum
  }

  def timeBigIntScale(reps: Int) = run(reps) {
    var sum = BigInt(0)
    //bigints.foreach(n => sum * 2)
    bigints.foreach(n => sum << 1)
    sum
  }

  def timeSafeLongScales(reps: Int) = run(reps) {
    var sum = SafeLong(0)
    //safes.foreach(n => sum * 2)
    safes.foreach(n => sum * 2)
    sum
  }
}
