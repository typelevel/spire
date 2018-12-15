package spire
package benchmark

/*
import scala.util.Random
import Random._

import spire.math._
import spire.implicits._

object Mo5Benchmarks extends MyRunner(classOf[Mo5Benchmarks])

class Mo5Benchmarks extends MyBenchmark {
  val mo5_hb = new HighBranchingMedianOf5 { }
  val mo5_m = new MutatingMedianOf5 { }

  var as: Array[Int] = null

  val len = 5000000

  override protected def setUp(): Unit = {
    as = init(len)(nextInt)
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
*/