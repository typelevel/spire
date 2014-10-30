package spire.benchmark

import scala.util.Random
import spire.macros.{Checked, Checked2, Checked3, Checked4}
import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

object CheckedBenchmarks extends MyRunner(classOf[CheckedBenchmarks])

class CheckedBenchmarks extends MyBenchmark {

  @Param(Array("0", "100000000"))
  var start: Int = 0

  @Param(Array("100", "1000", "10000"))
  var len: Int = 0

  var limit: Int = 0

  override def setUp(): Unit =
    limit = start + len

  def timeChecked(reps: Int) = run(reps) {
    var i = start
    var sum = 0
    while (i < limit) {
      try { sum = Checked.checked(sum + i * i + i) } catch { case _: Exception => sum = 0 }
      i += 1
    }
    sum
  }

  def timeChecked2(reps: Int) = run(reps) {
    var i = start
    var sum = 0
    while (i < limit) {
      sum = Checked2.tryOrElse(sum + i * i + i)(0)
      i += 1
    }
    sum
  }


  def timeChecked4(reps: Int) = run(reps) {
    var i = start
    var sum = 0
    while (i < limit) {
      sum = Checked4.tryOrReturn(sum + i * i + i)(0)
      i += 1
    }
    sum
  }
}
