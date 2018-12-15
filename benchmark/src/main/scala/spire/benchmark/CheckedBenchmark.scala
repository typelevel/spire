package spire
package benchmark
/*
import spire.macros.Checked
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

  def timeOption(reps: Int) = run(reps) {
    var i = start
    var sum = 0
    while (i < limit) {
      sum = Checked.option(sum + i * i + i).getOrElse(0)
      i += 1
    }
    sum
  }

  def timeChecked(reps: Int) = run(reps) {
    var i = start
    var sum = 0
    while (i < limit) {
      try { sum = Checked.checked(sum + i * i + i) } catch { case _: Exception => sum = 0 }
      i += 1
    }
    sum
  }

  def timeTryOrElse(reps: Int) = run(reps) {
    var i = start
    var sum = 0
    while (i < limit) {
      sum = Checked.tryOrElse(sum + i * i + i)(0)
      i += 1
    }
    sum
  }

  def timeIncorrectRaw(reps: Int) = run(reps) {
    var i = start
    var sum = 0
    while (i < limit) {
      sum = sum + i * i + i
      i += 1
    }
    sum
  }
}
*/