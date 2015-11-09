package spire
package benchmark.jmh

import org.openjdk.jmh.annotations.{Scope, Setup, State}

import scala.util.Random._

@State(Scope.Thread)
class DoubleState extends StateSupport {
  var values: Array[Double] = _
  @Setup
  def setup(): Unit = values = init(size)(nextDouble)
}
