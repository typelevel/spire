package spire
package benchmark.jmh

import org.openjdk.jmh.annotations.{Scope, Setup, State}

import scala.util.Random._

@State(Scope.Thread)
class FloatState extends StateSupport {
  var values: Array[Float] = _
  @Setup
  def setup(): Unit = values = init(size)(nextFloat)
}
