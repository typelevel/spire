package spire
package benchmark.jmh

import org.openjdk.jmh.annotations.{Scope, Setup, State}
import spire.math.FastComplex

import scala.util.Random._

@State(Scope.Thread)
class FastComplexState extends StateSupport {
  var values: Array[Long] = _
  @Setup
  def setup(): Unit = values = init(size)(FastComplex(nextFloat(), nextFloat()))
}
