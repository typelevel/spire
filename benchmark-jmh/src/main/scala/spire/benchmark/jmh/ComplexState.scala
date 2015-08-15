package spire.benchmark.jmh

import org.openjdk.jmh.annotations._
import spire.math.Complex

@State(Scope.Thread)
class ComplexState extends StateSupport {
  var values: Array[Complex[Double]] = _
  @Setup
  def setup(): Unit = values = init(size)(nextComplex)
}
