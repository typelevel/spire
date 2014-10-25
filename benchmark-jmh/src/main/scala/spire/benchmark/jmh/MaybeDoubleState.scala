package spire.benchmark.jmh

import org.openjdk.jmh.annotations.{Scope, Setup, State}
import spire.math.algebraic.MaybeDouble

import scala.util.Random._

@State(Scope.Thread)
class MaybeDoubleState extends StateSupport {
  var values: Array[MaybeDouble] = _
  @Setup
  def setup: Unit = values = init(size)(MaybeDouble(nextDouble))
}
