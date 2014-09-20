package spire.benchmark.jmh

import org.openjdk.jmh.annotations.{Scope, Setup, State}

import scala.util.Random._

@State(Scope.Thread)
class LongState extends StateSupport {
  var values: Array[Long] = _
  @Setup
  def setup: Unit = values = init(size)(nextLong)
}
