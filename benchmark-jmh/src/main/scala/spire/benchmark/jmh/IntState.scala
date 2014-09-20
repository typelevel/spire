package spire.benchmark.jmh

import org.openjdk.jmh.annotations.{Scope, Setup, State}

import scala.util.Random._

@State(Scope.Thread)
class IntState extends StateSupport {
  var values: Array[Int] = _
  @Setup
  def setup: Unit = values = init(size)(nextInt)
}
