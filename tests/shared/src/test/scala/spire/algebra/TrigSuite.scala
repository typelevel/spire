package spire
package algebra

import spire.math._
import spire.implicits.DoubleAlgebra

class TrigSuite extends munit.FunSuite {

  val epsilon: Double = 1e-15

  final class RelativeOps(lhs: Double) {
    def within(rhs: Double) = {
      val ok = abs(lhs - rhs) < epsilon
      if (!ok) println("failed: abs(%s - %s) < %s".format(lhs, rhs, epsilon))
      assert(ok)
    }
  }
  implicit def relativeOps(lhs: Double): RelativeOps = new RelativeOps(lhs)

  test("Trig[Double]") {
    val t = implicitly[Trig[Double]]

    assertEquals(t.e, spire.math.e)
    assertEquals(t.pi, spire.math.pi)

    t.sin(0.0).within(0.0)
    t.sin(t.pi / 2).within(1.0)
    t.sin(t.pi).within(0.0)
    t.sin(3 * t.pi / 2).within(-1.0)
    t.sin(2 * t.pi).within(0.0)

    t.cos(0.0).within(1.0)
    t.cos(t.pi / 2).within(0.0)
    t.cos(t.pi).within(-1.0)
    t.cos(3 * t.pi / 2).within(0.0)
    t.cos(2 * t.pi).within(1.0)

    t.tan(0.0).within(0.0)
    t.tan(t.pi / 4).within(1.0)
    assert(abs(t.tan(t.pi / 2)) > 1.633e16)
    t.tan(3 * t.pi / 4).within(-1.0)
    t.tan(t.pi).within(0.0)
  }
}
