package spire.math.fpf

import spire.algebra._
import spire.math._
import spire.implicits._

import org.scalatest.FunSuite


class FPFilterTest extends FunSuite {
  final class Evaluated extends Exception
  private def evaluated = throw new Evaluated

  sealed trait Bad
  implicit object BadField extends Field[Bad] with IsReal[Bad] with NRoot[Bad] {
    def zero: Bad = evaluated
    def one: Bad = evaluated
    def negate(a:Bad): Bad = evaluated
    def plus(a:Bad, b:Bad): Bad = evaluated
    def quot(a:Bad, b:Bad) = evaluated
    def mod(a:Bad, b:Bad) = evaluated
    def gcd(a:Bad, b:Bad):Bad = evaluated
    override def fromDouble(n: Double): Bad = evaluated
    def times(x:Bad, b:Bad): Bad = evaluated
    def div(a:Bad, b:Bad): Bad = evaluated
    def nroot(a: Bad, k: Int): Bad = evaluated
    def fpow(a: Bad, b: Bad) = evaluated
    def compare(x: Bad, y: Bad) = evaluated
    def signum(a: Bad): Int = evaluated
    def abs(a: Bad): Bad = evaluated
    def toDouble(x: Bad): Double = evaluated
    def ceil(a:Bad): Bad = evaluated
    def floor(a:Bad): Bad = evaluated
    def round(a:Bad): Bad = evaluated
    def isWhole(a:Bad): Boolean = evaluated
  }

  test("FpFilter doesn't evaluated for easy problems") {
    val x = FpFilter.exact[Bad](1D)
    val y = FpFilter.exact[Bad](1.2D)
    assert((x + y).signum == 1)
    assert((x - y).signum == -1)
    assert((x * y).signum == 1)
    assert((x / y).signum == 1)
    assert(y.sqrt.signum == 1)
  }

  test("Find tricky zero") {
    val x = FpFilter.exact[Algebraic](18)
    val y = FpFilter.exact[Algebraic](8)
    val z = FpFilter.exact[Algebraic](2)
    assert((x.sqrt - y.sqrt - z.sqrt).signum == 0)
  }

  test("Comparisons") {
    val x = FpFilter.exact[Algebraic](-2)
    val y = FpFilter.exact[Algebraic](8)
    assert(x < y)
    assert(y > x)
    assert(x <= y)
    assert(x <= x)
    assert(y >= x)
    assert(y >= y)
    assert(x === x)
  }

  test("Mix-match macro and non-macro") {
    val x = FpFilter.exact[Algebraic](18)
    val y = FpFilter.exact[Algebraic](8)
    val z = FpFilter.exact[Algebraic](2)
    val u = x.sqrt - y.sqrt
    val v = u - z.sqrt
    assert(v.signum == 0)
  }
}
