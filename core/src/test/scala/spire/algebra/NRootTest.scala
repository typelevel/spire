package spire.algebra

import spire.math._
import org.scalatest.FunSuite

import scala.reflect.ClassTag


class NRootTest extends FunSuite {
  def testIntegralNRoot[A: Numeric: ClassTag] {
    val cls = implicitly[ClassTag[A]].runtimeClass.getSimpleName
    test("Integral NRoot (%s)" format cls) {
      val one = Rig[A].one
      assert(NRoot[A].nroot(Rig[A].one, 2) === Rig[A].one)
      assert(NRoot[A].nroot(Numeric[A].fromInt(1234), 2) === Numeric[A].fromInt(35))
      assert(NRoot[A].nroot(Numeric[A].fromInt(912384), 3) === Numeric[A].fromInt(96))
    }
  }

  testIntegralNRoot[Int]
  testIntegralNRoot[Long]
  testIntegralNRoot[BigInt]

  val DECIMAL1 = new java.math.MathContext(1)

  // Returns the smallest value that can be added/removed from x.
  def eps(x: BigDecimal): BigDecimal =
    x.round(DECIMAL1) * BigDecimal(1, x.mc.getPrecision - 1)

  def checkNRoot(x: BigDecimal, n: Int) {
    import spire.implicits._

    val y = x nroot n
    val e = eps(y)

    if (x > 0) {
      assert(((y - e) ** n) < x, "expected %s ** %d < %s" format (y - e, n, x))
      assert(((y + e) ** n) > x, "expected %s ** %d > %s" format (y + e, n, x))
    } else {
      assert(((y + e) ** n) < x, "expected %s ** %d < %s" format (y + e, n, x))
      assert(((y - e) ** n) > x, "expected %s ** %d > %s" format (y - e, n, x))
    }
  }

  val HighPrecision = new java.math.MathContext(250)

  val bases = Seq(
    BigDecimal(2),
    BigDecimal(3),
    BigDecimal("3492919288716623419872.99818234", HighPrecision),
    BigDecimal("0.00000000000000000000000000000012345")
  )

  val roots = Seq(2, 3, 6, 9, 23, 53)

  test("BigDecimal NRoot") {
    bases foreach { x =>
      roots foreach (checkNRoot(x, _))
    }

    bases map (-_) foreach { x =>
      roots filter (_ % 2 == 1) foreach (checkNRoot(x, _))
    }
  }
}

