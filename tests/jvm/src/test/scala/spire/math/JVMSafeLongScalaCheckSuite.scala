package spire
package math

import org.scalacheck.Prop._

class JVMSafeLongScalaCheckSuite extends munit.ScalaCheckSuite {
  def invariant(z: SafeLong): SafeLong = {
    z match {
      case SafeLongLong(_)       => ()
      case SafeLongBigInteger(n) => assertEquals(BigInt(n).isValidLong, false)
    }
    z
  }

  property("x & y") {
    forAll { (x: BigInt, y: BigInt) =>
      assertEquals(invariant(SafeLong(x) & SafeLong(y)), SafeLong(x & y))
      assertEquals(invariant(SafeLong(x) & y), SafeLong(x & y))
      assertEquals(invariant(SafeLong(x) & y.toLong), SafeLong(x & y.toLong))
    }
  }

  property("x | y") {
    forAll { (x: BigInt, y: BigInt) =>
      assertEquals(invariant(SafeLong(x) | SafeLong(y)), SafeLong(x | y))
      assertEquals(invariant(SafeLong(x) & y), SafeLong(x & y))
      assertEquals(invariant(SafeLong(x) & y.toLong), SafeLong(x & y.toLong))
    }
  }

  property("x ^ y") {
    forAll { (x: BigInt, y: BigInt) =>
      assertEquals(invariant(SafeLong(x) ^ SafeLong(y)), SafeLong(x ^ y))
    }
  }

}
