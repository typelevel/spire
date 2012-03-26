package spire.math

import org.scalatest.FunSuite
import spire.math.fun._
import Implicits.{eqOps => _, _}

object Const {
  val pos = 1
  val zero = 0
  val neg = -1
}

class SBigIntTest extends FunSuite {
  import Const._
  
  test("BigInt(0) same as BigInt.Zero") {
    assert(SBigInt(0) eq SBigInt.Zero)
  }
  
  test("BigInt(0) equal to BigInt.Zero") {
    assert(SBigInt(0) === SBigInt.Zero)
  }
  
  ignore("BigInt(1) equal to BigInt.One") {
    assert(SBigInt(1) === SBigInt.One)
  }
  
//  forAll { (n: Int, d: Int) =>
//
//  whenever (d != 0 && d != Integer.MIN_VALUE
//      && n != Integer.MIN_VALUE) {
//
//    val f = new Fraction(n, d)
//
//    if (n < 0 && d < 0 || n > 0 && d > 0)
//      f.numer should be > 0
//    else if (n != 0)
//      f.numer should be < 0
//    else
//      f.numer should be === 0
//
//    f.denom should be > 0
//  }
//}
  
  test("create BigInt(42)") {
    val bi = SBigInt(42)
    assert(bi.signum === pos)
    assert(bi.arr === Array(42))
  }

}