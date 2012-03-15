package spire.math.fpf

import spire.math._
import Implicits._

import org.scalatest.FunSuite


class FPFilterTest extends FunSuite {
  test("FPFilter is a Ring") {
    def someRingStuff[A: Ring](a: A, b: A, sum: A, diff: A, prod: A) {
      assert(a + b == sum)
      assert(a - b == diff)
      assert(a * b == prod)
    }

    someRingStuff(FPFilter(BigDecimal("1.234")),
                  FPFilter(BigDecimal("0.999")),
                  FPFilter(BigDecimal("2.233")),
                  FPFilter(BigDecimal("0.235")),
                  FPFilter(BigDecimal("1.232766")))
  }

  test("FPFilter is a EuclideanRing") {
    def quotIt[A: EuclideanRing](a: A, b: A, quot: A, mod: A) {
      assert(a /~ b == quot)
      assert(a % b == mod)
    }

    quotIt(FPFilter(BigInt(7)),
           FPFilter(BigInt(5)),
           FPFilter(BigInt(1)),
           FPFilter(BigInt(2)))

    quotIt(FPFilter(BigDecimal(-3.7)),
           FPFilter(BigDecimal(1.2)),
           FPFilter(BigDecimal(-3)),
           FPFilter(BigDecimal(-0.1)))
  }

  test("FPFilter is a Field") {
    def divAndStuff[A: Field](a: A, b: A, c: A) {
      assert(a / b == c)
    }

    divAndStuff(FPFilter(Rational(5)),
                FPFilter(Rational(7)),
                FPFilter(Rational(5, 7)))
  }

  test("FPFilter is FieldWithNRoot") {
    def powerToTheRoot[A: FieldWithNRoot](a: A, b: A) {
      assert(a.sqrt == b)
      assert((a pow 2) == (b pow 4))
    }

    powerToTheRoot(FPFilter(Real(2)),
                   FPFilter(Real(2).sqrt))
  }

  // TODO: Really sketchy.
  def isEvaluated[A](x: FPFilter[A]): Boolean = {
    classOf[FPFilter[_]].getField("bitmap$0").get(x) != 0
  }
 
  test("Non-zero sign doesn't (always) evaluate value") {
    val zero = FPFilter(BigDecimal(0))
    val a = FPFilter(BigDecimal(5))
    val b = FPFilter(BigDecimal(9))

    val sum = a + a
    val prod = a * b
    val quot = a / b

    val s1 = sum == zero
    val s2 = prod == zero
    val s3 = quot == zero

    assert(!isEvaluated(sum))
    assert(!isEvaluated(prod))
    assert(!isEvaluated(quot))
  }
}

