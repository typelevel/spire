package spire.math.fpf

import spire.algebra._
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

  test("FPFilter is Field:NRoot") {
    def powerToTheRoot[A: Field:NRoot](a: A, b: A) {
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
    def wrap(b:BigDecimal)(f:() => Unit) = new FPFilter(MaybeDouble(b), { f(); b })

    // used to track evaluation of FPFilter's lazy value
    var zeroEval = false
    var aEval = false
    var bEval = false

    val zero = wrap(BigDecimal(0))(() => zeroEval = true)
    val a = wrap(BigDecimal(5))(() => aEval = true)
    val b = wrap(BigDecimal(9))(() => bEval = true)

    val sum = a + a
    val prod = a * b
    val quot = a / b

    val s1 = sum == zero
    val s2 = prod == zero
    val s3 = quot == zero

    // make sure we haven't evaluated the exact value yet
    assert(!zeroEval)
    assert(!aEval)
    assert(!bEval)

    // make sure a.value actaully evalutes
    val x = a.value
    assert(aEval)
  }
}

