package spire.macros

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.scalacheck.{Arbitrary, Gen}

class CheckedTest extends FunSuite with GeneratorDrivenPropertyChecks with ShouldMatchers {
  import Checked.checked
  import Arbitrary.arbitrary

  case class NotZero[A](value: A)
  implicit def arbNotZeroLong = Arbitrary(arbitrary[Long] filter (_ != 0L) map (NotZero(_)))
  implicit def arbNotZeroInt = Arbitrary(arbitrary[Int] filter (_ != 0L) map (NotZero(_)))

  def checkForLongOverflow(value: BigInt, check: => Long) = {
    if (value.isValidLong) {
      check should equal (value.toLong)
    } else {
      evaluating { check } should produce[ArithmeticException]
    }
  }

  def checkForIntOverflow(value: BigInt, check: => Int) = {
    if (value.isValidInt) {
      check should equal (value.toInt)
    } else {
      evaluating { check } should produce[ArithmeticException]
    }
  }

  test("Negate of Int.MinValue overflows") {
    val x = Int.MinValue
    evaluating { checked(-x) } should produce[ArithmeticException]
  }

  test("Int negate overflow throws arithmetic exception") {
    forAll("x") { (x: Int) =>
      checkForIntOverflow(-BigInt(x), checked(-x))
    }
  }

  test("Int addition overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Int, y: Int) =>
      checkForIntOverflow(BigInt(x) + BigInt(y), checked(x + y))
    }
  }

  test("Int subtraction overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Int, y: Int) =>
      checkForIntOverflow(BigInt(x) - BigInt(y), checked(x - y))
    }
  }

  test("Int multiplication overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Int, y: Int) =>
      checkForIntOverflow(BigInt(x) * BigInt(y), checked(x * y))
    }
  }

  test("Int division overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Int, y: NotZero[Int]) =>
      checkForIntOverflow(BigInt(x) / BigInt(y.value), checked(x / y.value))
    }
  }

  def distSq(x: Long, y: Long): BigInt = BigInt(x) * BigInt(x) + BigInt(y) * BigInt(y)

  test("Int euclidean square distance overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Int, y: Int) =>
      checkForIntOverflow(distSq(x, y), checked(x * x + y * y))
    }
  }

  test("Negate of Long.MinValue overflows") {
    val x = Long.MinValue
    evaluating { checked(-x) } should produce[ArithmeticException]
  }

  test("Long negate overflow throws arithmetic exception") {
    forAll("x") { (x: Long) =>
      checkForLongOverflow(-BigInt(x), checked(-x))
    }
  }

  test("Long addition overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Long, y: Long) =>
      checkForLongOverflow(BigInt(x) + BigInt(y), checked(x + y))
    }
  }

  test("Long subtraction overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Long, y: Long) =>
      checkForLongOverflow(BigInt(x) - BigInt(y), checked(x - y))
    }
  }

  test("Long multiplication overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Long, y: Long) =>
      checkForLongOverflow(BigInt(x) * BigInt(y), checked(x * y))
    }
  }

  test("Long division overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Long, y: NotZero[Long]) =>
      checkForLongOverflow(BigInt(x) / BigInt(y.value), checked(x / y.value))
    }
  }

  test("Long euclidean square distance overflow throws arithmetic exception") {
    forAll("x", "y") { (x: Long, y: Long) =>
      checkForLongOverflow(distSq(x, y), checked(x * x + y * y))
    }
  }

  test("Int upgrades to Long for overflow checks when mixed in binary op") {
    Checked.option {
      val x = 2L
      val y = Int.MaxValue
      x + y
    } should equal(Some(Int.MaxValue.toLong + 2))

    Checked.option {
      val x = 2L
      val y = Int.MaxValue
      y + x
    } should equal(Some(Int.MaxValue.toLong + 2))

    evaluating(checked {
      val x = Long.MaxValue
      val y = 2
      x * y
    }) should produce[ArithmeticException]

    evaluating(checked {
      val x = Long.MaxValue
      val y = 2
      y * x
    }) should produce[ArithmeticException]
  }

  test("Byte and Short upgrade to Int when mixed") {
    evaluating(checked {
      val x = Int.MaxValue
      val y = (2: Byte)
      x * y
    }) should produce[ArithmeticException]

    evaluating(checked {
      val x = Int.MaxValue
      val y = (2: Byte)
      y * x
    }) should produce[ArithmeticException]

    evaluating(checked {
      val x = Int.MaxValue
      val y = (2: Short)
      x * y
    }) should produce[ArithmeticException]

    evaluating(checked {
      val x = Int.MaxValue
      val y = (2: Short)
      y * x
    }) should produce[ArithmeticException]
  }
}
