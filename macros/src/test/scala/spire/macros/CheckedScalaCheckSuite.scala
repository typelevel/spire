package spire
package macros

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class CheckedScalaCheckSuite extends munit.ScalaCheckSuite {
  import Checked.checked
  import Arbitrary.arbitrary

  case class NotZero[A](value: A)
  implicit def arbNotZeroLong: Arbitrary[NotZero[Long]] = Arbitrary(arbitrary[Long].filter(_ != 0L).map(NotZero(_)))
  implicit def arbNotZeroInt: Arbitrary[NotZero[Int]] = Arbitrary(arbitrary[Int].filter(_ != 0L).map(NotZero(_)))

  def checkForLongOverflow(value: BigInt, check: => Long): Unit = {
    if (value.isValidLong) {
      assertEquals(check, value.toLong)
    } else {
      intercept[ArithmeticException] { check }
    }
  }

  def checkForIntOverflow(value: BigInt, check: => Int): Unit = {
    if (value.isValidInt) {
      assertEquals(check, value.toInt)
    } else {
      intercept[ArithmeticException] { check }
    }
  }

  test("Negate of Int.MinValue overflows") {
    val x = Int.MinValue
    intercept[ArithmeticException] { checked(-x) }
  }

  property("Int negate overflow throws arithmetic exception") {
    forAll { (x: Int) =>
      checkForIntOverflow(-BigInt(x), checked(-x))
    }
  }

  property("Int addition overflow throws arithmetic exception") {
    forAll { (x: Int, y: Int) =>
      checkForIntOverflow(BigInt(x) + BigInt(y), checked(x + y))
    }
  }

  property("Int subtraction overflow throws arithmetic exception") {
    forAll { (x: Int, y: Int) =>
      checkForIntOverflow(BigInt(x) - BigInt(y), checked(x - y))
    }
  }

  property("Int multiplication overflow throws arithmetic exception") {
    forAll { (x: Int, y: Int) =>
      checkForIntOverflow(BigInt(x) * BigInt(y), checked(x * y))
    }
  }

  property("Int division overflow throws arithmetic exception") {
    forAll { (x: Int, y: NotZero[Int]) =>
      checkForIntOverflow(BigInt(x) / BigInt(y.value), checked(x / y.value))
    }
  }

  def distSq(x: Long, y: Long): BigInt = BigInt(x) * BigInt(x) + BigInt(y) * BigInt(y)

  property("Int euclidean square distance overflow throws arithmetic exception") {
    forAll { (x: Int, y: Int) =>
      checkForIntOverflow(distSq(x, y), checked(x * x + y * y))
    }
  }

  test("Negate of Byte.MinValue overflows") {
    val x = Byte.MinValue
    assertEquals(-Byte.MinValue, checked(-x))
  }

  case class A(p: Long, r: Long) {
    def plus(a: A): A = Checked.tryOrReturn {
      A(this.p + a.p, this.r + a.r)
    } {
      A(0, 0)
    }
  }
  def compare(p: Long): Int = p.toInt
  test("Negate of Long.MinValue overflows") {
    val x = Long.MinValue
    intercept[ArithmeticException] { checked(-x) }
    assert { Checked.option(-x).isEmpty }
    assertEquals(-1L, Checked.tryOrElse(-x)(-1L))
    assertEquals(-1L, odd(x))
    assertEquals(0L, odd(0))
    assertEquals(Long.MaxValue - 1, add(-1))
    assertEquals(Long.MaxValue, add(0))
    assertEquals(-1L, add(1))
    val a = A(1L, 1L)
    val p = 1L
    val n: Long = 3
    val m: Int = 3
    val i = compare(p * n)
    // Long * Long
    val c1: Long = Checked.tryOrElse {
      val i: Long = compare(p * n)
      i
    } {
      val j = 0L
      j
    }
    assertEquals(3L, c1)
    // Long * Int
    val c2: Long = Checked.tryOrElse {
      val i: Long = compare(p * m)
      i
    } {
      val j = 0L
      j
    }
    assertEquals(3L, c2)
    // Int * Int
    val c3: Int = Checked.tryOrElse {
      val i: Int = compare(m * l)
      i
    } {
      val j = 0
      j
    }
    assertEquals(18, c3)
    // Long * Long
    val c4: Long = Checked.tryOrElse {
      val i: Long = compare(p * n + 1)
      i
    } {
      val j = 0L
      j
    }
    assertEquals(4L, c4)
    // Long * Int
    val c5: Long = Checked.tryOrElse {
      val i: Long = compare(p * m)
      i
    } {
      val j = 0L
      j
    }
    assertEquals(3L, c5)
    val c6: A = { //Checked.tryOrElse {
      val a = A(Long.MaxValue, Long.MaxValue)
      val i = A(1, 5L)
      a.plus(i)
    }
    println(c6)
    // // Int * Int
    // val c3: Int = Checked.tryOrElse {
    //   val i: Int = compare(m * l)
    //   i
    // } {
    //   val j = 0
    //   j
    // }
    // Checked.tryOrElse {
    //   val i = compare(p * n + 1)
    //   i
    // } {
    //   val j = 0L
    //   j
    // }
  }

  // sealed trait Rational
  // case class SafeLong(n: Long)
  //   def compare(r: Rational): Int = r match {
  //     case r: LongRational =>
  //       val n: Int = Checked.tryOrElse {
  //         LongAlgebra.compare(n * r.d, r.n * d)
  //       } {
  //         val dgcd = spire.math.gcd(d, r.d)
  //         val u: Int =
  //           if (dgcd == 1L)
  //             (SafeLong(n) * r.d).compare(SafeLong(r.n) * d)
  //           else
  //             (SafeLong(n) * (r.d / dgcd)).compare(SafeLong(r.n) * (d / dgcd))
  //         u
  //       }
  //       n
  //
  //     case r: BigRational =>
  //       ???
  //     // val dgcd = spire.math.gcd(d, (r.d % d).toLong)
  //     // if (dgcd == 1L)
  //     //   (SafeLong(n) * r.d).compare(r.n * d)
  //     // else
  //     //   (SafeLong(n) * (r.d / dgcd)).compare(r.n * (d / dgcd))
  //   }
  //
  def odd(a: Long): Long =
    Checked.tryOrReturn(-a)(-1L)

  def add(a: Long): Long =
    Checked.tryOrReturn(Long.MaxValue + a)(-1L)
//   property("Long negate overflow throws arithmetic exception") {
//     forAll { (x: Long) =>
//       checkForLongOverflow(-BigInt(x), checked(-x))
//     }
//   }
//
//   property("Long addition overflow throws arithmetic exception") {
//     forAll { (x: Long, y: Long) =>
//       checkForLongOverflow(BigInt(x) + BigInt(y), checked(x + y))
//     }
//   }
//
//   property("Long subtraction overflow throws arithmetic exception") {
//     forAll { (x: Long, y: Long) =>
//       checkForLongOverflow(BigInt(x) - BigInt(y), checked(x - y))
//     }
//   }
//
//   property("Long multiplication overflow throws arithmetic exception") {
//     forAll { (x: Long, y: Long) =>
//       checkForLongOverflow(BigInt(x) * BigInt(y), checked(x * y))
//     }
//   }
//
//   property("Long division overflow throws arithmetic exception") {
//     forAll { (x: Long, y: NotZero[Long]) =>
//       checkForLongOverflow(BigInt(x) / BigInt(y.value), checked(x / y.value))
//     }
//   }
//
//   property("Long euclidean square distance overflow throws arithmetic exception") {
//     forAll { (x: Long, y: Long) =>
//       checkForLongOverflow(distSq(x, y), checked(x * x + y * y))
//     }
//   }
//
//   test("Int upgrades to Long for overflow checks when mixed in binary op") {
//     assertEquals(Checked.option {
//                    val x = 2L
//                    val y = Int.MaxValue
//                    x + y
//                  },
//                  Some(Int.MaxValue.toLong + 2)
//     )
//
//     assertEquals(Checked.option {
//                    val x = 2L
//                    val y = Int.MaxValue
//                    y + x
//                  },
//                  Some(Int.MaxValue.toLong + 2)
//     )
//
//     intercept[ArithmeticException](checked {
//       val x = Long.MaxValue
//       val y = 2
//       x * y
//     })
//
//     intercept[ArithmeticException](checked {
//       val x = Long.MaxValue
//       val y = 2
//       y * x
//     })
//   }
//
//   test("Byte and Short upgrade to Int when mixed") {
//     intercept[ArithmeticException](checked {
//       val x = Int.MaxValue
//       val y = 2: Byte
//       x * y
//     })
//
//     intercept[ArithmeticException](checked {
//       val x = Int.MaxValue
//       val y = 2: Byte
//       y * x
//     })
//
//     intercept[ArithmeticException](checked {
//       val x = Int.MaxValue
//       val y = 2: Short
//       x * y
//     })
//
//     intercept[ArithmeticException](checked {
//       val x = Int.MaxValue
//       val y = 2: Short
//       y * x
//     })
//   }
//
}
