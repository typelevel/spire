/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package macros

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

case class A(p: Long, r: Long)

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

  test("Negate of Long.MinValue overflows") {
    val x = Long.MinValue
    intercept[ArithmeticException] { checked(-x) }
  }

  test("Option") {
    val x = Long.MinValue
    assert { Checked.option(-x).isEmpty }
  }

  def compare(p: Long): Int = p.toInt
  test("tryOrElse") {
    val x = Long.MinValue
    assertEquals(-1L, Checked.tryOrElse(-x)(-1L))
    val p = 1L
    val n: Long = 3
    val m: Int = 3
    val l = 6
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
    val ag = A(Long.MaxValue, Long.MaxValue)
    intercept[ArithmeticException] { checked(ag.p * 2L) }
    // Border case failing in earlier versions of the scala 3 macro
    intercept[ArithmeticException] {
      checked(List(1L, 2L).map { k =>
        ag.p * k
      })
    }
  }

  property("Long negate overflow throws arithmetic exception") {
    forAll { (x: Long) =>
      checkForLongOverflow(-BigInt(x), checked(-x))
    }
  }

  property("Long addition overflow throws arithmetic exception") {
    forAll { (x: Long, y: Long) =>
      checkForLongOverflow(BigInt(x) + BigInt(y), checked(x + y))
    }
  }

  property("Long subtraction overflow throws arithmetic exception") {
    forAll { (x: Long, y: Long) =>
      checkForLongOverflow(BigInt(x) - BigInt(y), checked(x - y))
    }
  }

  property("Long multiplication overflow throws arithmetic exception") {
    forAll { (x: Long, y: Long) =>
      checkForLongOverflow(BigInt(x) * BigInt(y), checked(x * y))
    }
  }

  property("Long division overflow throws arithmetic exception") {
    forAll { (x: Long, y: NotZero[Long]) =>
      checkForLongOverflow(BigInt(x) / BigInt(y.value), checked(x / y.value))
    }
  }

  property("Long euclidean square distance overflow throws arithmetic exception") {
    forAll { (x: Long, y: Long) =>
      checkForLongOverflow(distSq(x, y), checked(x * x + y * y))
    }
  }

  test("Int upgrades to Long for overflow checks when mixed in binary op") {
    assertEquals(Checked.option {
                   val x = 2L
                   val y = Int.MaxValue
                   x + y
                 },
                 Some(Int.MaxValue.toLong + 2)
    )

    assertEquals(Checked.option {
                   val x = 2L
                   val y = Int.MaxValue
                   y + x
                 },
                 Some(Int.MaxValue.toLong + 2)
    )

    intercept[ArithmeticException](checked {
      val x = Long.MaxValue
      val y = 2
      x * y
    })

    intercept[ArithmeticException](checked {
      val x = Long.MaxValue
      val y = 2
      y * x
    })
  }

  test("Byte and Short upgrade to Int when mixed") {
    intercept[ArithmeticException](checked {
      val x = Int.MaxValue
      val y = 2: Byte
      x * y
    })

    intercept[ArithmeticException](checked {
      val x = Int.MaxValue
      val y = 2: Byte
      y * x
    })

    intercept[ArithmeticException](checked {
      val x = Int.MaxValue
      val y = 2: Short
      x * y
    })

    intercept[ArithmeticException](checked {
      val x = Int.MaxValue
      val y = 2: Short
      y * x
    })
  }

}
