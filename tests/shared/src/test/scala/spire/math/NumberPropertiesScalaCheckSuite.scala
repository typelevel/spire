package spire
package math

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class NumberPropertiesScalaCheckSuite extends munit.ScalaCheckSuite {
  property("Number.apply(Long)") {
    forAll { (n: Long) => Number(n) === n }
  }

  property("Number.apply(Long) II") {
    forAll { (n: Long) => Number(n) === SafeLong(n) }
  }

  property("Number.apply(Long) III") {
    // we need to do (n - 1).abs to ensure we don't get a negative number
    forAll { (n: Long) => Number((n - 1).abs) === Natural((n - 1).abs) }
  }

  property("Number.apply(BigInt)") {
    forAll { (n: BigInt) => Number(n) === n }
  }

  property("Number.apply(BigInt) II") {
    forAll { (n: BigInt) => Number(n) === SafeLong(n) }
  }

  property("Number.apply(BigInt) III") {
    forAll { (n: BigInt) => Number(n.abs) === Natural(n.abs) }
  }

  property("Number.apply(BigDecimal)") {
    forAll { (n: BigDecimal) => Number(n) === n }
  }

  property("Number.apply(Rational)") {
    forAll { (n: BigInt, d0: BigInt) =>
      val d = if (d0 == 0) BigInt(1) else d0
      val r = Rational(n, d)
      Number(r) === r
    }
  }

  def bothEq[A, B](a: A, b: B) = {
    a == b && b == a
  }

  property("RationalNumber == Int") {
    forAll { (n: Int) => bothEq(Number(Rational(n)), n) }
  }

  property("RationalNumber == Long") {
    forAll { (n: Long) => bothEq(Number(Rational(n)), n) }
  }

  property("RationalNumber == BigInt") {
    forAll { (n: BigInt) => Number(Rational(n)) === n }
  }

  property("Long + Long") {
    forAll { (x: Long, y: Long) =>
      val lx = Number(x)
      val ly = Number(y)
      val lz = lx + ly
      val bx = Number(BigInt(x))
      val by = Number(BigInt(y))

      lz === BigInt(x) + BigInt(y) &&
      bx + by === lz &&
      lx + by === lz &&
      bx + ly === lz
    }
  }
}
