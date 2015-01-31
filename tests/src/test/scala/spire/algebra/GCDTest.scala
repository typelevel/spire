package spire.algebra

import spire.math.{ Rational, NumberTag }
import spire.std.int._
import spire.std.long._
import spire.std.float._
import spire.std.double._
import spire.syntax.euclideanRing._
import spire.syntax.isReal._

import scala.reflect.ClassTag

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class GCDTest extends FunSuite with Checkers {
  implicit def ArbBigDecimal: Arbitrary[BigDecimal] = Arbitrary(for {
    value <- arbitrary[Long]
    scale <- arbitrary[Short]
  } yield BigDecimal(value, scale.toInt))

  implicit def ArbRational: Arbitrary[Rational] = Arbitrary(for {
    n <- arbitrary[Long]
    d <- arbitrary[Long] if d != 0
  } yield Rational(n, d))

  def testGcd[A: EuclideanRing: IsReal: NumberTag](x: A, y: A): Boolean = {
    (x == Ring[A].zero || y == Ring[A].zero) || {
      val den = spire.math.gcd(x, y)
      val x0 = x /~ den
      val y0 = y /~ den
      if (NumberTag[A].isFinite(x0) && NumberTag[A].isFinite(y0)) {
        x0.isWhole && y0.isWhole && (spire.math.gcd(x0, y0) == Ring[A].one)
      } else {
        // Ideally we'd filter this out at the ScalaCheck level.
        true
      }
    }
  }

  test("Int GCD")(check(forAll { (a: Int, b: Int) => testGcd(a, b) }))
  test("Long GCD")(check(forAll { (a: Long, b: Long) => testGcd(a, b) }))
  test("Float GCD")(check(forAll { (a: Float, b: Float) => testGcd(a, b) }))
  test("Double GCD")(check(forAll { (a: Double, b: Double) => testGcd(a, b) }))
  // Disabled. Getting unexplainable OOM errors, even with isWhole commented out.
  // test("BigDecimal GCD")(check(forAll { (a: BigDecimal, b: BigDecimal) => testGcd(a, b) }))
  test("Rational GCD")(check(forAll { (a: Rational, b: Rational) => testGcd(a, b) }))
}
