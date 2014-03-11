package spire.math

import spire.implicits._
import scala.util.Try

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class FixedPointCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import ArbitrarySupport._

  implicit val arbFixedScale: Arbitrary[FixedScale] =
    Arbitrary(arbitrary[Int].map(_.abs).filter(_ > 0).map(FixedScale))

  implicit val arbFixedPoint: Arbitrary[FixedPoint] =
    Arbitrary(arbitrary[Long].map(new FixedPoint(_)))

  property("FixedScale(r).toRational ~= r") {
    forAll { (s: FixedScale, r: Rational) =>
      implicit val scale = s
      val minV = FixedPoint.MinValue.toRational
      val maxV = FixedPoint.MaxValue.toRational
      if (r < minV || maxV < r) {
        Try(FixedPoint(r)).isSuccess shouldBe false
      } else {
        FixedPoint(r).toRational shouldBe r.roundTo(s.denom)
      }
    }
  }

  property("new FixedScale(n).toRational = n/d") {
    forAll { (s: FixedScale, n: Long) =>
      implicit val scale = s
      new FixedPoint(n).toRational shouldBe Rational(n, s.denom)
    }
  }

  import BigDecimal.RoundingMode.FLOOR

  def build(x: Long, y0: Long, z: Byte, noZero: Boolean): (Int, Int, FixedPoint, FixedPoint, Rational, Rational) = {
    val y = if (y0 == 0L && noZero) 1L else y0
    val d = z.toInt.abs % 11
    val denom = 10 ** (d)
    val (fx, fy) = (new FixedPoint(x), new FixedPoint(y))
    val (ax, ay) = (Rational(x, denom), Rational(y, denom))
    (d, denom, fx, fy, ax, ay)
  }

  type S2[A] = (A, A, FixedScale) => A
  type F2[A] = (A, A) => A

  import scala.util.{Try, Success}
  def testBinop2(name: String, noZero: Boolean, f: S2[FixedPoint], g: F2[Rational]) =
    property(name) {
      forAll { (x: Long, y: Long, s: FixedScale) =>
        implicit val scale = s
        if (!noZero || y != 0L) {
          val (fx, fy) = (new FixedPoint(x), new FixedPoint(y))
          val (ax, ay) = (Rational(x, s.denom), Rational(y, s.denom))
          val az = g(ax, ay)
          val fz = Try(f(fx, fy, scale)) match {
            case Success(fz) =>
              BigInt(fz.long) shouldBe (az * s.denom).toBigInt
            case _ =>
              (az * s.denom < Long.MinValue || Long.MaxValue < az * s.denom) shouldBe true
          }
        }
      }
    }

  def testBinop(name: String, noZero: Boolean, f: S2[FixedPoint], g: F2[Rational]) =
    property(name) {
      forAll { (x: Long, y: Long, z: Byte) =>
        val (_, denom, fx, fy, ax, ay) = build(x, y, z, noZero)
        val az = g(ax, ay)

        val ofz = try {
          implicit val scale = FixedScale(denom)
          Some(f(fx, fy, scale))
        } catch {
          case _: FixedPointOverflow => None
        }

        ofz match {
          case Some(fz) =>
            BigInt(fz.long) shouldBe (az * denom).toBigInt
          case None =>
            (az * denom < Long.MinValue || Long.MaxValue < az * denom) shouldBe true
        }
      }
    }

  testBinop2("addition", false, (x, y, s) => x + y, _ + _)

  testBinop2("subtraction", false, (x, y, s) => x - y, _ - _)

  testBinop2("multiplication", false, (x, y, s) => (x).*(y)(s), _ * _)

  testBinop2("division", true, (x, y, s) => (x)./(y)(s), _ / _)

  testBinop2("modulus", true, (x, y, s) => x % y, _ % _)

  def buildHalf(x: Long, z: Byte): (Int, Int, FixedPoint, Rational) = {
    val d = z.toInt.abs % 11
    val denom = 10 ** (d)
    val fx = new FixedPoint(x)
    val ax = Rational(x, denom)
    (d, denom, fx, ax)
  }

  type SH2[A] = (A, Long, FixedScale) => A
  type FH2[A] = (A, Long) => A

  def testHalfop(name: String, noZero: Boolean, f: SH2[FixedPoint], g: FH2[Rational]) =
    property(name) {
      forAll { (x: Long, y0: Long, z: Byte) =>
        val y = if (noZero && y0 == 0) 1L else y0
        val (d, denom, fx, ax) = buildHalf(x, z)
        val az = g(ax, y)

        val ofz = try {
          implicit val scale = FixedScale(denom)
          Some(f(fx, y, scale))
        } catch {
          case _: FixedPointOverflow => None
        }

        ofz match {
          case Some(fz) =>
            BigInt(fz.long) shouldBe (az * denom).toBigInt
          case None =>
            (az * denom < Long.MinValue || Long.MaxValue < az * denom) shouldBe true
        }
      }
    }

  testHalfop("h-addition", false, (x, y, s) => (x).+(y)(s), _ + _)

  testHalfop("h-subtraction", false, (x, y, s) => (x).-(y)(s), _ - _)

  testHalfop("h-multiplication", false, (x, y, s) => x * y, _ * _)

  testHalfop("h-division", true, (x, y, s) => x / y, _ / _)

  testHalfop("h-modulus", true, (x, y, s) => (x).%(y)(s), _ % _)

  property("pow") {
    forAll { (x: Long, k0: Byte, d0: Byte) =>
      val k = k0.toInt.abs
      val denom = 10 ** (d0.toInt.abs % 11)

      val az = Rational(x, denom).pow(k)

      val ofz = try {
        implicit val scale = FixedScale(denom)
        Some(new FixedPoint(x).pow(k))
      } catch {
        case _: FixedPointOverflow => None
      }

      ofz match {
        case Some(fz) =>
          BigInt(fz.long) shouldBe (az * denom).toBigInt
        case None =>
          (az * denom < Long.MinValue || Long.MaxValue < az * denom) shouldBe true
      }
    }
  }
}
