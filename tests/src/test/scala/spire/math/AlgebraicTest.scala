package spire.math

import spire.algebra.Sign
import spire.tests.SpireProperties

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary.arbitrary

import java.math.{ MathContext, RoundingMode }
import MathContext.{DECIMAL64, DECIMAL128}

class AlgebraicTest extends SpireProperties {

  def approximation(approx0: Algebraic, scale: Int, actual: BigDecimal): Unit = {
    val error = BigDecimal(10).pow(-scale)
    val approx = approx0.toBigDecimal(scale, RoundingMode.HALF_EVEN)
    (approx - error) should be <= actual
    actual should be <= (approx + error)
  }

  property("absolute approximation of addition is correct") {
    val sqrt2x100 = Iterator.fill(100)(Algebraic(2).sqrt).reduce(_ + _)
    val dblSqrt2x100 = math.sqrt(2) * 100
    approximation(sqrt2x100, 4, BigDecimal(dblSqrt2x100))
  }

  property("relative approximation of addition is correct") {
    val sum = Iterator.fill(29)(Algebraic(1) / 29) reduce (_ + _)
    sum.toDouble shouldBe 1.0
    sum.toBigDecimal(DECIMAL128) shouldBe BigDecimal(1)
  }

  property("absolute approximation of subtraction is correct") {
    val negSqrt2x98 = Iterator.fill(100)(Algebraic(2).sqrt) reduce (_ - _)
    val dblNegSqrt2x98 = -math.sqrt(2) * 98
    approximation(negSqrt2x98, 4, BigDecimal(dblNegSqrt2x98))
  }

  property("absolute approximation of multiplication is correct") {
    val p = Iterator.fill(32)(Algebraic(2).sqrt).reduce(_ * _)
    approximation(p, 4, BigDecimal(1 << 16))
  }

  property("relative approximation of multiplication is correct") {
    val p = Iterator.fill(32)(Algebraic(2).sqrt).reduce(_ * _)
    p.toBigDecimal(DECIMAL64) shouldBe BigDecimal(1 << 16)
  }

  property("absolute approximation of division is correct") {
    val q = Algebraic(2).sqrt / 2
    approximation(q, 4, BigDecimal(0.7071067811865476))
  }

  property("relative approximation of division is correct") {
    val q = Iterator.fill(16)(Algebraic(2)).foldLeft(Algebraic(1 << 16))(_ / _)
    q.toDouble shouldBe 1.0

    val oneThird = BigDecimal(-1, DECIMAL128) / 3
    (Algebraic(-1) / 3).toBigDecimal(DECIMAL128) shouldBe oneThird
    (Algebraic(1) / -3).toBigDecimal(DECIMAL128) shouldBe oneThird
  }

  property("absolute approximation of roots is correct") {
    approximation(Algebraic(2).sqrt, 5, BigDecimal(1.4142135623730951))

    // give or take
    approximation(Algebraic(-4) nroot 3, 5, BigDecimal(-1.5874010519681994))
  }

  property("associativity with large and small numbers") {
    val x = Algebraic(1e308)
    val y = Algebraic(-1e308)
    val z = Algebraic(1)
    ((x + (y + z)) shouldBe (x + y + z))
  }

  // This generates rational Algebraic expressions along with their Rational
  // value, then tests that the 2 are equal.
  property("equality test of rational algebraic is correct") {
    forAll("rational") { (qa: RationalAlgebraic) =>
      val RationalAlgebraic(a, q) = qa
      a == Algebraic(q)
    }
  }

  // This generates a Algebraic expression that we know if rational, along with
  // its actual Rational value. We then verify that the computed square root
  // using Algebraic is actually a good approximation to the real square root.
  property("approximation of sqrt of rational is correct") {
    forAll("rational") { (qa: RationalAlgebraic) =>
      val RationalAlgebraic(a, q) = qa
      val x = a.sqrt.toBigDecimal(DECIMAL64)
      val error = x.ulp * 4
      val xSq = x * x
      Rational(xSq - error) < q && Rational(xSq + error) > q
    }
  }

  // This generates a class of sums of square-roots that evaluate to 0. We
  // simply verify that Algebraic agrees they are 0. Many of the terms
  // generated will be irrational.
  property("simple zero sum of sqrt") {
    forAll("rational") { (z0: RationalAlgebraic) =>
      val RationalAlgebraic(a, z) = z0
      val y = z.pow(3)
      val x = y + 2 * z.pow(2) + z
      val zero = Algebraic(x).sqrt - Algebraic(y).sqrt - a.sqrt
      zero.isZero
    }
  }

  def trickyZero = Algebraic(18).sqrt - Algebraic(8).sqrt - Algebraic(2).sqrt

  // This is just a simpler special case of the property test above.
  property("sign of tricky zero is Zero") {
    (trickyZero.sign shouldBe Sign.Zero)
  }

  property("relative approximation of zero is zero") {
    ((Algebraic(0).toBigDecimal(DECIMAL128)) shouldBe BigDecimal(0))
    (trickyZero.toDouble shouldBe 0.0)
  }

/*
  // this causes frequent test failures. Todo: fix this
  // Generate a bunch of rational roots for a polynomial, then construct a
  // rational polynomial with these roots, and then verify that Algebraic.roots
  // finds all the roots exactly.
  property("find all rational roots of rational polynomial") {
    import spire.implicits._
    val genRootSelection: Gen[(List[Rational], Int)] = for {
      roots <- Gen.nonEmptyListOf(genRational)
      i <- Gen.choose(0, roots.size)
    } yield (roots, i)

    // These tests can be a bit slow, so we bump down the # and size.
    forAll(Gen.nonEmptyListOf(genRational), minSuccessful(20), maxSize(8)) { roots =>
      val poly = roots.map(x => Polynomial.linear(Rational.one, -x)).qproduct
      val algebraicRoots = Algebraic.roots(poly)
      (roots.sorted zip algebraicRoots).forall { case (qRoot, aRoot) =>
        aRoot == Algebraic(qRoot)
      }
    }
  }
  */

  // This was a failing test case found using the property tests above.
  property("root isolation uses inverse transform to map upper-bound") {
    import spire.implicits._
    val roots = List(
      Rational("16279/50267"),
      Rational("223/175")
    )
    val poly = roots.map(x => Polynomial.linear(Rational.one, -x)).qproduct
    val algebraicRoots = Algebraic.roots(poly)
    (roots.sorted zip algebraicRoots).forall { case (qRoot, aRoot) =>
      aRoot == Algebraic(qRoot)
    }
  }

  // This was a failing test case found using the property tests above.
  property("divide by zero bug on near-zero root refinement") {
    import spire.implicits._
    // A failing special case of "algebraic root is zero", where the root is
    // closer to 0 then the approximation required to test.
    val roots = List(
      Rational("8791167214431305472/8377325351665"),
      Rational("12785/4238682313717812603653317580032"),
      Rational("0")
    )
    val poly = roots.map(x => Polynomial.linear(Rational.one, -x)).qproduct
    val algebraicRoots = Algebraic.roots(poly)
    (roots.sorted zip algebraicRoots).forall { case (qRoot, aRoot) =>
      aRoot == Algebraic(qRoot)
    }
  }

  // Generate random rational polynomials, find the exact roots using
  // Algebraic.roots, then test that those are actually roots be evaluating
  // the polynomial and checking if the result isZero.
  property("roots of polynomial evaluate to 0") {
    // This test can be a bit slow, so we limit the tests here. If making major
    // changes to Algebraic, root isolation, refinement, etc, it is a good idea
    // to drop the limits and just give it a bit of time to run.
    forAll(genRationalPoly, minSuccessful(20), maxSize(6)) { poly =>
      val apoly = poly.map(Algebraic(_))
      Algebraic.roots(poly).forall { root =>
        apoly(root).isZero
      }
    }
  }

  def genBigInt: Gen[BigInt] = for {
    bytes <- Gen.listOf(arbitrary[Byte])
    signum <- arbitrary[Boolean].map(n => if (n) -1 else 1)
  } yield BigInt(signum, if (bytes.isEmpty) Array(0: Byte) else bytes.toArray)

  def genRational: Gen[Rational] = for {
    n <- genBigInt
    d <- genBigInt
    if (d.signum != 0)
  } yield Rational(n, d)

  def genRationalPoly: Gen[Polynomial[Rational]] = for {
    coeffs <- Gen.listOf(genRational)
  } yield Polynomial.dense(coeffs.toArray)

  /**
   * An algebraic expression + the exact rational value of this expression.
   */
  case class RationalAlgebraic(
    algebraic: Algebraic,
    rational: Rational
  )

  object RationalAlgebraic {
    implicit val ArbitraryRationalAlgebraic: Arbitrary[RationalAlgebraic] =
      Arbitrary(genRationalAlgebraic(1))

    val MaxDepth = 3

    def genRationalAlgebraic(depth: Int): Gen[RationalAlgebraic] =
      if (depth >= MaxDepth) genLeaf
      else Gen.frequency(
        (1, genAdd(depth + 1)),
        (1, genSub(depth + 1)),
        (1, genMul(depth + 1)),
        (1, genDiv(depth + 1)),
        (1, genNeg(depth + 1)),
        (1, genPow(depth + 1, arbitrary[Byte].map(_.toInt % 7))),
        (7, genLeaf))

    def genLong: Gen[RationalAlgebraic] = for {
      n <- arbitrary[Long]
    } yield RationalAlgebraic(Algebraic(n), Rational(n))

    def genBigDecimal: Gen[RationalAlgebraic] = for {
      unscaledValue <- genBigInt
      scale <- arbitrary[Byte]
      x = BigDecimal(unscaledValue, scale)
    } yield RationalAlgebraic(Algebraic(x), Rational(x))

    def genDouble: Gen[RationalAlgebraic] = for {
      x <- arbitrary[Double]
    } yield RationalAlgebraic(Algebraic(x), Rational(x))

    def genLeaf: Gen[RationalAlgebraic] = Gen.oneOf(
      genRational.map { q => RationalAlgebraic(Algebraic(q), q) },
      genBigDecimal,
      genDouble,
      genLong
    )

    def genAdd(depth: Int): Gen[RationalAlgebraic] = for {
      RationalAlgebraic(lhsA, lhsQ) <- genRationalAlgebraic(depth + 1)
      RationalAlgebraic(rhsA, rhsQ) <- genRationalAlgebraic(depth + 1)
    } yield RationalAlgebraic(lhsA + rhsA, lhsQ + rhsQ)

    def genSub(depth: Int): Gen[RationalAlgebraic] = for {
      RationalAlgebraic(lhsA, lhsQ) <- genRationalAlgebraic(depth + 1)
      RationalAlgebraic(rhsA, rhsQ) <- genRationalAlgebraic(depth + 1)
    } yield RationalAlgebraic(lhsA - rhsA, lhsQ - rhsQ)

    def genMul(depth: Int): Gen[RationalAlgebraic] = for {
      RationalAlgebraic(lhsA, lhsQ) <- genRationalAlgebraic(depth + 1)
      RationalAlgebraic(rhsA, rhsQ) <- genRationalAlgebraic(depth + 1)
    } yield RationalAlgebraic(lhsA * rhsA, lhsQ * rhsQ)

    def genDiv(depth: Int): Gen[RationalAlgebraic] = for {
      RationalAlgebraic(lhsA, lhsQ) <- genRationalAlgebraic(depth + 1)
      RationalAlgebraic(rhsA, rhsQ) <- genRationalAlgebraic(depth + 1)
      if (rhsQ.signum != 0)
    } yield RationalAlgebraic(lhsA / rhsA, lhsQ / rhsQ)

    def genNeg(depth: Int): Gen[RationalAlgebraic] = for {
      RationalAlgebraic(subA, subQ) <- genRationalAlgebraic(depth + 1)
    } yield RationalAlgebraic(-subA, -subQ)

    def genPow(depth: Int, genExp: Gen[Int]): Gen[RationalAlgebraic] = for {
      RationalAlgebraic(subA, subQ) <- genRationalAlgebraic(depth + 1)
      exp <- genExp
      if subQ.signum != 0 || exp > 0
    } yield RationalAlgebraic(subA.pow(exp), subQ.pow(exp))
  }
}
