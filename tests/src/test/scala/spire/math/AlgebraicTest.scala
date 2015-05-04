package spire.math

import spire.algebra.Sign

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import java.math.{ MathContext, RoundingMode }


class AlgebraicTest extends FunSuite with PropertyChecks {

  test("absolute approximation of addition is correct") {
    val sqrt2x100 = Iterator.fill(100)(Algebraic(2).sqrt) reduce (_ + _)
    val dblSqrt2x100 = math.sqrt(2) * 100

    val err = BigDecimal(0.0001)
    val approx = sqrt2x100.toBigDecimal(4, RoundingMode.HALF_EVEN)

    assert(approx - err <= dblSqrt2x100 && dblSqrt2x100 <= approx + err)
  }

  test("relative approximation of addition is correct") {
    val sum = Iterator.fill(29)(Algebraic(1) / 29) reduce (_ + _)
    assert(sum.toDouble === 1.0)
    assert(sum.toBigDecimal(MathContext.DECIMAL128) === BigDecimal(1))
  }

  test("absolute approximation of subtraction is correct") {
    val negSqrt2x98 = Iterator.fill(100)(Algebraic(2).sqrt) reduce (_ - _)
    val dblNegSqrt2x98 = -math.sqrt(2) * 98

    val err = BigDecimal(0.0001)
    val approx = negSqrt2x98.toBigDecimal(4, RoundingMode.HALF_EVEN)
    assert(approx - err <= dblNegSqrt2x98 && dblNegSqrt2x98 <= approx + err)
  }

  test("absolute approximation of multiplication is correct") {
    val prod = Iterator.fill(32)(Algebraic(2).sqrt) reduce (_ * _)
    val err = BigDecimal(0.0001)

    val approx = prod.toBigDecimal(4, RoundingMode.HALF_EVEN)
    val actual = BigDecimal(1 << 16)

    assert(actual - err <= approx && approx <= actual + err)
  }

  test("relative approximation of multiplication is correct") {
    val prod = Iterator.fill(32)(Algebraic(2).sqrt) reduce (_ * _)

    val approx = prod.toBigDecimal(MathContext.DECIMAL64)
    val actual = BigDecimal(1 << 16)

    assert(approx === actual)
  }

  test("absolute approximation of division is correct") {
    val quot = Algebraic(2).sqrt / 2
    val actual = 0.7071067811865476
    val err = BigDecimal(0.0001)
    val approx = quot.toBigDecimal(4, RoundingMode.HALF_EVEN)
    assert(actual - err <= approx && approx <= actual + err)
  }

  test("relative approximation of division is correct") {
    val quot = Iterator.fill(16)(Algebraic(2)).foldLeft(Algebraic(1 << 16))(_ / _)
    assert(quot.toDouble === 1.0)

    val aThird = Algebraic(-1) / 3
    val actual = BigDecimal(-1, MathContext.DECIMAL128) / 3
    assert(aThird.toBigDecimal(MathContext.DECIMAL128) === actual)

    val aThird2 = Algebraic(1) / -3
    assert(aThird2.toBigDecimal(MathContext.DECIMAL128) === actual)
  }

  test("absolute approximation of roots is correct") {
    val a = Algebraic(2).sqrt
    val err = BigDecimal(0.00001)
    val actual = BigDecimal(1.4142135623730951)
    val approx = a.toBigDecimal(5, RoundingMode.HALF_EVEN)
    assert(actual - err <= approx && approx <= actual + err)

    val b = Algebraic(-4) nroot 3
    val bctual = BigDecimal(-1.5874010519681994) // give or take
    val bpprox = b.toBigDecimal(5, RoundingMode.HALF_EVEN)
    assert(bctual - err <= bpprox && bpprox <= bctual + err)
  }

  test("associativity with large and small numbers") {
    val x = Algebraic(1e308)
    val y = Algebraic(-1e308)
    val z = Algebraic(1)
    assert((x + (y + z)) === (x + y + z))
  }

  // This generates rational Algebraic expressions along with their Rational
  // value, then tests that the 2 are equal.
  test("equality test of rational algebraic is correct") {
    forAll("rational") { (qa: RationalAlgebraic) =>
      val RationalAlgebraic(a, q) = qa
      a == Algebraic(q)
    }
  }

  // This generates a Algebraic expression that we know if rational, along with
  // its actual Rational value. We then verify that the computed square root
  // using Algebraic is actually a good approximation to the real square root.
  test("approximation of sqrt of rational is correct") {
    forAll("rational") { (qa: RationalAlgebraic) =>
      val RationalAlgebraic(a, q) = qa
      val x = a.sqrt.toBigDecimal(MathContext.DECIMAL64)
      val error = x.ulp * 4
      val xSq = x * x
      Rational(xSq - error) < q && Rational(xSq + error) > q
    }
  }

  // This generates a class of sums of square-roots that evaluate to 0. We
  // simply verify that Algebraic agrees they are 0. Many of the terms
  // generated will be irrational.
  test("simple zero sum of sqrt") {
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
  test("sign of tricky zero is Zero") {
    assert(trickyZero.sign === Sign.Zero)
  }

  test("relative approximation of zero is zero") {
    assert((Algebraic(0).toBigDecimal(MathContext.DECIMAL128)) === BigDecimal(0))
    assert(trickyZero.toDouble === 0.0)
  }

  // Generate a bunch of rational roots for a polynomial, then construct a
  // rational polynomial with these roots, and then verify that Algebraic.roots
  // finds all the roots exactly.
  test("find all rational roots of rational polynomial") {
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

  // This was a failing test case found using the property tests above.
  test("root isolation uses inverse transform to map upper-bound") {
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
  test("divide by zero bug on near-zero root refinement") {
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
  test("roots of polynomial evaluate to 0") {
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
      if (depth < MaxDepth) {
        Gen.frequency(
          1 -> genAdd(depth + 1),
          1 -> genSub(depth + 1),
          1 -> genMul(depth + 1),
          1 -> genDiv(depth + 1),
          1 -> genNeg(depth + 1),
          1 -> genPow(depth + 1, arbitrary[Byte].map(_.toInt % 7)),
          7 -> genLeaf
        )
      } else {
        genLeaf
      }

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
