package spire
package math

import spire.algebra._
import spire.math.poly._
import spire.std.bigDecimal._
import spire.std.bigInt._
import spire.syntax.literals._
import spire.optional.rationalTrig._

import org.scalatest.Matchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

object PolynomialSetup {
  implicit val arbitraryRational = Arbitrary(for {
    n0 <- arbitrary[Long]
    d0 <- arbitrary[Long]
  } yield {
    val (n, d) = (n0 % 100, d0 % 100)
    if (d == 0L) Rational(n, 1L) else Rational(n, d)
  })

  // default scalacheck bigdecimals are weird
  implicit val arbitraryBigDecimal = Arbitrary(for {
    r <- arbitrary[Int]
  } yield {
    BigDecimal(r)
  })

  implicit def arbitraryComplex[A: Arbitrary: Fractional: Trig] = Arbitrary(for {
    re <- arbitrary[A]
    im <- arbitrary[A]
  } yield {
    Complex(re, im)
  })

  implicit def arbitraryTerm[A: Arbitrary: Ring: Eq: ClassTag] = Arbitrary(for {
    c <- arbitrary[A]
    e0 <- arbitrary[Int]
  } yield {
    Term(c, (e0 % 100).abs)
  })
}


class PolynomialCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import PolynomialSetup._

  val ebd = Eq[BigDecimal]
  val fbd = Field[BigDecimal]
  val cbd = implicitly[ClassTag[BigDecimal]]

  runDense[Rational]("rational")
  runSparse[Rational]("rational")
  runDense[Complex[Rational]]("complex")
  runSparse[Complex[Rational]]("complex")
  // runDense[BigDecimal]("decimal")(arbitraryBigDecimal, sbd, fbd, cbd)
  // runSparse[BigDecimal]("decimal")(arbitraryBigDecimal, sbd, fbd, cbd)

  def runDense[A: Arbitrary: Eq: Field: ClassTag](typ: String): Unit = {
    implicit val arb: Arbitrary[Polynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Term[A]]]
    } yield {
      Polynomial(ts).toDense
    })
    runTest[A](s"$typ/dense")
  }

  def runSparse[A: Arbitrary: Eq: Field: ClassTag](typ: String): Unit = {
    implicit val arb: Arbitrary[Polynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Term[A]]]
    } yield {
      Polynomial(ts).toSparse
    })
    runTest[A](s"$typ/sparse")
  }

  def runTest[A: Eq: Field: ClassTag](name: String)(implicit arb: Arbitrary[Polynomial[A]], arb2: Arbitrary[A]): Unit = {
    type P = Polynomial[A]

    val zero = Polynomial.zero[A]
    val one = Polynomial.one[A]

    property(s"$name p = p") {
      forAll { (p: P) => p shouldBe p }
    }

    property(s"$name p + 0 = p") {
      forAll { (p: P) => p + zero shouldBe p }
    }

    property(s"$name p + (-p) = 0") {
      forAll { (p: P) => p + (-p) shouldBe zero }
    }

    property(s"$name p * 0 = 0") {
      forAll { (p: P) => p * zero shouldBe zero }
    }

    property(s"$name p * 1 = p") {
      forAll { (p: P) => p * one shouldBe p }
    }

    property(s"$name p /~ 1 = p") {
      forAll { (p: P) => p /~ one shouldBe p }
    }

    property(s"$name p /~ p = 1") {
      forAll { (p: P) => if (!p.isZero) p /~ p shouldBe one }
    }

    property(s"$name p % p = 0") {
      forAll { (p: P) => if (!p.isZero) p % p shouldBe zero }
    }

    property(s"$name x + y = y + x") {
      forAll { (x: P, y: P) => x + y shouldBe y + x }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: P, y: P) => x * y shouldBe y * x }
    }

    property(s"$name (x /~ y) * y + (x % y) = x") {
      forAll { (x: P, y: P) => if (!y.isZero) (x /~ y) * y + (x % y) shouldBe x }
    }

    property(s"$name p = p.reductum + p.maxTerm") {
      forAll { (p: P) =>
        p shouldBe p.reductum + Polynomial(p.maxTerm :: Nil)
      }
    }
  }

  property("(x compose y)(z) == x(y(z))") {
    forAll { (rs1: List[Rational], rs2: List[Rational], r: Rational) =>
      def xyz(rs: List[Rational]): Polynomial[Rational] =
        Polynomial(rs.take(4).zipWithIndex.map { case (c, e) => Term(c, e) })

      val (p1, p2) = (xyz(rs1), xyz(rs2))
      val p3 = p1 compose p2
      p3(r) shouldBe p1(p2(r))
    }
  }

  implicit val arbPolynomial: Arbitrary[Polynomial[BigInt]] = Arbitrary(for {
    ts <- arbitrary[List[Term[BigInt]]]
    isDense <- arbitrary[Boolean]
  } yield {
    val p = Polynomial(ts)
    if (isDense) p.toDense else p.toSparse
  })

  implicit val arbDense: Arbitrary[PolyDense[Rational]] = Arbitrary(for {
    ts <- arbitrary[List[Term[Rational]]]
  } yield {
    Polynomial(ts).toDense
  })

  implicit val arbSparse: Arbitrary[PolySparse[Rational]] = Arbitrary(for {
    ts <- arbitrary[List[Term[Rational]]]
  } yield {
    Polynomial(ts).toSparse
  })

  property("terms") {
    forAll { (t: Term[Rational]) =>
      t.toTuple shouldBe ((t.exp, t.coeff))
      t.isIndexZero shouldBe (t.exp == 0)
      forAll { (x: Rational) =>
        t.eval(x) shouldBe t.coeff * x.pow(t.exp.toInt)
      }
      t.isZero shouldBe (t.coeff == 0)
      if (t.exp > 0) t.der.int shouldBe t
      t.int.der shouldBe t
    }
  }

  property("sparse p = p") {
    forAll { (p: PolySparse[Rational]) =>
      val d = p.toDense
      p shouldBe p
      p shouldBe d
      p.## shouldBe d.##
    }
  }

  property("dense p = p") {
    forAll { (p: PolyDense[Rational]) =>
      val s = p.toSparse
      p shouldBe p
      p shouldBe s
      p.## shouldBe s.##
    }
  }

  property("p.toSparse.toDense = p") {
    forAll { (p: PolyDense[Rational]) =>
      p.toSparse.toDense shouldBe p
    }
  }

  property("p.toDense.toSparse = p") {
    forAll { (p: PolySparse[Rational]) =>
      p.toDense.toSparse shouldBe p
    }
  }

  property("apply(p.toString).toDense = p") {
    forAll { (p: PolySparse[Rational]) =>
      Polynomial(p.toString).toDense shouldBe p
    }
  }

  property("apply(p.toString) = p") {
    forAll { (p: PolyDense[Rational]) =>
      Polynomial(p.toString) shouldBe p
    }
  }

  property("apply(r, 0) = r") {
    forAll { (r: Rational) =>
      val p = Polynomial(r, 0)
      p shouldBe r
      p.## shouldBe r.##
    }
  }

  property(s"p.shift(h) = p.compose(x + h)") {
    forAll { (p: Polynomial[BigInt], h: BigInt) =>
      p.shift(h) shouldBe p.compose(Polynomial.x[BigInt] + Polynomial.constant(h))
    }
  }

  def gcdTest(x: Polynomial[Rational], y: Polynomial[Rational]): Unit = {
    if (!x.isZero || !y.isZero) {
      val gcd = spire.math.gcd[Polynomial[Rational]](x, y)
      if (!gcd.isZero) {
        (x % gcd) shouldBe 0
        (y % gcd) shouldBe 0
      }
    }
  }

  property("test gcd regression") {
    val x = poly"(3/37x^9 - 85x^7 - 71/4x^6 + 27/25x)"
    val y = poly"(17/9x^8 - 1/78x^6)"
    gcdTest(x.toDense, y.toDense)
  }

  property("x % gcd(x, y) == 0 && y % gcd(x, y) == 0") {
    implicit val arbPolynomial: Arbitrary[Polynomial[Rational]] = Arbitrary(for {
      ts <- Gen.listOf(for {
          c <- arbitrary[Rational]
          e <- arbitrary[Int] map { n => (n % 10).abs }
        } yield (e, c))
    } yield {
      Polynomial(ts.toMap).toDense
    })

    forAll { (x: Polynomial[Rational], y: Polynomial[Rational]) =>
      gcdTest(x, y)
    }
  }
}

class PolynomialTest extends FunSuite {

  test("Polynomial(List(Term(0, 0), Term(0, 0))) should not throw") {
    val ts = Term(r"0", 0) :: Term(r"0", 0) :: Nil
    assert(Polynomial(ts) == Polynomial.zero[Rational])
  }

  test("polynomial term implicit operations") {
    val t = Term(r"5/6", 2)
    assert(t.eval(r"2") === r"10/3")
    assert(t.eval(r"2") === r"10/3")
    assert(t.isZero === false)
    assert(t.der === Term(r"5/3", 1))
    assert(t.int === Term(r"5/18", 3))
  }

  test("polynomial construction") {
    val p = Polynomial(Array(Term(r"1/2", 0), Term(r"1/4", 2), Term(r"2", 1)))
    assert(p.terms.toSet === Set(Term(r"1/2", 0), Term(r"1/4", 2), Term(r"2", 1)))
    assert(p === Polynomial("1/4x^2 + 2x + 1/2"))
    assert(p === Polynomial("1/4x² + 2x + 1/2"))
    assert(p === Polynomial("1/4x² + x + x + 1/2"))
    assert(p === Polynomial(Map(2 -> r"1/4", 1 -> r"2", 0 -> r"1/2")))
  }

  test("polynomial non-arithmetic functions") {
    val p = Polynomial("1/4x^2 + 2x + 1/2")

    assert(p.coeffsArray === Array(r"1/2", r"2", r"1/4"))
    assert(p.maxTerm === Term(r"1/4", 2))
    assert(p.degree === 2)
    assert(p.maxOrderTermCoeff === Rational(1,4))
    assert(p(r"2") === r"11/2")
    assert(p.isZero === false)
    assert(p.monic === Polynomial("x^2 + 8x + 2"))
    assert(p.derivative === Polynomial("1/2x + 2"))
    assert(p.integral === Polynomial("1/12x^3 + x^2 + 1/2x"))

    assert(p.toDense.coeffs === Array(r"1/2", r"2/1", r"1/4"))
    assert(p.toDense.maxTerm === Term(r"1/4", 2))
    assert(p.toDense.degree === 2)
    assert(p.toDense.maxOrderTermCoeff === Rational(1,4))
    assert(p.toDense.apply(r"2") === r"11/2")
    assert(p.toDense.isZero === false)
    assert(p.toDense.monic === Polynomial.dense(Array(r"2/1", r"8/1", r"1/1")))
    assert(p.toDense.derivative === Polynomial.dense(Array(r"2/1", r"1/2")))
    assert(p.toDense.integral === Polynomial.dense(Array(r"0", r"1/2", r"1/1", r"1/12")))

  }

  test("polynomial arithmetic") {

    val p1 = Polynomial("1/4x^2 + 2x + 1/2")
    val p2 = Polynomial("1/4x^2 + 3x + 1/2")

    val legSparse = SpecialPolynomials.legendres[Rational](4).toList

    assert(p1 + p2 === Polynomial("1/2x^2 + 5x + 1"))
    assert(legSparse(2) * legSparse(3) === Polynomial("15/4x^5 - 7/2x^3 + 3/4x"))
    assert(p1 % p2 === Polynomial("-x"))
    assert(p1 /~ p2 === Polynomial("1"))

    val legDense = legSparse.map(_.toDense)

    assert(p1 + p2 === Polynomial.dense(Array(r"1/1", r"5/1", r"1/2")))
    assert((legDense(2) * legDense(3)).coeffsArray === Array(r"0", r"3/4", r"0", r"-7/2", r"0", r"15/4"))
    assert(p1 % p2 === Polynomial("-x"))
    assert(p1 /~ p2 === Polynomial("1"))

  }

  test("special polynomials") {

    val leg = SpecialPolynomials.legendres[Rational](5).toList
    val lag = SpecialPolynomials.laguerres[Rational](5).toList
    val chebFirstKind = SpecialPolynomials.chebyshevsFirstKind[Rational](5).toList
    val chebSecondKind = SpecialPolynomials.chebyshevsSecondKind[Rational](5).toList
    val hermProb = SpecialPolynomials.probHermites[Rational](5).toList
    val hermPhys = SpecialPolynomials.physHermites[Rational](5).toList

    assert(leg(4) === Polynomial("35/8x^4 - 30/8x^2 + 3/8"))
    assert(lag(4) === Polynomial("1/24x^4 - 16/24x^3 + 72/24x^2 - 96/24x + 1"))
    assert(chebFirstKind(4) === Polynomial("8x^4 - 8x^2 + 1"))
    assert(chebSecondKind(4) === Polynomial("16x^4 - 12x^2 + 1"))
    assert(hermProb(4) === Polynomial("x^4 - 6x^2 + 3"))
    assert(hermPhys(4) === Polynomial("16x^4 - 48x^2 + 12"))

  }

  test("GCD returns nice results") {
    val a = Polynomial("x^2 + 2x + 1")
    val b = Polynomial("x - 1")
    assert(spire.math.gcd(a, b) == 1)
    assert(spire.math.gcd(2 *: a, Polynomial("2")) == 2)
    assert(spire.math.gcd(2 *: a, 2 *: b) == 2)
  }

  test("GCD doesn't run out of memory for BigDecimals") {
    val a = Polynomial.linear(BigDecimal("2"))
    val b = Polynomial.constant(BigDecimal("3.4"))
    val c = (a + b) * (a + b) // (4x² + 13.6x + 11.56)
    assert(spire.math.gcd(a, c) === BigDecimal("0.02"))
    assert(spire.math.gcd(a + b, c) === a + b)
  }

  test("Polynomial(terms...) sums terms") {
    val terms = List(
      Term(Rational("-2/17"), 10),
      Term(Rational("97/8"), 0),
      Term(Rational("-8/7"), 0),
      Term(Rational("-8/47"), 47),
      Term(Rational("-1/71"), 26),
      Term(Rational("1"), 0),
      Term(Rational("0"), 1),
      Term(Rational("-29/8"), 19),
      Term(Rational("55/7"), 57),
      Term(Rational("-8/97"), 93),
      Term(Rational("-99/62"), 1),
      Term(Rational("0"), 58),
      Term(Rational("-7/22"), 1),
      Term(Rational("-93/70"), 38),
      Term(Rational("-2/21"), 54),
      Term(Rational("34/79"), 47),
      Term(Rational("-56/55"), 49),
      Term(Rational("19/44"), 0))
    val expected = terms
      .map { case Term(c, k) => Polynomial(Map(k -> c)) }
      .foldLeft(Polynomial.zero[Rational])(_ + _)
    assert(Polynomial(terms) === expected)
  }
}
