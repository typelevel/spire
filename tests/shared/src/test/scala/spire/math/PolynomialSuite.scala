package spire
package math

import spire.algebra._
import spire.math.poly._
import spire.std.bigDecimal._
import spire.syntax.euclideanRing._
import spire.syntax.literals._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import java.util.Arrays

object PolynomialSetup {
  implicit val arbitraryRational: Arbitrary[Rational] = Arbitrary(for {
    n0 <- arbitrary[Long]
    d0 <- arbitrary[Long]
  } yield {
    val (n, d) = (n0 % 100, d0 % 100)
    if (d == 0L) Rational(n, 1L) else Rational(n, d)
  })

  // default scalacheck bigdecimals are weird
  implicit val arbitraryBigDecimal: Arbitrary[BigDecimal] = Arbitrary(for {
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

class PolynomialSuite extends munit.FunSuite {

  test("Polynomial(List(Term(-1, 4), List(1, 4))).toSparse should be equal to Polynomial.zero") {
    val ts = Term(r"-1", 4) :: Term(r"1", 4) :: Nil
    assert(Polynomial(ts).toSparse == Polynomial.zero[Rational])
  }

  test("Polynomial(List(Term(0, 0), Term(0, 0))) should not throw") {
    val ts = Term(r"0", 0) :: Term(r"0", 0) :: Nil
    assert(Polynomial(ts) == Polynomial.zero[Rational])
  }

  test("polynomial term implicit operations") {
    val t = Term(r"5/6", 2)
    assertEquals(t.eval(r"2"), r"10/3")
    assertEquals(t.eval(r"2"), r"10/3")
    assertEquals(t.isZero, false)
    assertEquals(t.der, Term(r"5/3", 1))
    assertEquals(t.int, Term(r"5/18", 3))
  }

  test("polynomial construction") {
    val p = Polynomial(Array(Term(r"1/2", 0), Term(r"1/4", 2), Term(r"2", 1)))
    assertEquals(p.terms.toSet, Set(Term(r"1/2", 0), Term(r"1/4", 2), Term(r"2", 1)))
    assert(p == Polynomial("1/4x^2 + 2x + 1/2"))
    assert(p == Polynomial("1/4x² + 2x + 1/2"))
    assert(p == Polynomial("1/4x² + x + x + 1/2"))
    assertEquals(p, Polynomial(Map(2 -> r"1/4", 1 -> r"2", 0 -> r"1/2")))
  }

  test("polynomial non-arithmetic functions") {
    val p = Polynomial("1/4x^2 + 2x + 1/2")

    assert(Arrays.equals(p.coeffsArray.toArray[Object], Array[Object](r"1/2", r"2", r"1/4")))
    assertEquals(p.maxTerm, Term(r"1/4", 2))
    assertEquals(p.degree, 2)
    assertEquals(p.maxOrderTermCoeff, Rational(1, 4))
    assertEquals(p(r"2"), r"11/2")
    assertEquals(p.isZero, false)
    assertEquals(p.monic, Polynomial("x^2 + 8x + 2"))
    assertEquals(p.derivative, Polynomial("1/2x + 2"))
    assertEquals(p.integral, Polynomial("1/12x^3 + x^2 + 1/2x"))

    assert(Arrays.equals(p.toDense.coeffs.toArray[Object], Array[Object](r"1/2", r"2/1", r"1/4")))
    assertEquals(p.toDense.maxTerm, Term(r"1/4", 2))
    assertEquals(p.toDense.degree, 2)
    assertEquals(p.toDense.maxOrderTermCoeff, Rational(1, 4))
    assertEquals(p.toDense.apply(r"2"), r"11/2")
    assertEquals(p.toDense.isZero, false)
    assertEquals(p.toDense.monic, Polynomial.dense(Array(r"2/1", r"8/1", r"1/1")))
    assertEquals(p.toDense.derivative, Polynomial.dense(Array(r"2/1", r"1/2")))
    assertEquals(p.toDense.integral, Polynomial.dense(Array(r"0", r"1/2", r"1/1", r"1/12")))

  }

  test("polynomial arithmetic") {

    val p1 = Polynomial("1/4x^2 + 2x + 1/2")
    val p2 = Polynomial("1/4x^2 + 3x + 1/2")

    val legSparse = SpecialPolynomials.legendres[Rational](4).toList

    assertEquals(p1 + p2, Polynomial("1/2x^2 + 5x + 1"))
    assertEquals(legSparse(2) * legSparse(3), Polynomial("15/4x^5 - 7/2x^3 + 3/4x"))
    assertEquals((p1.emod(p2)), Polynomial("-x"))
    assertEquals((p1.equot(p2)), Polynomial("1"))

    val legDense = legSparse.map(_.toDense)

    assertEquals(p1 + p2, Polynomial.dense(Array(r"1/1", r"5/1", r"1/2")))
    assert(
      Arrays.equals((legDense(2) * legDense(3)).coeffsArray.toArray[Object],
                    Array[Object](r"0", r"3/4", r"0", r"-7/2", r"0", r"15/4")
      )
    )
    assertEquals((p1.emod(p2)), Polynomial("-x"))
    assertEquals((p1.equot(p2)), Polynomial("1"))

  }

  test("special polynomials") {

    val leg = SpecialPolynomials.legendres[Rational](5).toList
    val lag = SpecialPolynomials.laguerres[Rational](5).toList
    val chebFirstKind = SpecialPolynomials.chebyshevsFirstKind[Rational](5).toList
    val chebSecondKind = SpecialPolynomials.chebyshevsSecondKind[Rational](5).toList
    val hermProb = SpecialPolynomials.probHermites[Rational](5).toList
    val hermPhys = SpecialPolynomials.physHermites[Rational](5).toList

    assertEquals(leg(4), Polynomial("35/8x^4 - 30/8x^2 + 3/8"))
    assertEquals(lag(4), Polynomial("1/24x^4 - 16/24x^3 + 72/24x^2 - 96/24x + 1"))
    assertEquals(chebFirstKind(4), Polynomial("8x^4 - 8x^2 + 1"))
    assertEquals(chebSecondKind(4), Polynomial("16x^4 - 12x^2 + 1"))
    assertEquals(hermProb(4), Polynomial("x^4 - 6x^2 + 3"))
    assertEquals(hermPhys(4), Polynomial("16x^4 - 48x^2 + 12"))

  }

  /* TODO: define formally "nice" and document it
  test("GCD returns nice results") {
    val a = Polynomial("x^2 + 2x + 1")
    val b = Polynomial("x - 1")
    assertEquals(spire.math.gcd(a, b), 1)
    assertEquals(spire.math.gcd(2 *: a, Polynomial("2")), 2)
    assertEquals(spire.math.gcd(2 *: a, 2 *: b), 2)
  }
   */

  test("GCD doesn't run out of memory for BigDecimals") {
    GCDRing[BigDecimal]
    import Polynomial.{constant, linear}
    val a = linear(BigDecimal("2")) // 2x
    val b = constant(BigDecimal("3.4")) // 3.4
    val c = a + b // 2x + 3.4
    val d = c * c // 4x² + 13.6x + 11.56
    // assertEquals((a gcd c), constant(BigDecimal("0.2"))) TODO: does not work anymore
    // assertEquals((a gcd d), constant(BigDecimal("0.04")))
    assertEquals((c.gcd(d)), c)
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
      Term(Rational("19/44"), 0)
    )
    val expected = terms
      .map { case Term(c, k) => Polynomial(Map(k -> c)) }
      .foldLeft(Polynomial.zero[Rational])(_ + _)
    assert(Polynomial(terms) == expected)
  }

  test("Derivative of constant zero polynomial is itself") {
    val polynomial: Polynomial[Rational] = Polynomial.constant(Rational.zero)
    val derivative = polynomial.derivative
    assertEquals(polynomial, derivative)
  }

}
