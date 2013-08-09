package spire.math

import spire.syntax.literals._
import spire.syntax.euclideanRing._

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class PolynomialCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {
  
  implicit val arbitraryRational = Arbitrary(for {
    n0 <- arbitrary[Long]
    d0 <- arbitrary[Long]
  } yield {
    val (n, d) = (n0 % 100, d0 % 100)
    if (d == 0L) Rational(n, 1L) else Rational(n, d)
  })

  implicit val arbitraryRationalTerm = Arbitrary(for {
    c <- arbitrary[Rational]
    e0 <- arbitrary[Int]
  } yield {
    Term(c, (e0 % 100).abs)
  })

  implicit val arbitraryRationalPolynomial = Arbitrary(for {
    ts <- arbitrary[List[Term[Rational]]]
  } yield {
    Polynomial(ts.foldLeft(Map.empty[Int, Rational]) { (m, t) =>
      m.updated(t.exp, m.getOrElse(t.exp, r"0") + t.coeff)
    }.filter { case (e, c) => c != 0 })
  })

  property("terms") {
    forAll { (t: Term[Rational]) =>
      t.toTuple should be === (t.exp, t.coeff)
      t.isIndexZero should be === (t.exp == 0)
      forAll { (x: Rational) =>
        t.eval(x) should be === t.coeff * x.pow(t.exp.toInt)
      }
      t.isZero should be === (t.coeff == 0)
      if (t.exp > 0) t.der.int should be === t
      t.int.der should be === t
    }
  }

  property("p = p") {
    forAll { (p: Polynomial[Rational]) =>
      p should be === p
    }
  }

  property("apply(p.toString) = p") {
    forAll { (p: Polynomial[Rational]) =>
      Polynomial(p.toString) should be === p
    }
  }

  property("p + 0 = p") {
    forAll { (p: Polynomial[Rational]) =>
      p + Polynomial("0") should be === p
    }
  }

  property("p + (-p) = 0") {
    forAll { (p: Polynomial[Rational]) =>
      p + (-p) should be === Polynomial("0")
    }
  }

  property("p * 0 = 0") {
    forAll { (p: Polynomial[Rational]) =>
      p * Polynomial("0") should be === Polynomial("0")
    }
  }

  property("p * 1 = p") {
    forAll { (p: Polynomial[Rational]) =>
      p * Polynomial("1") should be === p
    }
  }

  property("p /~ 1 = p") {
    forAll { (p: Polynomial[Rational]) =>
      p /~ Polynomial("1") should be === p
    }
  }

  property("p /~ p = 1") {
    forAll { (p: Polynomial[Rational]) =>
      if (!p.isZero) {
        p /~ p should be === Polynomial("1")
      }
    }
  }

  property("p % p = 0") {
    forAll { (p: Polynomial[Rational]) =>
      if (!p.isZero) {
        p % p should be === Polynomial("0")
      }
    }
  }

  property("x + y = y + x") {
    forAll { (x: Polynomial[Rational], y: Polynomial[Rational]) =>
      x + y should be === y + x
    }
  }

  property("x * y = y * x") {
    forAll { (x: Polynomial[Rational], y: Polynomial[Rational]) =>
      (x * y).toString should be === (y * x).toString
    }
  }

  property("(x /~ y) * y + (x % y) = x") {
    forAll { (x: Polynomial[Rational], y: Polynomial[Rational]) =>
      if (!y.isZero) {
        (x /~ y) * y + (x % y) should be === x
      }
    }
  }
}

class PolynomialTest extends FunSuite {

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
    assert(p === Polynomial(Map(2 -> r"1/4", 1 -> r"2", 0 -> r"1/2")))
  }

  test("polynomial non-arithmetic functions") {
    val p = Polynomial("1/4x^2 + 2x + 1/2")

    assert(p.coeffs === List(r"1/4", r"2", r"1/2"))
    assert(p.maxTerm === Term(r"1/4", 2))
    assert(p.degree === 2)
    assert(p.maxOrderTermCoeff === Rational(1,4))
    assert(p(r"2") === r"11/2")
    assert(p.isZero === false)

    assert(p.monic === Polynomial("x^2 + 8x + 2"))
    assert(p.derivative === Polynomial("1/2x + 2"))
    assert(p.integral === Polynomial("1/12x^3 + x^2 + 1/2x"))
  }

  test("polynomial arithmetic") {

    val p1 = Polynomial("1/4x^2 + 2x + 1/2")
    val p2 = Polynomial("1/4x^2 + 3x + 1/2")

    def legendres(i: Int): List[Polynomial[Rational]] = {
      val one = Polynomial(r"1", 0)
      val x = Polynomial(r"1", 1)
      lazy val leg : Stream[Polynomial[Rational]] = {
        def loop(pnm1: Polynomial[Rational], pn: Polynomial[Rational], n: Int = 1): Stream[Polynomial[Rational]] = {
          val a = Polynomial(Rational(1, n + 1), 0)
          val b = Polynomial(Rational(2 * n + 1), 1)
          val c = Polynomial(-Rational(n), 0)
          pn #:: loop(pn, a * (b * pn + c * pnm1), n + 1)
        }
        one #:: loop(one, x)
      }
      leg.take(i).toList
    }

    val leg = legendres(4)

    assert(p1 + p2 === Polynomial("1/2x^2 + 5x + 1"))
    assert(leg(2) * leg(3) === Polynomial("15/4x^5 - 7/2x^3 + 3/4x"))
    assert(p1 % p2 === Polynomial("-x"))
    assert(p1 /~ p2 === Polynomial("1"))
  }

}
