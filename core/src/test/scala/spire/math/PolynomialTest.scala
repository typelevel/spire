package spire.math

import org.scalatest.FunSuite

class PolynomialTest extends FunSuite {

  test("polynomial term construction") {
    val t1 = Term(Rational(5,6), 0L)
    val t2 = Term(-0.5, 1L)
    assert(t1.coeff === Rational(5,6))
    assert(t2.coeff === -0.5)
    assert(t1.exp === 0L)
    assert(t2.exp === 1L)
  }

  test("polynomial term non-arithmetic functions") {
    val t1 = Term(Rational(-5,6), 0L)
    assert(t1.toTuple === (Rational(-5,6), 0L))
    assert(t1.isIndexZero === true)
  }

  test("polynomial term implicit operations") {
    val t1 = Term(Rational(5,6), 2L)

    expectResult(Rational(10,3)) {
      t1.eval(Rational(2,1))
    }

    expectResult(false) {
      t1.isZero
    }

    expectResult(Term(Rational(5,3), 1L)) {
      t1.der
    }

    expectResult(Term(Rational(5,18), 3L)) {
      t1.int
    }
  }

  test("polynomial construction") {
    val p = Polynomial(Array(Term(Rational(1,2), 0L), Term(Rational(1,4), 2L), Term(Rational(2/1), 1L)))
    assert(p.terms === Array(Term(Rational(1,2), 0L), Term(Rational(1,4), 2L), Term(Rational(2/1), 1L)))
  }

  test("polynomial non-arithmetic functions") {
    val p = Polynomial(Array(Term(Rational(1,2), 0L), Term(Rational(1,4), 2L), Term(Rational(2/1), 1L)))

    expectResult(Rational(1,4)) {
      val a = p.coeffs
      a(0)
    }

    expectResult(Term(Rational(1,4), 2L)) {
      p.maxTerm
    }

    assert(p.maxOrder === 2L)
    assert(p.maxOrderTermCoeff === Rational(1,4))
    assert(p.degree === 2L)

    expectResult(Rational(11,2)) {
      p(Rational(2,1))
    }

    assert(p.isZero === false)

    expectResult(Polynomial(Array(Term(Rational(2,1), 0L), Term(Rational(1,1), 2L), Term(Rational(8/1), 1L)))) {
      p.monic
    }

    expectResult(Polynomial(Array(Term(Rational(1,2), 1L), Term(Rational(2,1), 0L)))) {
      p.derivative
    }

    expectResult(Polynomial(Array(Term(Rational(1,2), 1L), Term(Rational(1,12), 3L), Term(Rational(1/1), 2L)))) {
      p.integral
    }
  }

  test("polynomial arithmetic") {
    val pr = implicitly[PolynomialRing[Rational]]

    val p1 = Polynomial(Array(Term(Rational(1,2), 0L), Term(Rational(1,4), 2L), Term(Rational(2/1), 1L)))
    val p2 = Polynomial(Array(Term(Rational(1,2), 0L), Term(Rational(1,4), 2L), Term(Rational(3/1), 1L)))

    def legendres(i: Int)(implicit r: Numeric[Rational]) : List[Polynomial[Rational]] = {
      val one = Polynomial(Map(0L -> r.one))
      val x = Polynomial(Map(1L -> r.one))
      lazy val leg : Stream[Polynomial[Rational]] = {
        def loop(pnm1: Polynomial[Rational], pn: Polynomial[Rational], n: Int = 1) : Stream[Polynomial[Rational]] = {
          pn #:: loop(pn, pr.times(Polynomial(Map(0L -> r.fromInt(n + 1).reciprocal)), pr.plus(
            (pr.times(Polynomial(Map(1L -> r.fromInt(2 * n + 1))), pn)),
            (pr.times(Polynomial(Map(0L -> r.fromInt(n).unary_-)), pnm1)))), n + 1)
        }
        one #:: loop(one, x)
      }
      leg.take(i).toList
    }

    val leg = legendres(4)

    expectResult(Polynomial(Array(Term(Rational(1,1), 0L), Term(Rational(1,2), 2L), Term(Rational(5/1), 1L)))) {
      pr.plus(p1, p2)
    }

    expectResult(Polynomial(Array(Term(Rational(15,4), 5L), Term(Rational(-5,4), 3L), Term(Rational(3,4), 1L)))) {
      pr.times(leg(2), leg(3))
    }

    expectResult(Polynomial(Array(Term(Rational(-1,1), 1L)))) {
      pr.mod(p1, p2)
    }

    expectResult(Polynomial(Array(Term(Rational(1,1), 0L)))) {
      pr.quot(p1, p2)
    }
  }

}
