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
		assert(p.terms === Array(Term(Rational(1,2), 0L), Term(Rational(1,4), 2L), Term(Rational(2/1), 1L))
	}

	test("polynomial non-arithmetic functions") {
		val p = Polynomial(Array(Term(Rational(1,2), 0L), Term(Rational(1,4), 2L), Term(Rational(2/1), 1L)))
		
		expectResult(Array(Rational(1,4), Rational(2/1), Rational(1,2)))

	}

}