package spire.math

import spire.math.poly._
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

  implicit val arbitraryDenseRationalPolynomial: Arbitrary[PolyDense[Rational]] = Arbitrary(for {
    ts <- arbitrary[List[Term[Rational]]]
  } yield {
    val p = Polynomial(ts.foldLeft(Map.empty[Int, Rational]) { (m, t) =>
      m.updated(t.exp, m.getOrElse(t.exp, r"0") + t.coeff)
    }.filter { case (e, c) => c != 0 })
    Polynomial.dense(p.coeffs)
  })

  implicit val arbitrarySparseRationalPolynomial: Arbitrary[PolySparse[Rational]] = Arbitrary(for {
    ts <- arbitrary[List[Term[Rational]]]
  } yield {
    Polynomial(ts)
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

  property("sparse p = p") {
    forAll { (p: PolySparse[Rational]) =>
      p should be === p
    }
  }

  property("dense p = p") {
    forAll { (p: PolyDense[Rational]) =>
      p should be === p
    }
  }

  property("conversion dense to sparse to dense p = p") {
    forAll { (p: PolyDense[Rational]) =>
      p.toSparse.toDense should be === p
    }
  }

  property("conversion sparse to dense to sparse p = p") {
    forAll { (p: PolySparse[Rational]) =>
      p.toDense.toSparse should be === p
    }
  }

  property("sparse apply(p.toString) = p") {
    forAll { (p: PolySparse[Rational]) =>
      Polynomial(p.toString).toDense should be === p
    }
  }

  property("dense apply(p.toString) = p") {
    forAll { (p: PolyDense[Rational]) =>
      Polynomial(p.toString) should be === p
    }
  }

  property("sparse p + 0 = p") {
    forAll { (p: PolySparse[Rational]) =>
      p + Polynomial("0") should be === p
    }
  }

  property("dense p + 0 = p") {
    forAll { (p: PolyDense[Rational]) =>
      p + Polynomial("0") should be === p
    }
  }

  property("sparse p + (-p) = 0") {
    forAll { (p: PolySparse[Rational]) =>
      p + (-p) should be === Polynomial("0")
    }
  }

  property("dense p + (-p) = 0") {
    forAll { (p: PolyDense[Rational]) =>
      p + (-p) should be === Polynomial("0")
    }
  }

  property("sparse p * 0 = 0") {
    forAll { (p: PolySparse[Rational]) =>
      p * Polynomial("0") should be === Polynomial("0")
    }
  }

  property("dense p * 0 = 0") {
    forAll { (p: PolyDense[Rational]) =>
      p * Polynomial("0") should be === Polynomial("0")
    }
  }

  property("sparse p * 1 = p") {
    forAll { (p: PolySparse[Rational]) =>
      p * Polynomial("1") should be === p
    }
  }

  property("dense p * 1 = p") {
    forAll { (p: PolyDense[Rational]) =>
      p * Polynomial("1") should be === p
    }
  }

  property("sparse p /~ 1 = p") {
    forAll { (p: PolySparse[Rational]) =>
      p /~ Polynomial("1") should be === p
    }
  }

  property("dense p /~ 1 = p") {
    forAll { (p: PolyDense[Rational]) =>
      p /~ Polynomial("1") should be === p
    }
  }

  property("sparse p /~ p = 1") {
    forAll { (p: PolySparse[Rational]) =>
      if (!p.isZero) {
        p /~ p should be === Polynomial("1")
      }
    }
  }

  property("dense p /~ p = 1") {
    forAll { (p: PolyDense[Rational]) =>
      if (!p.isZero) {
        p /~ p should be === Polynomial("1")
      }
    }
  }

  property("sparse p % p = 0") {
    forAll { (p: PolySparse[Rational]) =>
      if (!p.isZero) {
        p % p should be === Polynomial("0")
      }
    }
  }

  property("dense p % p = 0") {
    forAll { (p: PolyDense[Rational]) =>
      if (!p.isZero) {
        p % p should be === Polynomial("0")
      }
    }
  }

  property("sparse x + y = y + x") {
    forAll { (x: PolySparse[Rational], y: PolySparse[Rational]) =>
      x + y should be === y + x
    }
  }

  property("dense x + y = y + x") {
    forAll { (x: PolyDense[Rational], y: PolyDense[Rational]) =>
      x + y should be === y + x
    }
  }

  property("sparse x * y = y * x") {
    forAll { (x: PolySparse[Rational], y: PolySparse[Rational]) =>
      (x * y).toString should be === (y * x).toString
    }
  }

  property("dense x * y = y * x") {
    forAll { (x: PolyDense[Rational], y: PolyDense[Rational]) =>
      (x * y).toString should be === (y * x).toString
    }
  }

  property("sparse (x /~ y) * y + (x % y) = x") {
    forAll { (x: PolySparse[Rational], y: PolySparse[Rational]) =>
      if (!y.isZero) {
        (x /~ y) * y + (x % y) should be === x
      }
    }
  }

  property("dense (x /~ y) * y + (x % y) = x") {
    forAll { (x: PolyDense[Rational], y: PolyDense[Rational]) =>
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

    assert(p.coeffs === Array(r"1/4", r"2", r"1/2"))
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
    assert(p.toDense(r"2") === r"11/2")
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
    assert((legDense(2) * legDense(3)).coeffs === Array(r"0", r"3/4", r"0", r"-7/2", r"0", r"15/4"))
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

}
