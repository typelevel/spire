package spire.math

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag
import spire.algebra._
import spire.math.poly._
import spire.syntax.literals._
import spire.syntax.euclideanRing._
import spire.optional.rationalTrig._
import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalatest._
import prop._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary


object MonomialSetup {

  implicit val arbitraryRational = Arbitrary(for {
    n0 <- arbitrary[Long]
    d0 <- arbitrary[Long]
  } yield {
    val (n, d) = (n0 % 100, d0 % 100)
    if (d == 0L) Rational(n, 1L) else Rational(n, d)
  })

  implicit def arbitraryVariable = Arbitrary(for {
    c <- Gen.choose('a', 'e')
    v <- Gen.choose(1, 10)
  } yield (c, v))

}

class MonomialCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  import MonomialSetup._

  implicit val er = Eq[Rational]
  implicit val fr = Field[Rational]
  implicit val cr = implicitly[ClassTag[Rational]]
  implicit val or = Order[Rational]

  runMonomialCheck[Rational]("rational")

  def runMonomialCheck[A: Arbitrary: Order: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[Monomial[A]] = Arbitrary(for {
      coeff <- arbitrary[A]
      vs <- arbitrary[List[(Char,Int)]]
    } yield Monomial(coeff, vs) )
    runTest[A](s"$typ/monomial")
  }

  def runTest[A: Order: Field: ClassTag](name: String)(implicit arb: Arbitrary[Monomial[A]]) {
    type M = Monomial[A]

    val zero = Monomial.zero[A]
    val one = Monomial.one[A]

    property(s"$name m = m") {
      forAll { (m: M) => m should be === m }
    }

    property(s"$name m + 0 = m") {
      forAll { (m: M) => m + zero should be === m }
    }

    property(s"$name m + (-m) = 0") {
      forAll { (m: M) => m + (-m) should be === zero }
    }

    property(s"$name m * 0 = 0") {
      forAll { (m: M) => m * zero should be === zero }
    }

    property(s"$name m * 1 = m") {
      forAll { (m: M) => m * one should be === m }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: M, y: M) => x * y should be === y * x }
    }

    property(s"$name x + y = y + x") {
      forAll { (x: M, y: M) => 
        if(x.vars == y.vars) x + y should be === y + x }
    }

  }

}

class MonomialTest extends FunSuite {

  test("Monomial construction") {
    val m = Monomial(r"3/2",('d', 3), ('b', 2), ('c', 0), ('a', 2))
    assert(m.vars === Map(('a', 2), ('b', 2), ('d', 3)))
    assert(m.toString === " + 3/2a^2b^2d^3")
    assert((-m).toString === " - 3/2a^2b^2d^3")
    assert(m === Monomial(r"3/2",('c', 0), ('b', 2), ('d', 3), ('a', 2)))
    assert(Monomial.zero[Rational].toString === " + (0)")
    assert(Monomial.one[Rational].toString === " + 1")
    assert(Monomial.xzero(r"22/7").toString === " + 22/7")
  }

  test("Monomial basic operations") {
    val m = Monomial(r"5/6", ('x', 2), ('y', 1), ('z', 0))
    assert(m.eval(Map(('x', r"2"), ('y', r"3"), ('z', r"2"))) === r"10")
    assert(m.isZero === false)
    assert((-m).toString === " - 5/6x^2y")
    assert(m.degree === 3)
  }

  test("Monomial arithmetic") {
    val m1 = Monomial(r"16/17", ('x', 1), ('y', 4), ('z', 2))
    val m2 = Monomial(r"3/7", ('x', 3), ('z', 2))
    assert(m1 + Monomial(r"1/17", ('x', 1), ('y', 4), ('z', 2)) === Monomial(r"1", ('x', 1), ('y', 4), ('z', 2)))
    assert(m2 + m2 === Monomial(r"6/7", ('x', 3), ('z', 2)))
    assert(m1 - Monomial(r"1", ('x', 1), ('y', 4), ('z', 2)) === Monomial(r"-1/17", ('x', 1), ('y', 4), ('z', 2)))
    assert(m1 * m2 === Monomial(r"48/119", ('x', 4), ('y', 4), ('z', 4)))
    assert(m1 :* r"3/4" === Monomial(r"12/17", ('x', 1), ('y', 4), ('z', 2)))
    assert(m2 :/ r"1/2" === Monomial(r"6/7", ('x', 3), ('z', 2)))
    assert(-m1 + m1 === Monomial.zero[Rational])
    assert(m1 * Monomial.one[Rational] === m1)
    assert(m1 / Monomial.one[Rational] === m1)
    assert(m1 / Monomial(r"1", ('x', 1)) === Monomial(r"16/17", ('y', 4), ('z', 2)))
    assert(m2 * Monomial.zero[Rational] === Monomial.zero[Rational])
  }

  test("Monomial GCD / LCM functions") {
    val m1 = Monomial(r"15/17", ('x', 2), ('y', 4), ('z', 2))
    val m2 = Monomial(r"4/7", ('x', 3), ('z', 1))
    assert(m1.gcd(m2) === Monomial(r"1/119", ('x', 2), ('z', 1)))
    assert(m2.gcd(Monomial.one[Rational]) === Monomial(r"1/7", ('x', 0)))
    assert(m2.lcm(m1) === Monomial(r"60", ('x', 3), ('z', 2)))
  }

}

class MultivariatePolynomialCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  import MonomialSetup._

  implicit val er = Eq[Rational]
  implicit val fr = Field[Rational]
  implicit val cr = implicitly[ClassTag[Rational]]
  implicit val or = Order[Rational]

  runMVPCheck[Rational]("rational")

  implicit def mono[A: Arbitrary: Field: Order: ClassTag] = Arbitrary(for {
    coeff <- arbitrary[A]
    vs <- arbitrary[List[(Char,Int)]]
  } yield Monomial(coeff, vs) )

  @tailrec private final def uniqueTerms[A](ts: Array[Monomial[A]], a: ArrayBuilder[Monomial[A]] = ArrayBuilder.make[Monomial[A]])
    (implicit eq: Eq[Monomial[A]]): Array[Monomial[A]] = ts.length match {
      case 0 => a.result()
      case 1 => a += ts(0); a.result()
      case _ => {
        val reduction = ts.filter(t => eq.eqv(t,ts(0))).reduce(_ + _)
        a += reduction
        uniqueTerms(ts.filterNot(t => eq.eqv(t,ts(0))), a)
      }
  }   

  def runMVPCheck[A: Arbitrary: Field: Order: ClassTag](typ: String) {
    implicit val arb: Arbitrary[MultivariatePolynomial[A]] = Arbitrary(for {
      m <- arbitrary[Array[Monomial[A]]]
    } yield MultivariatePolynomial(uniqueTerms(m)) )
    runTest[A](s"$typ/lexicographic")
  }

  def runTest[A: Order: Field: ClassTag](name: String)(implicit arb: Arbitrary[MultivariatePolynomial[A]]) {
    type P = MultivariatePolynomial[A]

    val zero = MultivariatePolynomial.zero[A]
    val one = MultivariatePolynomial.one[A]

    property(s"$name p = p") {
      forAll { (p: P) => p should be === p }
    }

    property(s"$name p + 0 = p") {
      forAll { (p: P) => p + zero should be === p }
    }

    property(s"$name p + (-p) = 0") {
      forAll { (p: P) => p + (-p) should be === zero }
    }

    property(s"$name p - p = 0") {
      forAll { (p: P) => p - p should be === zero }
    }

    property(s"$name p * 0 = 0") {
      forAll { (p: P) => p * zero should be === zero }
    }

    property(s"$name p * 1 = p") {
      forAll { (p: P) => p * one should be === p }
    }

    property(s"$name p /~ 1 = p") {
      forAll { (p: P) => p /~ one should be === p }
    }

    property(s"$name p /~ p = 1") {
      forAll { (p: P) => if (!p.isZero) p /~ p should be === one }
    }

    property(s"$name p % p = 0") {
      forAll { (p: P) => if (!p.isZero) p % p should be === zero }
    }

    property(s"$name x + y = y + x") {
      forAll { (x: P, y: P) => if(y.terms.length < 6 && x.terms.length < 6) x + y should be === y + x }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: P, y: P) => if(y.terms.length < 6 && x.terms.length < 6) x * y should be === y * x }
    }

    property(s"$name (x /~ y) * y + (x % y) = x") {
      forAll { (x: P, y: P) => if(y.terms.length < 6 && x.terms.length < 6 && !y.isZero) (x /~ y) * y + (x % y) should be === x }
    }  
  }

}

class MultivariatePolynomialTest extends FunSuite {

  test("Multivariate polynomial construction") {
    val m1 = Monomial(r"3/2",('d', 3), ('b', 2), ('c', 0), ('a', 2))
    val m2 = Monomial(r"1/2",('d', 2), ('b', 1), ('c', 1), ('a', 1))
    val m3 = Monomial(r"18/9",('d', 5), ('b', 0), ('c', 2), ('a', 3))
    val p = MultivariatePolynomial(m1, m2, m3)
    assert(p.terms === Array(m3, m1, m2))
    assert(p.toString === "(2a^3c^2d^5 + 3/2a^2b^2d^3 + 1/2abcd^2)")
    assert(p.toString === MultivariatePolynomial("3/2a^2b^2d^3 + 2a^3c^2d^5 + 1/2abcd^2").toString)
    assert((-p).toString === "(-2a^3c^2d^5 - 3/2a^2b^2d^3 - 1/2abcd^2)")
    assert(p === MultivariatePolynomial(m2, m3, m1))
    assert(MultivariatePolynomial.zero[Rational].toString === "(0)")
    assert(MultivariatePolynomial.one[Rational].toString === "(1)")
    val l = List(m2, m1 , m3) 
    assert(MultivariatePolynomial(l) === p)
  }

  test("Multivariate polynomial operations") {
    val m1 = Monomial(r"1/4",('d', 2), ('b', 3))
    val m2 = Monomial(r"1/2",('d', 1), ('a', 1))
    val m3 = Monomial(r"2",('c', 2), ('a', 3))
    val p = MultivariatePolynomial(m1, m2, m3)
    assert(p.eval(Map(('a', r"2"), ('b', r"3"), ('c', r"2"), ('d', r"1"))) === r"287/4")
    assert(p.isZero === false)
    assert(p.degree === 5)
  }

  test("Multivariate polynomial arithmetic") {
    val m1 = Monomial(r"3/2",('d', 3), ('b', 2))
    val m2 = Monomial(r"1/2",('c', 1), ('a', 1))
    val m3 = Monomial(r"18/9",('a', 3))
    val p1 = MultivariatePolynomial(m1, m2, m3)
    val m4 = Monomial(r"1/4",('b', 3))
    val m5 = Monomial(r"1/2",('d', 1), ('a', 1))
    val m6 = Monomial(r"2",('c', 2), ('a', 3))
    val p2 = MultivariatePolynomial(m4, m5, m6)
    assert(p1 + p2 === MultivariatePolynomial("2a^3c^2 + 2a^3 + 1/2ac + 1/2ad + 1/4b^3 + 3/2b^2d^3"))
    assert(p2 + p1 === MultivariatePolynomial("2a^3c^2 + 2a^3 + 1/2ac + 1/2ad + 1/4b^3 + 3/2b^2d^3"))
    assert(p1 - p2 === MultivariatePolynomial("-2a^3c^2 + 2a^3 + 1/2ac - 1/2ad - 1/4b^3 + 3/2b^2d^3"))
    assert(p1 * p2 === MultivariatePolynomial("4a^6c^2 + a^4c^3 + a^4d + 1/2a^3b^3 + 3a^3b^2c^2d^3 + 1/4a^2cd + 1/8ab^3c + 3/4ab^2d^4 + 3/8b^5d^3"))
    assert(p2 :* r"1/2" === MultivariatePolynomial("a^3c^2 + 1/4ad + 1/8b^3"))
    assert(p1 :/ r"1/4" === MultivariatePolynomial("8a^3 + 2ac + 6b^2d^3"))
    assert(-p1 + p1 === MultivariatePolynomial.zero[Rational])
    assert(p2 + (-p2) === MultivariatePolynomial.zero[Rational])
    assert(p1 - p1 === MultivariatePolynomial.zero[Rational])
    assert(p2 - p2 === MultivariatePolynomial.zero[Rational])
    assert(p1 * MultivariatePolynomial.one[Rational] === p1)
    assert(MultivariatePolynomial.one[Rational] * p1 === p1)
    assert(p2 * MultivariatePolynomial.zero[Rational] === MultivariatePolynomial.zero[Rational])
    assert(MultivariatePolynomial.zero[Rational] * p2 === MultivariatePolynomial.zero[Rational])
  }

  test("Multivariate polynomial euclidean ring operations") {
    val p1 = MultivariatePolynomial("x + y + z")
    val p2 = MultivariatePolynomial("x^3 - 3xyz + y^3 + z^3")
    assert(p2 /% p1 === (MultivariatePolynomial("x^2 - xy - xz + y^2 - yz + z^2"), MultivariatePolynomial.zero[Rational]))
    assert(p1 /~ p2 === MultivariatePolynomial.zero[Rational])
    assert((p2 /~ p1) * p1 + (p2 % p1) === p2)
    assert((p1 /~ p2) * p1 + (p1 % p2) === p1)
  }

}
