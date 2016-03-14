package spire.math

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder
import scala.reflect.ClassTag
import spire.algebra._
import spire.math.poly._
import spire.syntax.literals._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalatest.Matchers
import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
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
    c <- Gen.choose('x', 'z')
    v <- Gen.choose(1, 10)
  } yield (c, v))

}

class MonomialCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import MonomialSetup._

  implicit val er = Eq[Rational]
  implicit val fr = Field[Rational]
  implicit val cr = implicitly[ClassTag[Rational]]
  implicit val or = Order[Rational]

  runMonomialCheck[Rational]("rational")

  def runMonomialCheck[A: Arbitrary: Order: Field: ClassTag](typ: String): Unit = {
    implicit val arb: Arbitrary[Monomial[A]] = Arbitrary(for {
      coeff <- arbitrary[A]
      vs <- arbitrary[List[(Char,Int)]]
    } yield Monomial(coeff, vs) )
    runTest[A](s"$typ/monomial")
  }

  def runTest[A: Order: Field: ClassTag](name: String)(implicit arb: Arbitrary[Monomial[A]]): Unit = {
    type M = Monomial[A]

    val zero = Monomial.zero[A]
    val one = Monomial.one[A]

    property(s"$name m = m") {
      forAll { (m: M) => m shouldBe m }
    }

    property(s"$name m + 0 = m") {
      forAll { (m: M) => m add zero shouldBe m }
    }

    property(s"$name m + (-m) = 0") {
      forAll { (m: M) => m add (-m) shouldBe zero }
    }

    property(s"$name m * 0 = 0") {
      forAll { (m: M) => m * zero shouldBe zero }
    }

    property(s"$name m * 1 = m") {
      forAll { (m: M) => m * one shouldBe m }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: M, y: M) => x * y shouldBe y * x }
    }

    property(s"$name x + y = y + x") {
      forAll { (x: M, y: M) =>
        if(x.vars == y.vars) (x add y) shouldBe (y add x) }
    }

  }

}

class MonomialTest extends FunSuite {

  test("Monomial construction") {
    val m = Monomial(r"3/2",('d', 3), ('b', 2), ('c', 0), ('a', 2))
    assert(m.vars === Map(('a', 2), ('b', 2), ('d', 3)))
    assert(m.toString === "3/2a^2b^2d^3")
    assert((-m).toString === "-3/2a^2b^2d^3")
    assert(m === Monomial(r"3/2",('c', 0), ('b', 2), ('d', 3), ('a', 2)))
    assert(Monomial.zero[Rational].toString === "0")
    assert(Monomial.one[Rational].toString === "1")
    assert(Monomial.constant(r"22/7").toString === "22/7")
  }

  test("Monomial basic operations") {
    val m = Monomial(r"5/6", ('x', 2), ('y', 1), ('z', 0))
    assert(m.eval(Map(('x', r"2"), ('y', r"3"), ('z', r"2"))) === r"10")
    assert(m.isZero === false)
    assert((-m).toString === "-5/6x^2y")
    assert(m.degree === 3)
  }

  test("Monomial simplification") {
    import spire.implicits._
    val m1 = Monomial(3, ('x', 1), ('a', 1), ('x', 2)) // 3*x*a*x^2
    val m2 = Monomial(1, ('y', 2), ('x', 2), ('x', 2), ('y', 2))
    assert(m1.toString === "3ax^3")
    assert(m2.toString === "x^4y^4")
  }


  test("Monomial arithmetic") {
    val m1 = Monomial(r"16/17", ('x', 1), ('y', 4), ('z', 2))
    val m2 = Monomial(r"3/7", ('x', 3), ('z', 2))
    assert((m1 add Monomial(r"1/17", ('x', 1), ('y', 4), ('z', 2))) === Monomial(r"1", ('x', 1), ('y', 4), ('z', 2)))
    assert((m2 add m2) === Monomial(r"6/7", ('x', 3), ('z', 2)))
    assert((m1 subtract Monomial(r"1", ('x', 1), ('y', 4), ('z', 2))) === Monomial(r"-1/17", ('x', 1), ('y', 4), ('z', 2)))
    assert(m1 * m2 === Monomial(r"48/119", ('x', 4), ('y', 4), ('z', 4)))
    assert(m1 :* r"3/4" === Monomial(r"12/17", ('x', 1), ('y', 4), ('z', 2)))
    assert(m2 :/ r"1/2" === Monomial(r"6/7", ('x', 3), ('z', 2)))
    assert((-m1 add m1) === Monomial.zero[Rational])
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

class MultivariatePolynomialCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

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

  implicit def rationalMonomialEq = new MonomialEq[Rational] {
    val scalar = Semiring[Rational]
    val ct = implicitly[ClassTag[Rational]]
  }

  @tailrec private final def uniqueTerms[A](ts: Array[Monomial[A]], a: ArrayBuilder[Monomial[A]] = ArrayBuilder.make[Monomial[A]])
    (implicit eq: Eq[Monomial[A]]): Array[Monomial[A]] = ts.length match {
      case 0 => a.result()
      case 1 => a += ts(0); a.result()
      case _ => {
        val reduction = ts.filter(t => eq.eqv(t,ts(0))).reduce(_ add _)
        a += reduction
        uniqueTerms(ts.filterNot(t => eq.eqv(t,ts(0))), a)
      }
  }

  def runMVPCheck[A: Arbitrary: Field: Order: ClassTag](typ: String): Unit = {
    implicit val arb: Arbitrary[MultivariatePolynomial[A]] = Arbitrary(for {
      m <- arbitrary[Array[Monomial[A]]]
    } yield MultivariatePolynomial(uniqueTerms(m)) )
    runTest[A](s"$typ/lexicographic")
  }

  def runTest[A: Order: Field: ClassTag](name: String)(implicit arb: Arbitrary[MultivariatePolynomial[A]]): Unit = {
    type P = MultivariatePolynomial[A]

    val zero = MultivariatePolynomial.zero[A]
    val one = MultivariatePolynomial.one[A]

    property(s"$name p = p") {
      forAll { (p: P) => p shouldBe p }
    }

    property(s"$name p + 0 = p") {
      forAll { (p: P) => p + zero shouldBe p }
    }

    property(s"$name p + (-p) = 0") {
      forAll { (p: P) => (p + (-p)) shouldBe zero }
    }

    property(s"$name p - p = 0") {
      forAll { (p: P) => (p - p) shouldBe zero }
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
      forAll { (x: P, y: P) => if(y.terms.length < 6 && x.terms.length < 6) x + y shouldBe (y + x) }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: P, y: P) => if(y.terms.length < 6 && x.terms.length < 6) x * y shouldBe (y * x) }
    }

    property(s"$name (x /~ y) * y + (x % y) = x") {
      forAll { (x: P, y: P) => if(y.terms.length < 6 && x.terms.length < 6 && !y.isZero) (x /~ y) * y + (x % y) shouldBe x }
    }
  }

}

class MultivariatePolynomialTest extends FunSuite {

  test("Multivariate polynomial construction") {
    val m1 = Monomial("3/2d^3b^2c^0a^2")
    val m2 = Monomial("1/2d^2bca")
    val m3 = Monomial("18/9d^5b^0c^2a^3")
    val p = MultivariatePolynomial(m1, m2, m3)
    assert(p.terms === Array(m3, m1, m2))
    assert(p.toString === "2a^3c^2d^5 + 3/2a^2b^2d^3 + 1/2abcd^2")
    assert((-p).toString === "-2a^3c^2d^5 - 3/2a^2b^2d^3 - 1/2abcd^2")
    assert(p === MultivariatePolynomial(m3, m1, m2))
    assert(MultivariatePolynomial.zero[Rational].toString === "0")
    assert(MultivariatePolynomial.one[Rational].toString === "1")
    val l = List(m2, m1 , m3)
    assert(MultivariatePolynomial(l) === p)
    assert(MultivariatePolynomial(p.toString) === p)
  }

  test("Multivariate polynomial construction via string") {
    val p1 = MultivariatePolynomial.parseIntegral[Int]("20Ax^3y - bx^2y^2 - 1000 + c")
    assert(p1.toString === "20Ax^3y - bx^2y^2 + c - 1000")
    val p2 = MultivariatePolynomial.parseIntegral[Long]("1 + 1 + 1 + 0 - 7 + x")
    assert(p2.toString === "x - 4")

    val p3 = MultivariatePolynomial.parseFractional[Double]("-35.1x^2 + 33.333y + 1/2z")
    assert(p3.toString === "-35.1x^2 + 33.333y + 0.5z")

    val p4 = MultivariatePolynomial("-2a^3c^2+2a^3+1/2ac-1/2ad-1/4b^3+3/2b^2d^3")
    assert(((p4 :* 4) /~ Monomial[Rational]('a')).toString === "-8a^2c^2 + 8a^2 - a^-1b^3 + 6a^-1b^2d^3 + 2c - 2d")


    assert(MultivariatePolynomial("-8a^2c^2 + 8a^2 - a^-1b^3 + 6a^-1b^2d^3 + 2c - 2d").toString === "-8a^2c^2 + 8a^2 - a^-1b^3 + 6a^-1b^2d^3 + 2c - 2d")
  }

  test("Multivariate simplification") {
    import spire.implicits._
    val m1 = Monomial(4, ('x', 1), ('y', 2))
    val m2 = Monomial(5, ('x', 1), ('y', 2))
    val p = MultivariatePolynomial(m1, m2)
    assert(p === MultivariatePolynomial(m1 add m2))
    assert(p.toString === "9xy^2")
    assert(MultivariatePolynomial("yxxxy").toString === "x^3y^2")
  }

  test("Multivariate polynomial operations") {
    val p = MultivariatePolynomial("1/4d^2b^3 + 1/2da + 2c^2a^3")
    assert(p.eval(Map('a'->2, 'b'->3, 'c'->2, 'd'->1)) === r"287/4")
    assert(p.isZero === false)
    assert(p.degree === 5)

    val p2 = MultivariatePolynomial("3Ax^2 - y^3z^2 + Bz^4")
    assert(p2.evalPartial(Map('A'->5, 'B'->6)).toString === "15x^2 - y^3z^2 + 6z^4")
    assert(p2.evalPartial(Map('A'->r"1/3", 'z'->2, 'B'->1)).toString === "x^2 - 4y^3 + 16")
  }

  test("Multivariate polynomial arithmetic") {
    val p1 = MultivariatePolynomial("2a^3 + 1/2ac + 3/2b^2d^3")
    val p2 = MultivariatePolynomial("2a^3c^2 + 1/2ad + 1/4b^3")
    assert(p1 + p2 ===
      MultivariatePolynomial("2a^3c^2 + 2a^3 + 1/2ac + 1/2ad + 1/4b^3 + 3/2b^2d^3"))
    assert(p2 + p1 ===
      MultivariatePolynomial("2a^3c^2 + 2a^3 + 1/2ac + 1/2ad + 1/4b^3 + 3/2b^2d^3"))
    assert(p1 - p2 ===
      MultivariatePolynomial("-2a^3c^2 + 2a^3 1/2ac-1/2ad - 1/4b^3 + 3/2b^2d^3"))
    assert(p1 * p2 ===
      MultivariatePolynomial("4a^6c^2 + a^4c^3 + a^4d + 1/2a^3b^3 + 3a^3b^2c^2d^3 + 1/4a^2cd + 1/8ab^3c + 3/4ab^2d^4 + 3/8b^5d^3"))
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
    val p2 = MultivariatePolynomial("x^3 + y^3 + z^3 - 3xyz")
    val p3 = MultivariatePolynomial("xyz")
    assert((p2 /% p1) === (
      MultivariatePolynomial("x^2 - xy - xz + y^2 - yz + z^2") -> MultivariatePolynomial.zero[Rational]))
    assert((p2 /% p3) === (
      MultivariatePolynomial("x^2y^-1z^-1 + x^-1y^2z^-1 + x^-1y^-1z^2 - 3") -> MultivariatePolynomial.zero[Rational]))

    assert(p1 /~ p2 === MultivariatePolynomial.zero[Rational])
    assert((p2 /~ p1) * p1 + (p2 % p1) === p2)
    assert((p1 /~ p2) * p1 + (p1 % p2) === p1)
    assert((MultivariatePolynomial("a^2 + b^2") * MultivariatePolynomial("3x")).toString === "3a^2x + 3b^2x")
  }

}

