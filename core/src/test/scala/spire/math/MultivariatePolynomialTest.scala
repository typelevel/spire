package spire.math

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
    c <- Gen.choose('x', 'z')
    v <- Gen.choose(1, 10)
  } yield (c, v))

}

class MonomialCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  import MonomialSetup._

  implicit val er = Eq[Rational]
  implicit val fr = Field[Rational]
  implicit val cr = implicitly[ClassTag[Rational]]

  runMonomialCheck[Rational]("rational")

  def runMonomialCheck[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[Monomial[A]] = Arbitrary(for {
      coeff <- arbitrary[A]
      vs <- arbitrary[List[(Char,Int)]]
    } yield Monomial(coeff, vs) )
    runTest[A](s"$typ/monomial")
  }

  def runTest[A: Eq: Field: ClassTag](name: String)(implicit arb: Arbitrary[Monomial[A]]) {
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
    val m = Monomial(r"3/2",'d' -> 3, 'b' -> 2, 'c' -> 0, 'a' -> 2)
    assert(m.vars === Map('a' -> 2, 'b' -> 2, 'd' -> 3))
    assert(m.toString === " + 3/2a^2b^2d^3")
    assert((-m).toString === " - 3/2a^2b^2d^3")
    assert(m === Monomial(r"3/2",'c' -> 0, 'b' -> 2, 'd' -> 3, 'a' -> 2))
    assert(Monomial.zero[Rational].toString === " + (0)")
    assert(Monomial.one[Rational].toString === " + 1")
    assert(Monomial.xzero(r"22/7").toString === " + 22/7")
  }

  test("Monomial basic operations") {
    val m = Monomial(r"5/6", 'x' -> 2, 'y' -> 1, 'z' -> 0)
    assert(m.eval(Map('x' -> r"2", 'y' -> r"3", 'z' -> r"2")) === r"10")
    assert(m.isZero === false)
    assert((-m).toString === " - 5/6x^2y")
    assert(m.degree === 3)
  }

  test("Monomial arithmetic") {
    val m1 = Monomial(r"16/17", 'x' -> 1, 'y' -> 4, 'z' -> 2)
    val m2 = Monomial(r"3/7", 'x' -> 3, 'z' -> 2)
    assert(m1 + Monomial(r"1/17", 'x' -> 1, 'y' -> 4, 'z' -> 2) === Monomial(r"1", 'x' -> 1, 'y' -> 4, 'z' -> 2))
    assert(m2 + m2 === Monomial(r"6/7", 'x' -> 3, 'z' -> 2))
    assert(m1 - Monomial(r"1", 'x' -> 1, 'y' -> 4, 'z' -> 2) === Monomial(r"-1/17", 'x' -> 1, 'y' -> 4, 'z' -> 2))
    assert(m1 * m2 === Monomial(r"48/119", 'x' -> 4, 'y' -> 4, 'z' -> 4))
    assert(m1 :* r"3/4" === Monomial(r"12/17", 'x' -> 1, 'y' -> 4, 'z' -> 2))
    assert(m2 :/ r"1/2" === Monomial(r"6/7", 'x' -> 3, 'z' -> 2))
    assert(-m1 + m1 === Monomial.zero[Rational])
    assert(m1 * Monomial.one[Rational] === m1)
    assert(m1 / Monomial.one[Rational] === m1)
    assert(m2 * Monomial.zero[Rational] === Monomial.zero[Rational])
  }

  test("Monomial GCD / LCM functions") {
    val m1 = Monomial(r"15/17", 'x' -> 2, 'y' -> 4, 'z' -> 2)
    val m2 = Monomial(r"4/7", 'x' -> 3, 'z' -> 1)
    assert(m1.gcd(m2) === Monomial(r"1/119", 'x' -> 2, 'z' -> 1))
    assert(m2.gcd(Monomial.one[Rational]) === Monomial(r"1/7", 'x' -> 0))
    assert(m2.lcm(m1) === Monomial(r"60", 'x' -> 3, 'z' -> 2))
  }

}

class MultivariatePolynomialCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  import MonomialSetup._

  implicit val er = Eq[Rational]
  implicit val fr = Field[Rational]
  implicit val cr = implicitly[ClassTag[Rational]]

  runMVPCheck[Rational]("rational")

  implicit def mono[A: Arbitrary: Eq: Field: ClassTag] = Arbitrary(for {
    coeff <- arbitrary[A]
    vs <- arbitrary[List[(Char,Int)]]
  } yield Monomial(coeff, vs) )

  def runMVPCheck[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[MultivariatePolynomial[A]] = Arbitrary(for {
      m <- arbitrary[Array[Monomial[A]]]
    } yield MultivariatePolynomial(m) )
    runTest[A](s"$typ/lexicographic")
  }

  def runTest[A: Eq: Field: ClassTag](name: String)(implicit arb: Arbitrary[MultivariatePolynomial[A]]) {
    type P = MultivariatePolynomial[A]

    val zero = MultivariatePolynomial.zero[A]
    val one = MultivariatePolynomial.one[A]

    property(s"$name p = p") {
      forAll { (p: P) => 
        println(s"mvp = $p");
        p should be === p }
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
      forAll { (x: P, y: P) => x + y should be === y + x }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: P, y: P) => x * y should be === y * x }
    }

    property(s"$name (x /~ y) * y + (x % y) = x") {
      forAll { (x: P, y: P) => if (!y.isZero) (x /~ y) * y + (x % y) should be === x }
    }  
  }

}
