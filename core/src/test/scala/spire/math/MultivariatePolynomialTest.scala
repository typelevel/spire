package spire.math

import spire.math.poly._
import spire.algebra._
import spire.math.poly._
import spire.std.bigDecimal._
import spire.syntax.literals._
import spire.syntax.euclideanRing._
import spire.optional.rationalTrig._

import scala.reflect.ClassTag

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary


object MVPolynomialSetup {
  implicit val arbitraryRational = Arbitrary(for {
    n0 <- arbitrary[Long]
    d0 <- arbitrary[Long]
  } yield {
    val (n, d) = (n0 % 100, d0 % 100)
    if (d == 0L) Rational(n, 1L) else Rational(n, d)
  })

  case class Variable(val v: (Char, Int))

  implicit def arbitraryVariable = Arbitrary(for {
    i0 <- Gen.choose(0, 100) //arbitrary[Int]
    v0 <- Gen.choose('a','z') //arbitrary[Char]
  } yield {
    Variable((v0, i0)) //i0.abs))
  })

  implicit def arbitraryMonomial[A: Arbitrary: Ring: Eq: ClassTag] = Arbitrary(for {
    coeff <- arbitrary[A]
    count <- arbitrary[Int]
    vs <- arbitrary[List[Variable]];
    if(vs.length < 5) // we only need short variables - this is about algebraic tests after all - can expand later on.
  } yield {
    Monomial[A](coeff, vs.map(_.v))
  })
}

class MultivariatePolynomialCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  import MVPolynomialSetup._

  val ebd = Eq[Rational]
  val fbd = Field[Rational]
  val cbd = implicitly[ClassTag[Rational]]

  runMonomial[Rational]("rational")
  // runLex[Rational]("rational")
  // runGlex[Rational]("rational")
  // runGrevlex[Rational]("rational")

  // implicit val arbLex: Arbitrary[MultivariatePolynomial[Rational]] = Arbitrary(for {
  //   ts <- arbitrary[List[Monomial[Rational]]]
  // } yield {
  //   MultivariatePolynomialLex(ts)
  // })

  def runMonomial[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[MultivariatePolynomial[A]] = Arbitrary(for {
      m <- arbitrary[Monomial[A]]
    } yield { 
      m
    })
    runMonomialTest[A](s"$typ/monomial")
  }

  def runLex[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[MultivariatePolynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Monomial[A]]];
      if(ts.length <= 5)
    } yield {
      MultivariatePolynomialLex(ts)
    })
    runTest[A](s"$typ/lexicographic")
  }

  def runGlex[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[MultivariatePolynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Monomial[A]]]
    } yield {
      MultivariatePolynomialGlex(ts)
    })
    runTest[A](s"$typ/graded lexicographic")
  }

  def runGrevlex[A: Arbitrary: Eq: Field: ClassTag](typ: String) {
    implicit val arb: Arbitrary[MultivariatePolynomial[A]] = Arbitrary(for {
      ts <- arbitrary[List[Monomial[A]]]
    } yield {
      MultivariatePolynomialGrevlex(ts)
    })
    runTest[A](s"$typ/graded reverse lexicographic")
  }

  def runMonomialTest[A: Eq: ClassTag](name: String)(implicit arb: Arbitrary[MultivariatePolynomial[A]], f: Field[A]) {
    type M = Monomial[A]

    val zero = Monomial.zero[A]
    val one = Monomial.one[A]

    property(s"$name p = p") {
      forAll { (m: M) => m should be === m }
    }

    property(s"$name p + 0 = p") {
      forAll { (m: M) => m + zero should be === m }
    }

    property(s"$name p + (-p) = 0") {
      forAll { (m: m) => m + (-m) should be === zero }
    }

    property(s"$name p * 0 = 0") {
      forAll { (m: M) => m * zero should be === zero }
    }

    property(s"$name p * 1 = p") {
      forAll { (m: M) => m * one should be === m }
    }

    property(s"$name x + y = y + x") {
      forAll { (x: m, y: m) => x + y should be === y + x }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: m, y: m) => x * y should be === y * x }
    }

  }

  def runTest[A: Eq: ClassTag](name: String)(implicit arb: Arbitrary[Monomial[A]], f: Field[A]) {
    type P = MultivariatePolynomial[A]

    val zero = MultivariatePolynomialLex.zero[A]
    val one = MultivariatePolynomialLex.one[A]

    property(s"$name p = p") {
      forAll { (p: P) => p should be === p }
    }

    property(s"$name p + 0 = p") {
      forAll { (p: P) => p + zero should be === p }
    }

    property(s"$name p + (-p) = 0") {
      forAll { (p: P) => p + (-p) should be === zero }
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






