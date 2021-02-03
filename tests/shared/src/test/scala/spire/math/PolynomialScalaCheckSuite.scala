package spire
package math

import spire.algebra._
import spire.math.poly._
import spire.std.bigDecimal._
import spire.std.bigInt._
import spire.syntax.euclideanRing._
import spire.syntax.literals._
import spire.optional.rationalTrig._

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

import org.scalacheck.Prop._
import org.scalacheck.Prop

class PolynomialScalaCheckSuite extends munit.ScalaCheckSuite {

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

  def runTest[A: Eq: Field: ClassTag](name: String)(implicit arb: Arbitrary[Polynomial[A]]): Unit = {
    type P = Polynomial[A]

    val zero = Polynomial.zero[A]
    val one = Polynomial.one[A]

    property(s"$name p = p") {
      forAll { (p: P) => p == p }
    }

    property(s"$name p + 0 = p") {
      forAll { (p: P) => p + zero == p }
    }

    property(s"$name p + (-p) = 0") {
      forAll { (p: P) => p + (-p) == zero }
    }

    property(s"$name p * 0 = 0") {
      forAll { (p: P) => p * zero == zero }
    }

    property(s"$name p * 1 = p") {
      forAll { (p: P) => p * one == p }
    }

    property(s"$name p /~ 1 = p") {
      forAll { (p: P) => (p.equot(one)) == p }
    }

    property(s"$name p /~ p = 1") {
      forAll { (p: P) => if (!p.isZero)(p.equot(p)) == one else true }
    }

    property(s"$name p % p = 0") {
      forAll { (p: P) => if (!p.isZero)(p.emod(p)) == zero else true }
    }

    property(s"$name x + y = y + x") {
      forAll { (x: P, y: P) => x + y == y + x }
    }

    property(s"$name x * y = y * x") {
      forAll { (x: P, y: P) => x * y == y * x }
    }

    property(s"$name (x /~ y) * y + (x % y) = x") {
      forAll { (x: P, y: P) => if (!y.isZero)(x.equot(y)) * y + (x.emod(y)) == x else true }
    }

    property(s"$name p = p.reductum + p.maxTerm") {
      forAll { (p: P) =>
        p == p.reductum + Polynomial(p.maxTerm :: Nil)
      }
    }
  }

  property("(x compose y)(z) == x(y(z))") {
    forAll { (rs1: List[Rational], rs2: List[Rational], r: Rational) =>
      def xyz(rs: List[Rational]): Polynomial[Rational] =
        Polynomial(rs.take(4).zipWithIndex.map { case (c, e) => Term(c, e) })

      val (p1, p2) = (xyz(rs1), xyz(rs2))
      val p3 = p1.compose(p2)
      p3(r) == p1(p2(r))
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
      t.toTuple == ((t.exp, t.coeff))
      t.isIndexZero == (t.exp == 0)
      forAll { (x: Rational) =>
        t.eval(x) == t.coeff * x.pow(t.exp.toInt) &&
        t.isZero == (t.coeff == 0) &&
        (if (t.exp > 0) t.der.int == t else true) &&
        t.int.der == t
      }
    }
  }

  property("sparse p = p") {
    forAll { (p: PolySparse[Rational]) =>
      val d = p.toDense
      p == p &&
      p == d &&
      p.## == d.##
    }
  }

  property("dense p = p") {
    forAll { (p: PolyDense[Rational]) =>
      val s = p.toSparse
      p == p &&
      p == s &&
      p.## == s.##
    }
  }

  property("p.toSparse.toDense = p") {
    forAll { (p: PolyDense[Rational]) =>
      p.toSparse.toDense == p
    }
  }

  property("p.toDense.toSparse = p") {
    forAll { (p: PolySparse[Rational]) =>
      p.toDense.toSparse == p
    }
  }

  property("apply(p.toString).toDense = p") {
    forAll { (p: PolySparse[Rational]) =>
      Polynomial(p.toString).toDense == p
    }
  }

  property("apply(p.toString) = p") {
    forAll { (p: PolyDense[Rational]) =>
      Polynomial(p.toString) == p
    }
  }

  property("apply(r, 0) = r") {
    forAll { (r: Rational) =>
      val p = Polynomial(r, 0)
      p == r &&
      p.## == r.##
    }
  }

  property(s"p.shift(h) = p.compose(x + h)") {
    forAll { (p: Polynomial[BigInt], h: BigInt) =>
      p.shift(h) == p.compose(Polynomial.x[BigInt] + Polynomial.constant(h))
    }
  }

  def gcdTest(x: Polynomial[Rational], y: Polynomial[Rational]): Prop = {
    (!x.isZero || !y.isZero) ==> {
      val gcd = spire.math.gcd[Polynomial[Rational]](x, y)
      if (!gcd.isZero) {
        (x.emod(gcd)) == 0 &&
        (y.emod(gcd)) == 0
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
        e <- arbitrary[Int].map { n => (n % 10).abs }
      } yield (e, c))
    } yield {
      Polynomial(ts.toMap).toDense
    })

    forAll { (x: Polynomial[Rational], y: Polynomial[Rational]) =>
      gcdTest(x, y)
    }
  }
}
