package spire.math.poly

import compat._
import scala.reflect._
import scala.{specialized => spec}
import spire.algebra._
import spire.implicits._
import spire.math._


class MultivariatePolynomial[@spec(Double) C] private[spire] (val terms: Array[Monomial[C]], ord: Order[Monomial[C]])
(implicit val ct: ClassTag[C]) { lhs =>

  def isZero: Boolean = terms.isEmpty

  def apply(vars: Array[C])(implicit ring: Semiring[C]): C = {
    var sum = ring.zero
    cfor(0)(_ < terms.length, _ + 1) { i => sum += terms(i).eval(vars) }
    sum
  }

  def unary_-()(implicit ring: Rng[C]): MultivariatePolynomial[C] =
    new MultivariatePolynomial(terms.map(_.unary_-), ord)

  def +(rhs: MultivariatePolynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = ???
  def -(rhs: MultivariatePolynomial[C])(implicit ring: Rng[C], eq: Eq[C]): MultivariatePolynomial[C] = lhs + (-rhs)
  def *(rhs: MultivariatePolynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = ???
  def /~(rhs: MultivariatePolynomial[C])(implicit field: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = (lhs /% rhs)._1
  def /%(rhs: MultivariatePolynomial[C])(implicit field: Field[C], eq: Eq[C]): (MultivariatePolynomial[C], MultivariatePolynomial[C]) = ???
  def %(rhs: MultivariatePolynomial[C])(implicit field: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = (lhs /% rhs)._2

  def *: (k: C)(implicit ring: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = ???
  def :* (k: C)(implicit ring: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = ???
  def :/ (k: C)(implicit field: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = ???

  override def toString = {
    if (isZero) "(0)" else {
      QuickSort.sort(terms)(ord, implicitly[ClassTag[Monomial[C]]])
      val s = terms.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
  }

}

object MultivariatePolynomial extends MultivariatePolynomialInstances {

  def apply[C: Semiring: Eq: ClassTag](terms: List[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial(terms.toArray, new MonomialOrderingLex[C]{})

  def zero[@spec(Double) C: Eq: Semiring: ClassTag]: MultivariatePolynomial[C] = ???

  def one[@spec(Double) C: Eq: Semiring: ClassTag]: MultivariatePolynomial[C] = ???

}

trait MVPolynomialEq[@spec(Double) C] 
extends Eq[MultivariatePolynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def eqv(x: MultivariatePolynomial[C], y: MultivariatePolynomial[C]): Boolean = ???
}

trait MVPolynomialSemiring[@spec(Double) C] 
extends Semiring[MultivariatePolynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def zero: MultivariatePolynomial[C] = MultivariatePolynomial.zero[C]
  def plus(x: MultivariatePolynomial[C], y: MultivariatePolynomial[C]): MultivariatePolynomial[C] = x + y
  def times(x: MultivariatePolynomial[C], y: MultivariatePolynomial[C]): MultivariatePolynomial[C] = x * y
}

trait MVPolynomialRig[@spec(Double) C] extends MVPolynomialSemiring[C]
with Rig[MultivariatePolynomial[C]] {
  implicit override val scalar: Rig[C]

  def one: MultivariatePolynomial[C] = MultivariatePolynomial.one[C]
}

trait MVPolynomialRng[@spec(Double) C] extends MVPolynomialSemiring[C]
with RingAlgebra[MultivariatePolynomial[C], C] {
  implicit override val scalar: Rng[C]

  def timesl(r: C, v: MultivariatePolynomial[C]): MultivariatePolynomial[C] = r *: v
  def negate(x: MultivariatePolynomial[C]): MultivariatePolynomial[C] = -x
}

trait MVPolynomialRing[@spec(Double) C] extends MVPolynomialRng[C]
with Ring[MultivariatePolynomial[C]] {
  implicit override val scalar: Ring[C]

  def one: MultivariatePolynomial[C] = MultivariatePolynomial.one[C]
}

trait MVPolynomialEuclideanRing[@spec(Double) C] extends MVPolynomialRing[C]
with EuclideanRing[MultivariatePolynomial[C]] with VectorSpace[MultivariatePolynomial[C], C] {
  implicit override val scalar: Field[C]

  override def divr(x: MultivariatePolynomial[C], k: C): MultivariatePolynomial[C] = x :/ k
  def quot(x: MultivariatePolynomial[C], y: MultivariatePolynomial[C]) = x /~ y
  def mod(x: MultivariatePolynomial[C], y: MultivariatePolynomial[C]) = x % y
  override def quotmod(x: MultivariatePolynomial[C], y: MultivariatePolynomial[C]) = x /% y

  final def gcd(x: MultivariatePolynomial[C], y: MultivariatePolynomial[C]): MultivariatePolynomial[C] = ???
}


trait MVPolyInstances0 {
  implicit def semiring[@spec(Double) C: ClassTag: Semiring: Eq] =
    new MVPolynomialSemiring[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait MVPolyInstances1 extends MVPolyInstances0 {
  implicit def rig[@spec(Double) C: ClassTag: Rig: Eq] =
    new MVPolynomialRig[C] {
      val scalar = Rig[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def rng[@spec(Double) C: ClassTag: Rng: Eq] =
    new MVPolynomialRng[C] {
      val scalar = Rng[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait MVPolyInstances2 extends MVPolyInstances1 {
  implicit def ring[@spec(Double) C: ClassTag: Ring: Eq] =
    new MVPolynomialRing[C] {
      val scalar = Ring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait MVPolyInstances3 extends MVPolyInstances2 {
  implicit def euclideanRing[@spec(Double) C: ClassTag: Field: Eq] =
    new MVPolynomialEuclideanRing[C] {
      val scalar = Field[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait MultivariatePolynomialInstances extends MVPolyInstances3


// Lexicographic ordering
// e.g. x^2 < xy < xz < x < y^2 < yz < y < z^2 < z < 1
trait MonomialOrderingLex[@spec(Float, Double) C] extends Order[Monomial[C]] {

  def compare(x: Monomial[C], y: Monomial[C]): Int =
    (x.exps.isEmpty, y.exps.isEmpty) match {
      case (true, true) => 0
      case (false, true) => -1
      case (true, false) => 1
      case (false, false) => x.firstNonZeroVarIndex() compare y.firstNonZeroVarIndex() match {
        case -1 => -1
        case 1 => 1
        case 0 => x.firstNonZeroVarExp(x.exps) compare y.firstNonZeroVarExp(y.exps) match {
          case -1 => 1
          case 1 => -1
          case 0 => compare(x.monomialTail, y.monomialTail)
        }
      }
    }

}

// Graded lexicograpic ordering
// e.g. x^2 < xy < xz < y^2 < yz < z^2 < x < y < z < 1
trait MonomialOrderingGlex[@spec(Float, Double) C] extends Order[Monomial[C]] {

  def compare(x: Monomial[C], y: Monomial[C]): Int = ???

}

// Graded reverse lexicographic ordering
// e.g. x^2 < xy < y^2 < xz < yz < z^2 < x < y < z < 1
trait MonomialOrderingGrevlex[@spec(Float, Double) C] extends Order[Monomial[C]] {

  def compare(x: Monomial[C], y: Monomial[C]): Int = ???

}

