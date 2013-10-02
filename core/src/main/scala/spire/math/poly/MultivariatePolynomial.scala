package spire.math.poly

import compat._
import scala.annotation.tailrec
import scala.reflect._
import scala.{specialized => spec}
import spire.algebra._
import spire.implicits._
import spire.math._


trait LexOrdering {
  implicit def lexOrdering[C] = new MonomialOrderingLex[C] {}
}
trait GlexOrdering {
  implicit def glexOrdering[C] = new MonomialOrderingGlex[C] {}
}
trait GrevlexOrdering {
  implicit def grevlexOrdering[C] = new MonomialOrderingGrevlex[C] {}
}

case class MultivariatePolynomial[@spec(Double) C](val terms: Array[Monomial[C]])
  (implicit val ct: ClassTag[C], ord: Order[Monomial[C]]) { lhs =>

  def +(rhs: MultivariatePolynomial[C])(implicit r: Semiring[C], eq: Eq[Monomial[C]]): MultivariatePolynomial[C] = ???

  def -(rhs: MultivariatePolynomial[C])(implicit ring: Rng[C], eq: Eq[Monomial[C]]): MultivariatePolynomial[C] = ???

  def isZero: Boolean = terms.isEmpty

  override def toString =
    if (isZero) {
      "(0)"
    } else {
      terms.qsort
      val s = terms.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }

}

object MultivariatePolynomialLex extends LexOrdering {

  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] = {
    new MultivariatePolynomial(terms.toArray)
  }

}

object MultivariatePolynomialGlex extends GlexOrdering {

  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] = {
    new MultivariatePolynomial(terms.toArray)
  }

}

object MultivariatePolynomialGrevlex extends GrevlexOrdering {

  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] = {
    new MultivariatePolynomial(terms.toArray)
  }

}