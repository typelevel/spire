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

  def isZero: Boolean = 
    terms.isEmpty

  def degree: Int = 
    terms.map(_.degree).max

  def unary_-(implicit r: Rng[C]): MultivariatePolynomial[C] = 
    new MultivariatePolynomial(terms.map(_.unary_-))

  def simplify(ts: Array[Monomial[C]])(implicit r: Semiring[C], eq: Eq[Monomial[C]]): Array[Monomial[C]] = 
   (for(t <- ts) yield ts.view.filter(_ === t).reduce(_ + _)).distinct.filterNot(_.coeff == r.zero)

  // EuclideanRing ops
  def +(rhs: MultivariatePolynomial[C])(implicit r: Semiring[C], eq: Eq[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial(simplify(lhs.terms ++ rhs.terms))

  def -(rhs: MultivariatePolynomial[C])(implicit r: Rng[C], eq: Eq[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial(simplify(lhs.terms ++ rhs.terms))

  def *(rhs: MultivariatePolynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] =
    new MultivariatePolynomial(simplify(lhs.terms.flatMap(l => rhs.terms.map(r => l * r))))

  def /~(rhs: MultivariatePolynomial[C])(implicit f: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = ???
  def /%(rhs: MultivariatePolynomial[C])(implicit f: Field[C], eq: Eq[C]): (MultivariatePolynomial[C], MultivariatePolynomial[C]) = ???
  def %(rhs: MultivariatePolynomial[C])(implicit f: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = ???


  // VectorSpace ops
  def *:(k: C)(implicit r: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = 
    if(k === r.zero) new MultivariatePolynomial(new Array[Monomial[C]](0)) else new MultivariatePolynomial(terms.map(_.*:(k)))

  def :*(k: C)(implicit r: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = k *: lhs

  def :/ (k: C)(implicit f: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = lhs.*:(k.reciprocal)

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
  
  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] =
    new MultivariatePolynomial(terms.toArray)

}

object MultivariatePolynomialGlex extends GlexOrdering {

  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] =
    new MultivariatePolynomial(terms.toArray)

}

object MultivariatePolynomialGrevlex extends GrevlexOrdering {

  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] =
    new MultivariatePolynomial(terms.toArray)
     
}


