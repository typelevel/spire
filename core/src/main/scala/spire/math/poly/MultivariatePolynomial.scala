package spire.math.poly

import compat._
import scala.annotation.tailrec
import scala.reflect._
import scala.{specialized => spec}
import spire.algebra._
import spire.implicits._
import spire.math._


trait LexOrdering {
  implicit def lexOrdering[C] = new MonomialOrderingLex[C] {
    val ordInt = Order[Int]
    val ordChar = Order[Char]
  }
}

trait GlexOrdering {
  implicit def glexOrdering[C] = new MonomialOrderingGlex[C] {    
    val ordInt = Order[Int]
    val ordChar = Order[Char]
  }
}

trait GrevlexOrdering {
  implicit def grevlexOrdering[C] = new MonomialOrderingGrevlex[C] {
    val ordInt = Order[Int]
    val ordChar = Order[Char]
  }
}

case class MultivariatePolynomial[@spec(Double) C](val terms: Array[Monomial[C]])
  (implicit val ct: ClassTag[C], ord: Order[Monomial[C]]) { lhs =>

  def isZero(implicit r: Semiring[C], eq: Eq[C]): Boolean = 
    terms.forall(_.isZero)

  def isEmpty: Boolean =
    terms.isEmpty

  def degree: Int = 
    terms.map(_.degree).max

  def ts(implicit r: Semiring[C]): List[Monomial[C]] = {
    val sts = simplify(terms)
    sts.qsort
    sts.toList
  }

  def allVariables: Array[Char] =
    terms.flatMap(t => t.vars.keys).distinct

  def eval(values: Map[Char, C])(implicit r: Ring[C]): C = {
    require(allVariables.forall(values.contains), "Can't evaluate polynomial without all the variable (symbol) values!")
    terms.map(_.eval(values)).reduce(_ + _)
  }

  def unary_-(implicit r: Rng[C]): MultivariatePolynomial[C] = 
    new MultivariatePolynomial[C](terms.map(_.unary_-))

  def head(implicit r: Semiring[C], eq: Eq[C]): Monomial[C] =
    if(isZero) Monomial(r.zero, Map[Char, Int]()) else ts.head

  def headCoefficient(implicit r: Semiring[C], eq: Eq[C]): C =
    head.coeff

  def tail(implicit r: Semiring[C]): MultivariatePolynomial[C] = 
    new MultivariatePolynomial[C](ts.tail.toArray)

  def monic(implicit f: Field[C], eq: Eq[C]): MultivariatePolynomial[C] =
    if(isZero) new MultivariatePolynomial[C](new Array[Monomial[C]](0)) else 
      new MultivariatePolynomial[C](terms.map(_.:/(headCoefficient)))

  def simplify(xs: Array[Monomial[C]])(implicit r: Semiring[C], eq: Eq[Monomial[C]]): Array[Monomial[C]] = 
   (for(x <- xs) yield xs.view.filter(_ === x).reduce(_ + _)).distinct.filterNot(_.coeff == r.zero)

  def simplifySub(xs: Array[Monomial[C]])(implicit r: Rng[C], eq: Eq[Monomial[C]]): Array[Monomial[C]] = 
   (for(x <- xs) yield xs.view.filter(_ === x).reduce(_ - _)).distinct.filterNot(_.coeff == r.zero)

  // EuclideanRing ops
  def +(rhs: MultivariatePolynomial[C])(implicit r: Semiring[C], eq: Eq[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](simplify(lhs.terms ++ rhs.terms))

  def -(rhs: MultivariatePolynomial[C])(implicit r: Rng[C], eq: Eq[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](simplifySub(lhs.terms ++ rhs.terms))

  def *(rhs: MultivariatePolynomial[C])(implicit r: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](simplify(lhs.terms.flatMap(lt => rhs.terms.map(rt => lt * rt))))

  def /~(rhs: MultivariatePolynomial[C])(implicit f: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = 
    lhs./%(rhs)._2

  def %(rhs: MultivariatePolynomial[C])(implicit f: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = 
    lhs./%(rhs)._1

  def /%(rhs: MultivariatePolynomial[C])(implicit f: Field[C], eq: Eq[C], ct: ClassTag[C]) = {
    @tailrec def quotMod_(quot: MultivariatePolynomial[C],
                          dividend: MultivariatePolynomial[C],
                          divisor: MultivariatePolynomial[C]): (MultivariatePolynomial[C], MultivariatePolynomial[C]) = {
      if(divisor.isEmpty) (quot, dividend) else { // if we can't divide anything in, give it back the quot and dividend
        if(divisor.head.divides(dividend.head)) {
          val divTerm = new MultivariatePolynomial[C](Array(dividend.head / divisor.head)) // the first division
          val prod = divisor.tail * divTerm // then multiply the rhs.tail by the MVP containing only this product.
          val quotSum = quot + divTerm // expand the quotient with the divided term
          val rem = dividend.tail - prod // then subtract from the original dividend tail
          if(rem.isZero) (quotSum, rem) else quotMod_(quotSum, rem, divisor) // repeat
        } else quotMod_(quot, dividend, divisor.tail)
      }
    }
    quotMod_(new MultivariatePolynomial[C](Array(Monomial(f.zero, 'x' -> 0))), lhs, rhs)
  }

  // VectorSpace ops
  def *:(k: C)(implicit r: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = 
    if(k === r.zero) new MultivariatePolynomial[C](new Array[Monomial[C]](0)) else new MultivariatePolynomial[C](terms.map(_.*:(k)))

  def :*(k: C)(implicit r: Semiring[C], eq: Eq[C]): MultivariatePolynomial[C] = k *: lhs

  def :/ (k: C)(implicit f: Field[C], eq: Eq[C]): MultivariatePolynomial[C] = lhs.*:(k.reciprocal)

  override def toString =
    if (isEmpty) {
      "(0)"
    } else {
      QuickSort.sort(terms)(ord, implicitly[ClassTag[Monomial[C]]])
      val s = terms.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }

}


object MultivariatePolynomialLex extends LexOrdering {
  
  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](terms.toArray)

  def apply[@spec(Double) C: ClassTag](terms: List[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](terms.toArray)

}

object MultivariatePolynomialGlex extends GlexOrdering {

  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](terms.toArray)

  def apply[@spec(Double) C: ClassTag](terms: List[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](terms.toArray)

}

object MultivariatePolynomialGrevlex extends GrevlexOrdering {

  def apply[@spec(Double) C: ClassTag](terms: Monomial[C]*): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](terms.toArray)

  def apply[@spec(Double) C: ClassTag](terms: List[Monomial[C]]): MultivariatePolynomial[C] =
    new MultivariatePolynomial[C](terms.toArray)
     
}


