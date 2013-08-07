package spire.math

import compat._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

/*	Polynomial
		A univariate polynomial class and EuclideanRing extension trait 
		for arithmetic operations.Polynomials can be instantiated using 
		any type C, with exponents given by Long values. Arithmetic and 
		many other basic operations require either implicit Ring[C] 
		and/or Field[C]'s in scope.
*/


// 	Univariate polynomial term
case class Term[C](coeff: C, exp: Long) {

  def toTuple: (Long, C) = (exp, coeff)

  def eval(x: C)(implicit r: Ring[C]): C =
    coeff * (x pow exp.intValue)

  def isIndexZero: Boolean = 
    exp == 0L

  def isZero(implicit eq: Eq[C], r: Ring[C]): Boolean =
    coeff === r.zero

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der(implicit r: Ring[C]): Term[C] =
    Term(coeff * r.fromInt(exp.intValue), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt((exp + 1).intValue), exp + 1)

  def termString(implicit o: Order[C], r: Ring[C]) = {
    import r._
    (coeff, exp.intValue) match {
      case (0, _) => ""
      case (c, 0) => if (c >= zero) s" + ${c}" else s" - ${-c}"
      case (1, e) => if (e > 1) s" + x^$e" else s" + x"
      case (-1, e) => if (e > 1) s" - x^$e" else s" - x"
      case (c, 1) => if (c >= zero) s" + ${c}x" else s" - ${-c}x"
      case (c, e) => if (c >= zero) s" + ${c}x^$e" else s" - ${-c}x^$e"
    }
  }
}

object Term {
  def fromTuple[C](tpl: (Long, C)): Term[C] = Term(tpl._2, tpl._1)
  def zero[C](implicit r: Ring[C]): Term[C] = Term(r.zero, 0L)
  def one[C](implicit r: Ring[C]): Term[C] = Term(r.one, 0L)
}


// Univariate polynomial class
case class Polynomial[C: ClassTag](data: Map[Long, C]) {

  def terms: Array[Term[C]] =
    data.map(Term.fromTuple).toArray

  implicit object BigEndianPolynomialOrdering extends Order[Term[C]] {
    def compare(x:Term[C], y:Term[C]): Int = y.exp compare x.exp
  }

  def allTerms(implicit r: Ring[C]): Array[Term[C]] = {
    val m = maxOrder
    val cs = new Array[Term[C]]((m + 1).intValue)
    terms.foreach(t => cs(t.exp.intValue) = t)
    for(i <- 0 to m.intValue)
      if (cs(i) == null) cs(i) = Term(r.zero, i)
    cs.reverse
  }

  def coeffs(implicit r: Ring[C]): Array[C] =
    allTerms.map(_.coeff)

  def maxTerm(implicit r: Ring[C]): Term[C] =
    data.foldLeft(Term.zero[C]) { case (term, (e, c)) =>
      if (term.exp < e) Term(c, e) else term
    }

  def maxOrder(implicit r: Ring[C]): Long =
    if (data.isEmpty) 0 else data.keys.qmax

  def maxOrderTermCoeff(implicit r: Ring[C]): C =
    maxTerm.coeff

  def degree(implicit r: Ring[C], eq: Eq[C]): Long =
    data.foldLeft(0) { case (d, (e, c)) =>
      if (e > d && c =!= r.zero) e.intValue else d
    }
	
  def apply(x: C)(implicit r: Ring[C]): C =
    data.foldLeft(r.zero)((sum, t) => sum + Term.fromTuple(t).eval(x))

  def isZero(implicit r: Ring[C], eq: Eq[C]): Boolean =
    data.forall { case (e, c) => c === r.zero }

  def monic(implicit f: Field[C]): Polynomial[C] = {
    val m = maxOrderTermCoeff
    Polynomial(data.map { case (e, c) => (e, c / m) })
  }
	
  def derivative(implicit r: Ring[C], eq: Eq[C]): Polynomial[C] =
    Polynomial(data.flatMap { case (e, c) =>
      if (e > 0) Some(Term(c, e).der) else None
    })
	
  def integral(implicit f: Field[C], eq: Eq[C]): Polynomial[C] =
    Polynomial(data.map(t => Term.fromTuple(t).int))
	
  def show(implicit o: Order[C], r: Ring[C]) : String =
    if (isZero) {
      "(0)"
    } else {
      val ts = terms
      QuickSort.sort(ts)
      val s = ts.map(_.termString).mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}


object Polynomial {

	/* We have to get rid of coeff=zero terms here for long division
		 operations.
	 	I think we should have an Eq[C] and Ring[C] requirement for Polys.
	*/
  def apply[C: ClassTag](terms: Iterable[Term[C]])
  											(implicit eq: Eq[C], r: Ring[C]): Polynomial[C] =
    Polynomial(terms.filterNot(_.isZero).map(_.toTuple).toMap)

  implicit def pRD: PolynomialRing[Double] = new PolynomialRing[Double] {
    val ct = classTag[Double]
    val r = Ring[Double]
    val o = Order[Double]
    val f = Field[Double]
  }

  implicit def pRR: PolynomialRing[Rational] = new PolynomialRing[Rational] {
    val ct = classTag[Rational]
    val r = Ring[Rational]
    val o = Order[Rational]
    val f = Field[Rational]
  }
}



// Univariate Polynomials Form a EuclideanRing
trait PolynomialRing[C] extends EuclideanRing[Polynomial[C]] {

  implicit def ct: ClassTag[C]
  implicit def r: Ring[C]
  implicit def o: Order[C]
  implicit def f: Field[C]

  implicit def tR: Ring[Term[C]] = new Ring[Term[C]] {
    def negate(t: Term[C]): Term[C] = Term(-t.coeff, t.exp)
    def zero: Term[C] = Term(r.zero, 0L)
    def one: Term[C] = Term(r.one, 0L)
    def plus(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff + y.coeff, y.exp)
    def times(x: Term[C], y: Term[C]): Term[C] =
      Term(x.coeff * y.coeff, x.exp + y.exp)
  }

  def zero = Polynomial(Map(0L -> r.zero))

  def one = Polynomial(Map(0L -> r.one))

  def negate(x: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data.map { case (e, c) => (e, -c) })

  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data + y.data)

  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    Polynomial(x.data.flatMap { case (ex, cx) =>
      y.data.map { case (ey, cy) => (ex + ey, cx * cy) }
    })

  def quotMod(x: Polynomial[C], y: Polynomial[C]): (Polynomial[C], Polynomial[C]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")
      
    def zipSum(x: Array[C], y: Array[C]): Polynomial[C] = {
      val (s, l) = if(x.length > y.length) (y, x) else (x, y)
      val cs = s.zip(l).map(z => z._1 + z._2) ++ l.drop(s.length)
      Polynomial(cs.zip(((cs.length - 1) to 0 by -1)).tail.map {
        case (c, e) => Term(c, e)
      })
    }

    def polyFromCoeffsLE(cs: Iterable[C]): Polynomial[C] =
      Polynomial(cs.zipWithIndex.map { case (c, e) => Term(c, e) })
      
    @tailrec def eval(q: List[C], u: Polynomial[C], n: Long): (Polynomial[C], Polynomial[C]) = {
      lazy val q0 = u.maxOrderTermCoeff / y.maxOrderTermCoeff
      lazy val uprime = zipSum(u.coeffs, y.coeffs.map(_ * -q0))
      if (u.isZero || n < 0) (polyFromCoeffsLE(q), u) else eval(q0 :: q, uprime, n - 1)
    }
      
    eval(Nil, x, x.degree - y.degree)
  }

  def quot(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = quotMod(x, y)._1
    
  def mod(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = quotMod(x, y)._2

  @tailrec final def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    if (y.isZero && x.isZero) zero
    else if (y.maxTerm.isZero) x
    else gcd(y, mod(x, y))

}
