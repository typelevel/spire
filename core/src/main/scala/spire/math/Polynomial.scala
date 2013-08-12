package spire.math

import compat._
import scala.annotation.tailrec
import spire.algebra._
import spire.implicits._
import spire.syntax._

/**
 * Polynomial
 * A univariate polynomial class and EuclideanRing extension trait 
 * for arithmetic operations.Polynomials can be instantiated using 
 * any type C for which a Ring[C] and Signed[C] are in scope, with 
 * exponents given by Int values. Some operations require a Field[C]
 * to be in scope.
*/


// Univariate polynomial term
case class Term[C: Signed](coeff: C, exp: Int)(implicit r: Ring[C]) { lhs =>

  def unary_-(): Term[C] = Term(-coeff, exp)

  def +(rhs: Term[C]): Term[C] = {
    if (lhs.exp != rhs.exp)
      throw new IllegalArgumentException(s"can't add terms of degree $exp and $e")
    Term(lhs.coeff + rhs.coeff, lhs.exp)
  }

  def *(rhs: Term[C]): Term[C] =
    Term(lhs.coeff * rhs.coeff, lhs.exp + rhs.exp)

  def toTuple: (Int, C) = (exp, coeff)

  def eval(x: C): C = 
    coeff * (x pow exp)

  def isIndexZero: Boolean = 
    exp == 0

  def isZero: Boolean =
    coeff.signum == 0

  def divideBy(x: C)(implicit f: Field[C]): Term[C] =
    Term(coeff / x, exp)

  def der: Term[C] =
    Term(coeff * r.fromInt(exp), exp - 1)

  def int(implicit f: Field[C]): Term[C] =
    Term(coeff / f.fromInt(exp + 1), exp + 1)

  override def toString = {
    val pos = coeff.signum >= 0
    (coeff, exp) match {
      case (0, _) => ""
      case (c, 0) => if (pos) s" + ${c}" else s" - ${-c}"
      case (1, e) => if (e > 1) s" + x^$e" else s" + x"
      case (-1, e) => if (e > 1) s" - x^$e" else s" - x"
      case (c, 1) => if (pos) s" + ${c}x" else s" - ${-c}x"
      case (c, e) => if (pos) s" + ${c}x^$e" else s" - ${-c}x^$e"
    }
  }
}

object Term {
  def fromTuple[C: Ring : Signed](tpl: (Int, C)): Term[C] = Term(tpl._2, tpl._1)
  def zero[C: Signed](implicit r: Ring[C]): Term[C] = Term(r.zero, 0)
  def one[C: Signed](implicit r: Ring[C]): Term[C] = Term(r.one, 0)
}


// Univariate polynomial class
class Polynomial[C: Signed] private[spire] (val data: Map[Int, C])(implicit r: Ring[C]) extends Function1[C, C] { lhs =>

  override def equals(that: Any): Boolean = that match {
    case p: Polynomial[_] => data == p.data
    case n => terms match {
      case Nil => n == 0
      case Term(c, 0) :: Nil => n == c
      case _ => false
    }
  }

  def terms: List[Term[C]] =
    data.map(Term.fromTuple(_)).toList

  implicit object BigEndianPolynomialOrdering extends Order[Term[C]] {
    def compare(x:Term[C], y:Term[C]): Int = y.exp compare x.exp
  }

  def allTerms: List[Term[C]] = {
    val m = degree
    val cs = new Array[Term[C]](m + 1)
    terms.foreach(t => cs(t.exp) = t)
    for(i <- 0 to m)
      if (cs(i) == null) cs(i) = Term(r.zero, i)
    cs.toList.reverse
  }

  def coeffs: List[C] =
    allTerms.map(_.coeff)

  def maxTerm: Term[C] =
    data.foldLeft(Term.zero[C]) { case (term, (e, c)) =>
      if (term.exp <= e) Term(c, e) else term
    }

  def degree: Int =
    if (isZero) 0 else data.keys.qmax

  def maxOrderTermCoeff: C =
    maxTerm.coeff

  def apply(x: C): C =
    data.view.foldLeft(r.zero)((sum, t) => sum + Term.fromTuple(t).eval(x))

  def isZero: Boolean =
    data.isEmpty

  def unary_-(): Polynomial[C] =
    Polynomial(data.map { case (e, c) => (e, -c) })

  def +(rhs: Polynomial[C]): Polynomial[C] =
    Polynomial(lhs.data + rhs.data)

  def *(rhs: Polynomial[C]): Polynomial[C] =
    Polynomial(lhs.data.view.foldLeft(Map.empty[Int, C]) { case (m, (ex, cx)) =>
      rhs.data.foldLeft(m) { case (m, (ey, cy)) =>
        val e = ex + ey
        val c = cx * cy
        m.updated(e, m.get(e).map(_ + c).getOrElse(c))
      }
    })

  def /%(rhs: Polynomial[C])(implicit f: Field[C]): (Polynomial[C], Polynomial[C]) = {
    require(!rhs.isZero, "Can't divide by polynomial of zero!")

    def zipSum(x: List[C], y: List[C]): List[C] = {
      val (s, l) = if(x.length > y.length) (y, x) else (x, y)
      (s.zip(l).map(z => z._1 + z._2) ++ l.drop(s.length)).tail
    }

    def polyFromCoeffsLE(cs: List[C]): Polynomial[C] =
      Polynomial(cs.zipWithIndex.map({ case (c, e) => Term(c, e) }))

    def polyFromCoeffsBE(cs: List[C]): Polynomial[C] = {
      val ncs = cs.dropWhile(_.signum == 0)
      Polynomial(((ncs.length - 1) to 0 by -1).zip(ncs).map(Term.fromTuple(_)))
    }
            
    @tailrec def eval(q: List[C], u: List[C], n: Int): (Polynomial[C], Polynomial[C]) = {
      lazy val v0 = if (rhs.isZero) r.zero else rhs.maxOrderTermCoeff
      lazy val q0 = u.head / v0
      lazy val uprime = zipSum(u, rhs.coeffs.map(_ * -q0))
      if (u.isEmpty || n < 0) (polyFromCoeffsLE(q), polyFromCoeffsBE(u)) 
        else eval(q0 :: q, uprime, n - 1)
    }

    val ym = rhs.maxTerm
    if (ym.exp == 0) {
      val q = Polynomial(lhs.data.map { case (e, c) => (e, c / ym.coeff) })
      val r = Polynomial(Map.empty[Int, C])
      (q, r)
    } else eval(Nil, lhs.coeffs, lhs.degree - rhs.degree)
  }

  def /~(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._1
    
  def %(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._2

  def monic(implicit f: Field[C]): Polynomial[C] = {
    val m = maxOrderTermCoeff
    new Polynomial(data.map { case (e, c) => (e, c / m) })
  }

  def derivative: Polynomial[C] =
    Polynomial(data.flatMap { case (e, c) =>
      if (e > 0) Some(Term(c, e).der) else None
    })

  def integral(implicit f: Field[C]): Polynomial[C] =
    Polynomial(data.map(t => Term.fromTuple(t).int))

  override def toString =
    if (isZero) {
      "(0)"
    } else {
      val ts = terms.toArray
      QuickSort.sort(ts)
      val s = ts.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }

}


object Polynomial {

  def apply[C: Ring](data: Map[Int, C])(implicit s: Signed[C]): Polynomial[C] =
    new Polynomial(data.filterNot { case (e, c) => s.sign(c) == Sign.Zero })

  def apply[C: Ring : Signed](terms: Iterable[Term[C]]): Polynomial[C] =
    new Polynomial(terms.view.filterNot(_.isZero).map(_.toTuple).toMap)

  def apply[C: Ring : Signed](terms: Traversable[Term[C]]): Polynomial[C] =
    new Polynomial(terms.view.filterNot(_.isZero).map(_.toTuple).toMap)

  def apply[C: Ring](c: C, e: Int)(implicit s: Signed[C]): Polynomial[C] =
    new Polynomial(Map(e -> c).filterNot { case (e, c) => s.sign(c) == Sign.Zero})

  import scala.util.{Try, Success, Failure}

  def apply(s: String): Polynomial[Rational] = parse(s)

  def zero[C: Signed: Ring] = new Polynomial(Map.empty[Int, C])
  def one[C: Signed](implicit r: Ring[C]) = new Polynomial(Map(0 -> r.one))

  def constant[C: Signed: Ring](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(0 -> c))
  def linear[C: Signed: Ring](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(1 -> c))
  def quadratic[C: Signed: Ring](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(2 -> c))
  def cubic[C: Signed: Ring](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(3 -> c))

  implicit def ring[C](implicit a: Ring[C], s: Signed[C]) =
    new PolynomialRing[C] {
      val algebra = a
      val signed = s
    }

  implicit def euclideanRing[C](implicit a: Field[C], s: Signed[C]) =
    new PolynomialEuclideanRing[C] {
      val algebra = a
      val signed = s
    }

  private[this] val termRe = "([0-9]+\\.[0-9]+|[0-9]+/[0-9]+|[0-9]+)?(?:([a-z])(?:\\^([0-9]+))?)?".r

  private[this] val operRe = " *([+-]) *".r

  private[spire] def parse(s: String): Polynomial[Rational] = {

    // represents a term, plus a named variable v
    case class T(c: Rational, v: String, e: Int)

    // parse all the terms and operators out of the string
    @tailrec def parse(s: String, ts: List[T]): List[T] =
      if (s.isEmpty) {
        ts
      } else {
        val (op, s2) = operRe.findPrefixMatchOf(s) match {
          case Some(m) => (m.group(1), s.substring(m.end))
          case None => if (ts.isEmpty) ("+", s) else sys.error(s"parse error: $s")
        }

        val m2 = termRe.findPrefixMatchOf(s2).getOrElse(sys.error("parse error: $s2"))
        val c0 = Option(m2.group(1)).getOrElse("1")
        val c = if (op == "-") "-" + c0 else c0
        val v = Option(m2.group(2)).getOrElse("")
        val e0 = Option(m2.group(3)).getOrElse("")
        val e = if (e0 != "") e0 else if (v == "") "0" else "1"

        val t = try {
          T(Rational(c), v, e.toInt)
        } catch {
          case _: Exception => sys.error(s"parse error: $c $e")
        }
        parse(s2.substring(m2.end), if (t.c == 0) ts else t :: ts)
      }

    // do some pre-processing to remove whitespace/outer parens
    val t = s.trim
    val u = if (t.startsWith("(") && t.endsWith(")")) t.substring(1, t.length - 1) else t

    // parse out the terms
    val ts = parse(u, Nil)

    // make sure we have at most one variable
    val vs = ts.view.map(_.v).toSet.filter(_ != "")
    if (vs.size > 1) sys.error("only univariate polynomials supported")

    // we're done!
    Polynomial(ts.map(t => (t.e, t.c)).toMap)
  }
}

trait PolynomialRing[C] extends Ring[Polynomial[C]] {
  implicit def algebra: Ring[C]
  implicit def signed: Signed[C]

  def zero: Polynomial[C] = Polynomial.zero[C]
  def one: Polynomial[C] = Polynomial.one[C]

  def negate(x: Polynomial[C]): Polynomial[C] = -x
  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x + y
  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x * y
}

trait PolynomialEuclideanRing[C] extends EuclideanRing[Polynomial[C]] with PolynomialRing[C] {
  implicit def algebra: Field[C]

  def quot(x: Polynomial[C], y: Polynomial[C]) = x /~ y
  def mod(x: Polynomial[C], y: Polynomial[C]) = x % y
  override def quotmod(x: Polynomial[C], y: Polynomial[C]) = x /% y

  @tailrec final def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    if (y.isZero && x.isZero) Polynomial(algebra.zero, 0)
    else if (y.maxTerm.isZero) x
    else gcd(y, x % y)
}
