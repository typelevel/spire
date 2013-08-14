package spire.math

import compat._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}


/**
 * Polynomial
 * A univariate polynomial class and EuclideanRing extension trait 
 * for arithmetic operations.Polynomials can be instantiated using 
 * any type C for which a Ring[C] and Signed[C] are in scope, with 
 * exponents given by Int values. Some operations require a Field[C]
 * to be in scope.
*/


// Univariate polynomial term
case class Term[@spec(Double) C: ClassTag](coeff: C, exp: Int)(implicit r: Ring[C], s: Signed[C]) { lhs =>

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

  def fromTuple[@spec(Double) C: ClassTag](tpl: (Int, C))(implicit r: Ring[C], s: Signed[C]): Term[C] = 
    Term(tpl._2, tpl._1)
  def zero[@spec(Double) C: ClassTag](implicit r: Ring[C], s: Signed[C]): Term[C] = 
    Term(r.zero, 0)
  def one[@spec(Double) C: ClassTag](implicit r: Ring[C], s: Signed[C]): Term[C] = 
    Term(r.one, 0)

}


// Sparse polynomials - Big Endian Coeffs e.g. x^n, x^n-1, ... x^0
class PolySparse[@spec(Double) C] private[spire] (val data: Map[Int, C])
  (implicit r: Ring[C], s: Signed[C], val ct: ClassTag[C]) extends Function1[C, C] with Polynomial[C] { lhs =>

  def toDense: PolyDense[C] = new PolyDense(coeffs.reverse)
  def toSparse: PolySparse[C] = lhs

  def allTerms: List[Term[C]] = {
    val m = degree
    val cs = new Array[Term[C]](m + 1)
    terms.foreach(t => cs(t.exp) = t)
    for(i <- 0 to m)
      if (cs(i) == null) cs(i) = Term(r.zero, i)
    cs.toList.reverse
  }

  def terms: List[Term[C]] =
    data.map(Term.fromTuple(_)).toList

  def coeffs: Array[C] = {
    allTerms.map(_.coeff).toArray
  }

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

    def zipSum(x: Array[C], y: Array[C]): Array[C] =
      (x + y).tail

    def polyFromCoeffsLE(cs: Array[C]): Polynomial[C] = {
      val ncs: Array[Term[C]] = cs.zipWithIndex.map({ case (c, e) => Term(c, e) })
      Polynomial(ncs)
    }

    def polyFromCoeffsBE(cs: Array[C]): Polynomial[C] = {
      val ncs = cs.dropWhile(_.signum == 0)
      Polynomial(((ncs.length - 1) to 0 by -1).zip(ncs).map(Term.fromTuple(_)))
    }
            
    @tailrec def eval(q: Array[C], u: Array[C], n: Int): (Polynomial[C], Polynomial[C]) = {
      if (u.isEmpty || n < 0) {
        (polyFromCoeffsLE(q), polyFromCoeffsBE(u))
      } else {
        val v0 = if (rhs.isZero) r.zero else rhs.maxOrderTermCoeff
        val q0 = u(0) / v0
        val uprime = zipSum(u, rhs.coeffs.map(_ * -q0))
        eval(Array(q0) ++ q, uprime, n - 1)
      }
    }

    val ym = rhs.maxTerm
    if (ym.exp == 0) {
      val q = Polynomial(lhs.data.map { case (e, c) => (e, c / ym.coeff) })
      val r = Polynomial(Map.empty[Int, C])
      (q, r)
    } else eval(Array[C](), lhs.coeffs, lhs.degree - rhs.degree)
  }

  def /~(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._1
    
  def %(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._2

  def monic(implicit f: Field[C]): Polynomial[C] = {
    val m = maxOrderTermCoeff
    Polynomial(data.map { case (e, c) => (e, c / m) })
  }

  def derivative: Polynomial[C] =
    Polynomial(data.flatMap { case (e, c) =>
      if (e > 0) Some(Term(c, e).der) else None
    })

  def integral(implicit f: Field[C]): Polynomial[C] =
    Polynomial(data.map(t => Term.fromTuple(t).int))

  implicit object BigEndianPolynomialOrdering extends Order[Term[C]] {
    def compare(x:Term[C], y:Term[C]): Int = y.exp compare x.exp
  }

  override def toString =
    if (isZero) {
      "(0)"
    } else {
      val ts = terms.toArray
      QuickSort.sort(ts)
      val s = ts.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }

  override def equals(that: Any): Boolean = that match {
    case p: Polynomial[_] => data == p.data
    case n => terms match {
      case Nil => n == 0
      case Term(c, 0) :: Nil => n == c
      case _ => false
    }
  }

}

// Dense polynomials - Little Endian Coeffs e.g. x^0, x^1, ... x^n
class PolyDense[@spec(Double) C] private[spire] (val coeffs: Array[C])
  (implicit r: Ring[C], s: Signed[C], val ct: ClassTag[C]) extends Function1[C, C] with Polynomial[C] { lhs =>

  def toSparse: PolySparse[C] = new PolySparse(data)
  def toDense: PolyDense[C] = lhs

  def data: Map[Int, C] = {
    terms.view.map(_.toTuple).toMap
  }

  def terms: List[Term[C]] =
    coeffs.view.zipWithIndex.map({ case (c, e) => 
      Term(c, e)}).filterNot(_.isZero).toList

  def maxTerm: Term[C] =
    data.view.foldLeft(Term.zero[C]) { case (term, (e, c)) =>
      if (term.exp <= e) Term(c, e) else term
    }

  def degree: Int =
    if (isZero) 0 else coeffs.length - 1

  def maxOrderTermCoeff: C =
    coeffs(degree)
    
  def isZero: Boolean =
    coeffs.isEmpty

  def apply(x: C): C = {
    val cs = coeffs
    var c = cs(cs.length - 1)
    var i = cs.length - 2
    while(i >= 0) {
      c = cs(i) + c * x
      i -= 1
    }
    c
  }

  def unary_-(): Polynomial[C] = {
    val negArray = new Array[C](coeffs.length)
    var i = 0
    while(i < coeffs.length) {
      negArray(i) = -coeffs(i)
      i += 1
    }
    Polynomial.dense(negArray)
  }

  def monic(implicit f: Field[C]): Polynomial[C] = {
    val m = maxOrderTermCoeff
    val cs = new Array[C](coeffs.length)
    var i = 0
    while(i < coeffs.length) {
      cs(i) = coeffs(i) / m
      i += 1
    }
    Polynomial.dense(cs)
  }

  def derivative: Polynomial[C] = {
    if (isZero) return this
    val d = degree
    val cs = new Array[C](d)
    var i = cs.length - 1
    var j = coeffs.length - 1
    while(i >= 0) {
      cs(i) = r.fromInt(j) * coeffs(j)
      i -= 1
      j -= 1
    }
    Polynomial.dense(cs)
  }

  def integral(implicit f: Field[C]): Polynomial[C] = {
    val cs = new Array[C](coeffs.length + 1)
    var i = 0
    while(i < coeffs.length) {
      cs(i + 1) = coeffs(i) / f.fromInt(i + 1)
      i += 1
    }
    cs(0) = f.zero // n.b. must initialize first term 0x^0 otherwise it's null
    Polynomial.dense(cs)
  }

  def +(rhs: Polynomial[C]): Polynomial[C] =
    Polynomial.dense(lhs.coeffs + rhs.coeffs)

  def *(rhs: Polynomial[C]): Polynomial[C] = {
    if (rhs.isZero) return rhs
    if (lhs.isZero) return lhs

    val lcs = lhs.coeffs
    val rcs = rhs.coeffs
    val cs = new Array[C](lcs.length + rcs.length - 1)
    var i = 0
    while (i < cs.length) {
      cs(i) = r.zero
      i += 1
    }
    i = 0
    while (i < lcs.length) {
      val c = lcs(i)
      var j = 0
      var k = i
      while (j < rcs.length) {
        cs(k) += c * rcs(j)
        j += 1
        k += 1
      }
      i += 1
    }
    if (cs.exists(_ == null)) sys.error("argh! %s" format cs.toList)
    Polynomial.dense(cs)
  }

  def /%(rhs: Polynomial[C])(implicit f: Field[C]): (Polynomial[C], Polynomial[C]) = {
    require(!rhs.isZero, "Can't divide by polynomial of zero!")

    def zipSum(lcs: Array[C], rcs: Array[C])(implicit r: Ring[C]): Array[C] = 
      (lcs + rcs).tail

    def polyFromCoeffsLE(cs: Array[C]): Polynomial[C] =
      Polynomial.dense(cs)

    def polyFromCoeffsBE(cs: Array[C]): Polynomial[C] = {
      val ncs = cs.dropWhile(_.signum == 0)
      Polynomial.dense(ncs.reverse)
    }
            
    @tailrec def eval(q: Array[C], u: Array[C], n: Int): (Polynomial[C], Polynomial[C]) = {
      if (u.isEmpty || n < 0) {
        (polyFromCoeffsLE(q), polyFromCoeffsBE(u))
      } else {
        val v0 = if (rhs.isZero) r.zero else rhs.maxOrderTermCoeff
        val q0 = u(0) / v0
        val uprime = zipSum(u, rhs.coeffs.reverse.map(_ * -q0))
        eval(Array(q0) ++ q, uprime, n - 1)
      }
    }

    if (rhs.coeffs.length == 1) {
      val q = Polynomial.dense(lhs.coeffs.map { c => c / rhs.coeffs(0) })
      val r = Polynomial.dense(Array[C]())
      (q, r)
    } else 
    eval(Array[C](), lhs.coeffs.reverse, lhs.degree - rhs.degree)
  }

  def /~(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._1
    
  def %(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._2

  implicit object BigEndianPolynomialOrdering extends Order[Term[C]] {
    def compare(x:Term[C], y:Term[C]): Int = y.exp compare x.exp
  }

  override def toString =
    if (isZero) {
      "(0)"
    } else {
      val ts = terms.toArray
      QuickSort.sort(ts)
      val s = ts.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }

  override def equals(that: Any): Boolean = that match {
    case p: Polynomial[_] => data == p.data
    case n => terms match {
      case Nil => n == 0
      case Term(c, 0) :: Nil => n == c
      case _ => false
    }
  }

}


object Polynomial {

  def dense[@spec(Double) C](coeffs: Array[C])(implicit r: Ring[C], s: Signed[C], ct: ClassTag[C]): PolyDense[C] = {
    var i = coeffs.length - 1
    while (i >= 0 && coeffs(i).signum == 0) i -= 1
    if (i == coeffs.length) {
      new PolyDense(coeffs)
    } else {
      val cs = new Array[C](i + 1)
      System.arraycopy(coeffs, 0, cs, 0, i + 1)
      new PolyDense(cs)
    }
  }

  def sparse[@spec(Double) C](data: Map[Int, C])(implicit r: Ring[C], s: Signed[C], ct: ClassTag[C]): PolySparse[C] =
    new PolySparse(data.filterNot { case (e, c) => s.sign(c) == Sign.Zero })

  def apply[@spec(Double) C](data: Map[Int, C])(implicit r: Ring[C], s: Signed[C], ct: ClassTag[C]): PolySparse[C] =
    new PolySparse(data.filterNot { case (e, c) => s.sign(c) == Sign.Zero })

  def apply[@spec(Double) C](terms: Iterable[Term[C]])(implicit r: Ring[C], s: Signed[C], ct: ClassTag[C]): PolySparse[C] =
    new PolySparse(terms.view.filterNot(_.isZero).map(_.toTuple).toMap)

  def apply[@spec(Double) C](terms: Traversable[Term[C]])(implicit r: Ring[C], s: Signed[C], ct: ClassTag[C]): PolySparse[C] =
    new PolySparse(terms.view.filterNot(_.isZero).map(_.toTuple).toMap)

  def apply[@spec(Double) C](c: C, e: Int)(implicit r: Ring[C], s: Signed[C], ct: ClassTag[C]): PolySparse[C] =
    new PolySparse(Map(e -> c).filterNot { case (e, c) => s.sign(c) == Sign.Zero})

  import scala.util.{Try, Success, Failure}

  def apply(s: String): Polynomial[Rational] = parse(s)

  def zero[@spec(Double) C: Signed: Ring: ClassTag] = new PolySparse(Map.empty[Int, C])
  def one[@spec(Double) C: Signed: ClassTag](implicit r: Ring[C]) = new PolySparse(Map(0 -> r.one))

  def constant[@spec(Double) C: Signed: Ring: ClassTag](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(0 -> c))
  def linear[@spec(Double) C: Signed: Ring: ClassTag](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(1 -> c))
  def quadratic[@spec(Double) C: Signed: Ring: ClassTag](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(2 -> c))
  def cubic[@spec(Double) C: Signed: Ring: ClassTag](c: C) =
    if (c.signum == 0) zero[C] else Polynomial(Map(3 -> c))

  implicit def ring[@spec(Double) C: ClassTag](implicit a: Ring[C], s: Signed[C]) =
    new PolynomialRing[C] {
      val ct = classTag[C]
      val algebra = a
      val signed = s
    }

  implicit def euclideanRing[@spec(Double) C: ClassTag](implicit a: Field[C], s: Signed[C]) =
    new PolynomialEuclideanRing[C] {
      val ct = classTag[C]
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


trait Polynomial[@spec(Double) C] {

  def toDense: Polynomial[C]
  def toSparse: Polynomial[C]

  def terms: List[Term[C]]
  def coeffs: Array[C]
  def data: Map[Int, C]

  def maxTerm: Term[C]
  def degree: Int
  def maxOrderTermCoeff: C
  def isZero: Boolean

  def apply(x: C): C
  def unary_-(): Polynomial[C]
  def monic(implicit f: Field[C]): Polynomial[C]
  def derivative: Polynomial[C]
  def integral(implicit f: Field[C]): Polynomial[C]
  def +(rhs: Polynomial[C]): Polynomial[C]
  def *(rhs: Polynomial[C]): Polynomial[C]
  def /~(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C]
  def /%(rhs: Polynomial[C])(implicit f: Field[C]): (Polynomial[C], Polynomial[C])
  def %(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C]

  implicit def ct: ClassTag[C]
}

trait PolynomialRing[@spec(Double) C] extends Ring[Polynomial[C]] {
  implicit def algebra: Ring[C]
  implicit def signed: Signed[C]
  implicit def ct: ClassTag[C]

  def zero: Polynomial[C] = Polynomial.zero[C]
  def one: Polynomial[C] = Polynomial.one[C]

  def negate(x: Polynomial[C]): Polynomial[C] = -x
  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x + y
  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x * y
}

trait PolynomialEuclideanRing[@spec(Double) C] extends EuclideanRing[Polynomial[C]] with PolynomialRing[C] {
  implicit def algebra: Field[C]

  def quot(x: Polynomial[C], y: Polynomial[C]) = x /~ y
  def mod(x: Polynomial[C], y: Polynomial[C]) = x % y
  override def quotmod(x: Polynomial[C], y: Polynomial[C]) = x /% y

  @tailrec final def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] =
    if (y.isZero && x.isZero) Polynomial(algebra.zero, 0)
    else if (y.maxTerm.isZero) x
    else gcd(y, x % y)
}
