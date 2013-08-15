package spire.math

import compat._
import spire.math.poly._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}

/**
 * Polynomial
 * A univariate polynomial class and EuclideanRing extension trait 
 * for arithmetic operations. Polynomials can be instantiated using 
 * any type C for which a Ring[C] and Signed[C] are in scope, with 
 * exponents given by Int values. Some operations require a Field[C]
 * to be in scope.
*/

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
