package spire.math.poly

import compat._
import spire.math._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}

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
