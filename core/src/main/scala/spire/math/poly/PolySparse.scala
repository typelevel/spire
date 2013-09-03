package spire.math.poly

import compat._
import spire.math._
import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap
import scala.reflect._
import spire.algebra._
import spire.implicits._

import scala.{specialized => spec}

// Sparse polynomials - Big Endian Coeffs e.g. x^n, x^n-1, ... x^0
class PolySparse[@spec(Double) C] private[spire] (val data: ParMap[Int, C])
  (implicit r: Ring[C], s: Signed[C], val ct: ClassTag[C]) extends Function1[C, C] with Polynomial[C] { lhs =>

  private lazy val _degree: Int = if(isZero) 0 else data.keys.qmax
  def degree: Int = _degree

  def toDense: PolyDense[C] =
    if (isZero) {
      Polynomial.dense(new Array[C](0))
    } else {
      val m = degree + 1
      val cs = new Array[C](m)
      cfor(0)(_ < m, _ + 1) { i => cs(i) = data.getOrElse(i, r.zero) }
      Polynomial.dense(cs)
    }

  def toSparse: PolySparse[C] = lhs

  def allTerms: List[Term[C]] =
    (degree to 0 by -1).map(e => Term(nth(e), e)).toList

  def terms: List[Term[C]] =
    data.map(Term.fromTuple(_)).toList

  def coeffs: Array[C] =
    (degree to 0 by -1).map(nth).toArray

  def nth(n: Int): C =
    data.getOrElse(n, r.zero)

  def maxTerm: Term[C] =
    Term(nth(degree), degree)

  def maxOrderTermCoeff: C =
    nth(degree)

  def apply(x: C): C =
    data.foldLeft(r.zero) { case (sum, (e, c)) => sum + x.pow(e) * c }

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
    Polynomial(data.map(Term.fromTuple(_).int))
}
