package spire.math.poly

import compat._
import spire.math._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._
import spire.syntax._

import scala.{specialized => spec}

// Dense polynomials - Little Endian Coeffs e.g. x^0, x^1, ... x^n
class PolyDense[@spec(Double) C] private[spire] (val coeffs: Array[C])
  (implicit r: Ring[C], s: Signed[C], val ct: ClassTag[C]) extends Function1[C, C] with Polynomial[C] { lhs =>

  private val _degree = if (isZero) 0 else coeffs.length - 1
  def degree = _degree

  def toSparse: PolySparse[C] = new PolySparse(data)
  def toDense: PolyDense[C] = lhs

  def data: Map[Int, C] =
    (0 to coeffs.length - 1).foldLeft(Map.empty[Int, C]) { (m, e) =>
      if (coeffs(e).signum == 0) m else m.updated(e, coeffs(e))
    }

  def nth(n: Int): C =
    if (n < coeffs.length) coeffs(n) else r.zero

  def terms: List[Term[C]] =
    if (isZero)
      Term(r.zero, 0) :: Nil
    else
      (degree to 0 by -1).foldLeft(List.empty[Term[C]]) { (ts, e) =>
        if (coeffs(e).signum == 0) ts else Term(coeffs(e), e) :: ts
      }

  def maxTerm: Term[C] =
    if (isZero) Term(r.zero, 0) else Term(nth(degree), degree)
    

  def maxOrderTermCoeff: C =
    if (isZero) r.zero else coeffs(degree)
    
  def isZero: Boolean =
    coeffs.length == 0

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
    cs(0) = f.zero
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

    val cs = rhs.coeffs
    if (cs.length == 0) {
      throw new ArithmeticException("/ by zero polynomial")
    } else if (cs.length == 1) {
      val c = cs(0)
      val q = Polynomial.dense(lhs.coeffs.map(_ / c))
      val r = Polynomial.dense(new Array[C](0))
      (q, r)
    } else {
      eval(new Array[C](0), lhs.coeffs.reverse, lhs.degree - rhs.degree)
    }
  }

  def /~(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._1
    
  def %(rhs: Polynomial[C])(implicit f: Field[C]): Polynomial[C] = (lhs /% rhs)._2
}
