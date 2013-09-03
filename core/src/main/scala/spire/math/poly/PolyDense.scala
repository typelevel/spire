package spire.math.poly

import compat._
import spire.math._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._

import scala.{specialized => spec}

// Dense polynomials - Little Endian Coeffs e.g. x^0, x^1, ... x^n
class PolyDense[@spec(Double) C] private[spire] (val coeffs: Array[C])
  (implicit r: Ring[C], s: Signed[C], val ct: ClassTag[C]) extends Function1[C, C] with Polynomial[C] { lhs =>

  // val _degree: Int = if(isZero) 0 else coeffs.length - 1
  def degree: Int = if(isZero) 0 else coeffs.length - 1//_degree

  def toSparse: PolySparse[C] = Polynomial.sparse(data)
  def toDense: PolyDense[C] = lhs

  def foreachNonZero[U](f: (Int, C) => U): Unit = {
    cfor(0)(_ < coeffs.length, _ + 1) { e =>
      val c = coeffs(e)
      if (c.signum != 0)
        f(e, c)
    }
  }

  def nth(n: Int): C =
    if (n < coeffs.length) coeffs(n) else r.zero

  def maxOrderTermCoeff: C =
    if (isZero) r.zero else coeffs(degree)
    
  def isZero: Boolean =
    coeffs.length == 0

  def apply(x: C): C = {
    val cs = coeffs
    var c = cs(cs.length - 1)
    cfor(cs.length - 2)(_ >= 0, _ - 1) { i => c = cs(i) + c * x }
    c
  }

  def unary_-(): Polynomial[C] = {
    val negArray = new Array[C](coeffs.length)
    cfor(0)(_ < coeffs.length, _ + 1) { i => negArray(i) = -coeffs(i) }
    Polynomial.dense(negArray)
  }

  def derivative: Polynomial[C] = {
    if (isZero) return this
    val cs = new Array[C](degree)
    var j = coeffs.length - 1
    cfor(cs.length - 1)(_ >= 0, _ - 1) { i =>
      cs(i) = r.fromInt(j) * coeffs(j)
      j -= 1
    }
    Polynomial.dense(cs)
  }

  def integral(implicit f: Field[C]): Polynomial[C] = {
    val cs = new Array[C](coeffs.length + 1)
    cs(0) = f.zero
    cfor(0)(_ < coeffs.length, _ + 1) { i => cs(i + 1) = coeffs(i) / f.fromInt(i + 1) }
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
    cfor(0)(_ < cs.length, _ + 1) { i => cs(i) = r.zero }
    cfor(0)(_ < lcs.length, _ + 1) { i =>
      val c = lcs(i)
      var k = i
      cfor(0)(_ < rcs.length, _ + 1) { j =>
        cs(k) += c * rcs(j)
        k += 1
      }
    }
    Polynomial.dense(cs)
  }

  def *: (k: C): Polynomial[C] =
    if (k.signum == 0) {
      Polynomial.dense(new Array[C](0))
    } else {
      val cs = new Array[C](coeffs.length)
      cfor(0)(_ < cs.length, _ + 1) { i =>
        cs(i) = k * coeffs(i)
      }
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
