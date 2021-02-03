package spire
package math
package poly

import spire.algebra.{Eq, Field, Ring, Rng, Semiring}
import spire.math.Polynomial
import spire.std.array._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.field._

// Dense polynomials - Little Endian Coeffs e.g. x^0, x^1, ... x^n
class PolyDense[@sp(Double) C] private[spire] (val coeffs: Array[C])(implicit val ct: ClassTag[C])
    extends Polynomial[C] { lhs =>

  def degree: Int = if (isZero) 0 else coeffs.length - 1

  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C] =
    Polynomial.sparse(data)

  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C] = lhs

  def foreach[U](f: (Int, C) => U): Unit = {
    cfor(0)(_ < coeffs.length, _ + 1) { e =>
      f(e, coeffs(e))
    }
  }

  override def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C], eq: Eq[C]): Unit = {
    cfor(0)(_ < coeffs.length, _ + 1) { e =>
      val c = coeffs(e)
      if (c =!= ring.zero)
        f(e, c)
    }
  }

  def termsIterator: Iterator[Term[C]] =
    new TermIterator

  class TermIterator extends Iterator[Term[C]] {
    private[this] var e: Int = 0
    private[this] def findNext(): Unit =
      while (e < coeffs.length && coeffs(e) == 0) e += 1
    findNext()
    def hasNext: Boolean = e < coeffs.length
    override def next(): Term[C] = {
      val term = Term[C](coeffs(e), e)
      e += 1
      findNext()
      term
    }
  }

  def coeffsArray(implicit ring: Semiring[C]): Array[C] = coeffs

  def nth(n: Int)(implicit ring: Semiring[C]): C =
    if (n < coeffs.length) coeffs(n) else ring.zero

  def maxOrderTermCoeff(implicit ring: Semiring[C]): C =
    if (isZero) ring.zero else coeffs(degree)

  def reductum(implicit e: Eq[C], ring: Semiring[C], ct: ClassTag[C]): Polynomial[C] = {
    var i = coeffs.length - 2
    while (i >= 0 && coeffs(i) === ring.zero) i -= 1
    if (i < 0) {
      new PolyDense(new Array[C](0))
    } else {
      val arr = new Array[C](i + 1)
      System.arraycopy(coeffs, 0, arr, 0, i + 1)
      new PolyDense(arr)
    }
  }

  def isZero: Boolean =
    coeffs.length == 0

  def apply(x: C)(implicit ring: Semiring[C]): C = {
    if (isZero) return ring.zero

    var even = coeffs.length - 1
    var odd = coeffs.length - 2
    if ((even & 1) == 1) { even = odd; odd = coeffs.length - 1 }

    var c0 = coeffs(even)
    val x2 = x.pow(2)
    cfor(even - 2)(_ >= 0, _ - 2) { i =>
      c0 = coeffs(i) + c0 * x2
    }

    if (odd >= 1) {
      var c1 = coeffs(odd)
      cfor(odd - 2)(_ >= 1, _ - 2) { i =>
        c1 = coeffs(i) + c1 * x2
      }
      c0 + c1 * x
    } else {
      c0
    }
  }

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C] = {
    if (isZero) return this
    val cs = new Array[C](degree)
    var j = coeffs.length - 1
    cfor(cs.length - 1)(_ >= 0, _ - 1) { i =>
      cs(i) = ring.fromInt(j) * coeffs(j)
      j -= 1
    }
    Polynomial.dense(cs)
  }

  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = {
    val cs = new Array[C](coeffs.length + 1)
    cs(0) = field.zero
    cfor(0)(_ < coeffs.length, _ + 1) { i => cs(i + 1) = coeffs(i) / field.fromInt(i + 1) }
    Polynomial.dense(cs)
  }

  def unary_-(implicit ring: Rng[C]): Polynomial[C] = {
    val negArray = new Array[C](coeffs.length)
    cfor(0)(_ < coeffs.length, _ + 1) { i => negArray(i) = -coeffs(i) }
    new PolyDense(negArray)
  }

  def +(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] =
    PolyDense.plusDense(lhs, rhs)

  def *(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    if (rhs.isZero) return rhs
    if (lhs.isZero) return lhs
    val lcs = lhs.coeffsArray
    val rcs = rhs.coeffsArray
    val cs = new Array[C](lcs.length + rcs.length - 1)
    cfor(0)(_ < cs.length, _ + 1) { i => cs(i) = ring.zero }
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

  def *:(k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] =
    if (k === ring.zero) {
      Polynomial.dense(new Array[C](0))
    } else {
      val cs = new Array[C](coeffs.length)
      cfor(0)(_ < cs.length, _ + 1) { i =>
        cs(i) = k * coeffs(i)
      }
      Polynomial.dense(cs)
    }
}

object PolyDense {
  final private def plusDense[C: Semiring: Eq: ClassTag](lhs: Polynomial[C], rhs: Polynomial[C]): Polynomial[C] = {
    val lcoeffs = lhs.coeffsArray
    val rcoeffs = rhs.coeffsArray
    if (lcoeffs.length < rcoeffs.length) {
      plusDense(rhs, lhs)
    } else {
      val cs = new Array[C](lcoeffs.length)
      cfor(0)(_ < rcoeffs.length, _ + 1) { i =>
        cs(i) = lcoeffs(i) + rcoeffs(i)
      }
      cfor(rcoeffs.length)(_ < lcoeffs.length, _ + 1) { i =>
        cs(i) = lcoeffs(i)
      }
      Polynomial.dense(cs)
    }
  }

  final private[math] def quotmodDense[@sp(Double) C: Field: Eq: ClassTag](lhs: PolyDense[C],
                                                                           rhs: Polynomial[C]
  ): (Polynomial[C], Polynomial[C]) = {
    def zipSum(lcs: Array[C], rcs: Array[C]): Array[C] =
      (lcs + rcs).tail

    def polyFromCoeffsLE(cs: Array[C]): Polynomial[C] =
      Polynomial.dense(cs)

    def polyFromCoeffsBE(cs: Array[C]): Polynomial[C] = {
      val ncs = cs.dropWhile(_ === Field[C].zero)
      Polynomial.dense(ncs.reverse)
    }

    @tailrec def eval(q: Array[C], u: Array[C], n: Int): (Polynomial[C], Polynomial[C]) = {
      if (u.isEmpty || n < 0) {
        (polyFromCoeffsLE(q), polyFromCoeffsBE(u))
      } else {
        val v0 = if (rhs.isZero) Field[C].zero else rhs.maxOrderTermCoeff
        val q0 = u(0) / v0
        val uprime = zipSum(u, rhs.coeffsArray.reverse.map(_ * -q0))
        eval(Array(q0) ++ q, uprime, n - 1)
      }
    }

    val cs = rhs.coeffsArray
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

}
