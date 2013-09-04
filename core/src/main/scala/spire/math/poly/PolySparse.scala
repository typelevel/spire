package spire.math.poly

import java.lang.Integer.{ numberOfLeadingZeros, numberOfTrailingZeros }

import spire.math._
import scala.annotation.tailrec
import scala.reflect.ClassTag
import spire.algebra._
import spire.implicits._

import scala.{specialized => spec}

case class PolySparse[@spec(Double) C] private [spire] (val exp: Array[Int], val coeff: Array[C])
    (implicit val ct: ClassTag[C]) extends Polynomial[C] { lhs =>

  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C] =
    Polynomial.dense(coeffsArray)

  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C] = lhs

  def foreach[U](f: (Int, C) => U): Unit =
    cfor(0)(_ < exp.length, _ + 1) { i => f(exp(i), coeff(i)) }

  override def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C], eq: Eq[C]): Unit =
    foreach(f)

  def degree: Int = if (isZero) 0 else exp(exp.length - 1)

  def coeffsArray(implicit ring: Semiring[C]): Array[C] = if (isZero) {
    new Array[C](0)
  } else {
    val cs = new Array[C](degree + 1)
    cfor(0)(_ < cs.length, _ + 1) { i => cs(i) = ring.zero }
    cfor(0)(_ < exp.length, _ + 1) { i =>
      cs(exp(i)) = coeff(i)
    }
    cs
  }

  def nth(n: Int)(implicit ring: Semiring[C]): C = {
    val i = java.util.Arrays.binarySearch(exp, n)
    if (i >= 0) coeff(i) else ring.zero
  }

  def maxOrderTermCoeff(implicit ring: Semiring[C]): C =
    if (isZero) ring.zero else coeff(coeff.length - 1)

  private final def expBits(x: C)(implicit ring: Semiring[C]): Array[C] = {
    val bits = new Array[C](math.max(2, 32 - numberOfLeadingZeros(degree)))
    bits(0) = x
    cfor(1)(_ < bits.length, _ + 1) { i =>
      val prev = bits(i - 1)
      bits(i) = prev * prev
    }
    bits
  }

  @tailrec
  private final def fastExp(bits: Array[C], e: Int, i: Int, acc: C)(implicit ring: Semiring[C]): C = {
    if (e == 0) acc else {
      val lb = numberOfTrailingZeros(e) + 1
      val j = i + lb
      fastExp(bits, e >>> lb, j, acc * bits(j - 1))
    }
  }

  private final def fastExp(bits: Array[C], e: Int)(implicit ring: Semiring[C]): C = {
    val lb = numberOfTrailingZeros(e) + 1
    fastExp(bits, e >>> lb, lb, bits(lb - 1))
  }

  def isZero: Boolean =
    exp.isEmpty

  def apply(x: C)(implicit ring: Semiring[C]): C = if (isZero) {
    ring.zero
  } else if (exp.length == 1) {
    if (exp(0) != 0) coeff(0) * (x pow exp(0)) else coeff(0)
  } else {
    // TODO: Rewrite this to be more like PolyDense.
    val bits = expBits(x)
    val e0 = exp(0)
    val c0 = coeff(0)
    var sum = if (e0 == 0) c0 else c0 * fastExp(bits, e0)
    cfor(1)(_ < exp.length, _ + 1) { i =>
      sum += coeff(i) * fastExp(bits, exp(i))
    }
    sum
  }

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C] =
    Polynomial(data.flatMap { case (e, c) =>
      if (e > 0) Some(Term(c, e).der) else None
    })

  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C] =
    Polynomial(data.map(Term.fromTuple(_).int))

  def unary_-()(implicit ring: Rng[C]): Polynomial[C] = {
    val cs = new Array[C](coeff.length)
    cfor(0)(_ < cs.length, _ + 1) { i => cs(i) = -coeff(i) }
    new PolySparse(exp, cs)
  }

  def +(rhs0: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    val rhs: PolySparse[C] = PolySparse(rhs0)
    PolySparse.addSparse(lhs, rhs)
  }

  def *(rhs0: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    val rhs: PolySparse[C] = PolySparse(rhs0)
    PolySparse.multiplySparse(lhs, rhs)
  }

  def /%(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): (Polynomial[C], Polynomial[C]) = {
    require(!rhs.isZero, "Can't divide by polynomial of zero!")

    PolySparse.quotmodSparse(lhs, PolySparse(rhs))
  }

  def *: (k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    if (k === ring.zero) {
      PolySparse.zero[C]
    } else {
      val cs = new Array[C](coeff.length)
      cfor(0)(_ < cs.length, _ + 1) { i =>
        cs(i) = k * coeff(i)
      }
      new PolySparse(exp, cs)
    }
  }
}


object PolySparse {
  private final def dense2sparse[@spec(Double) C: Semiring: Eq: ClassTag](poly: PolyDense[C]): PolySparse[C] = {
    val cs = poly.coeffs
    val es = new Array[Int](cs.length)
    cfor(0)(_ < es.length, _ + 1) { i => es(i) = i }
    PolySparse.safe(es, cs)
  }

  private[math] final def safe[@spec(Double) C: Semiring: Eq: ClassTag]
      (exp: Array[Int], coeff: Array[C]): PolySparse[C] = {
    var len = 0
    cfor(0)(_ < coeff.length, _ + 1) { i =>
      if (coeff(i) =!= Semiring[C].zero)
        len += 1
    }

    if (len == coeff.length) {
      new PolySparse(exp, coeff)
    } else {
      val es = new Array[Int](len)
      val cs = new Array[C](len)
      @tailrec def loop(i: Int, j: Int): PolySparse[C] =
        if (i < coeff.length) {
          val c = coeff(i)
          if (c =!= Semiring[C].zero) {
            es(j) = exp(i)
            cs(j) = c
            loop(i + 1, j + 1)
          } else {
            loop(i + 1, j)
          }
        } else new PolySparse(es, cs)
      loop(0, 0)
    }
  }

  final def apply[@spec(Double) C: Semiring: Eq: ClassTag](data: Map[Int,C]): PolySparse[C] = {
    val data0 = data.toArray
    data0.qsortBy(_._1)
    val es = new Array[Int](data0.length)
    val cs = new Array[C](data0.length)
    cfor(0)(_ < data0.length, _ + 1) { i =>
      val (e, c) = data0(i)
      es(i) = e
      cs(i) = c
    }
    safe(es, cs)
  }

  final def apply[@spec(Double) C: Semiring: Eq: ClassTag](poly: Polynomial[C]): PolySparse[C] = {
    poly match {
      case (poly: PolySparse[_]) =>
        poly

      case (_: PolyDense[_]) =>
        dense2sparse(poly.asInstanceOf[PolyDense[C]]) // Yay...

      case _ =>
        var len = 0
        poly.foreachNonZero { (_, _) => len += 1 }
        val es = new Array[Int](len)
        val cs = new Array[C](len)
        var i = 0
        poly.foreachNonZero { (e, c) =>
          es(i) = e
          cs(i) = c
          i += 1
        }
        PolySparse.safe(es, cs)
    }
  }

  final def zero[@spec(Double) C: Semiring: Eq: ClassTag]: PolySparse[C] =
    new PolySparse(new Array[Int](0), new Array[C](0))

  private final def multiplyTerm[@spec(Double) C: Semiring: Eq: ClassTag](poly: PolySparse[C], c: C, e: Int): PolySparse[C] = {
    val exp = poly.exp
    val coeff = poly.coeff
    val cs = new Array[C](coeff.length)
    val es = new Array[Int](exp.length)
    cfor(0)(_ < coeff.length, _ + 1) { i =>
      cs(i) = c * coeff(i)
      es(i) = exp(i) + e
    }
    new PolySparse(es, cs)
  }

  private final def multiplySparse[@spec(Double) C: Semiring: Eq: ClassTag]
      (lhs: PolySparse[C], rhs: PolySparse[C]): PolySparse[C] = {
    val lexp = lhs.exp
    val lcoeff = lhs.coeff
    var sum = new PolySparse(new Array[Int](0), new Array[C](0))
    cfor(0)(_ < lexp.length, _ + 1) { i =>
      sum = addSparse(sum, multiplyTerm(rhs, lcoeff(i), lexp(i)))
    }
    sum
  }

  private final def countSumTerms[@spec(Double) C]
      (lhs: PolySparse[C], rhs: PolySparse[C], lOffset: Int = 0, rOffset: Int = 0): Int = {
    val PolySparse(lexp, lcoeff) = lhs
    val PolySparse(rexp, rcoeff) = rhs

    @tailrec
    def loop(i: Int, j: Int, count: Int): Int =
      if (i < lexp.length && j < rexp.length) {
        val cmp = lexp(i) + lOffset - rexp(j) - rOffset
        if (cmp == 0) loop(i + 1, j + 1, count + 1)
        else if (cmp < 0) loop(i + 1, j, count + 1)
        else loop(i, j + 1, count + 1)
      } else {
        count + (lexp.length - i) + (rexp.length - j)
      }

    loop(0, 0, 0)
  }

  private final def addSparse[C: Eq: Semiring: ClassTag](lhs: PolySparse[C], rhs: PolySparse[C]): PolySparse[C] = {
    val PolySparse(lexp, lcoeff) = lhs
    val PolySparse(rexp, rcoeff) = rhs

    val len = countSumTerms(lhs, rhs)
    val es = new Array[Int](len)
    val cs = new Array[C](len)

    @tailrec
    def sum(i: Int, j: Int, k: Int): PolySparse[C] =
      if (i < lexp.length && j < rexp.length) {
        val ei = lexp(i)
        val ej = rexp(j)
        if (ei == ej) {
          es(k) = ei
          cs(k) = lcoeff(i) + rcoeff(j)
          sum(i + 1, j + 1, k + 1)
        } else if (ei < ej) {
          es(k) = ei
          cs(k) = lcoeff(i)
          sum(i + 1, j, k + 1)
        } else {
          es(k) = ej
          cs(k) = rcoeff(j)
          sum(i, j + 1, k + 1)
        }
      } else {
        var k0 = k
        cfor(i)(_ < lexp.length, _ + 1) { i0 =>
          es(k0) = lexp(i0)
          cs(k0) = lcoeff(i0)
          k0 += 1
        }
        cfor(j)(_ < rexp.length, _ + 1) { j0 =>
          es(k0) = rexp(j0)
          cs(k0) = rcoeff(j0)
          k0 += 1
        }
        PolySparse.safe(es, cs)
      }

    sum(0, 0, 0)
  }

  private final def subtractScaled[C: Eq: Rng: ClassTag]
      (lhs: PolySparse[C], c: C, e: Int, rhs: PolySparse[C]) = {
    val PolySparse(lexp, lcoeff) = lhs
    val PolySparse(rexp, rcoeff) = rhs

    val len = countSumTerms(lhs, rhs, 0, e)
    val es = new Array[Int](len)
    val cs = new Array[C](len)

    @tailrec
    def loop(i: Int, j: Int, k: Int): PolySparse[C] = {
      if (i < lexp.length && j < rexp.length) {
        val ei = lexp(i)
        val ej = rexp(j) + e
        if (ei == ej) {
          es(k) = ei
          cs(k) = lcoeff(i) - c * rcoeff(j)
          loop(i + 1, j + 1, k + 1)
        } else if (ei < ej) {
          es(k) = ei
          cs(k) = lcoeff(i)
          loop(i + 1, j, k + 1)
        } else {
          es(k) = ej
          cs(k) = -c * rcoeff(j)
          loop(i, j + 1, k + 1)
        }
      } else {
        var k0 = k
        cfor(i)(_ < lexp.length, _ + 1) { i0 =>
          es(k0) = lexp(i0)
          cs(k0) = lcoeff(i0)
          k0 += 1
        }
        cfor(j)(_ < rexp.length, _ + 1) { j0 =>
          es(k0) = rexp(j0) + e
          cs(k0) = -c * rcoeff(j0)
          k0 += 1
        }
        PolySparse.safe(es, cs)
      }
    }

    loop(0, 0, 0)
  }

  private final def quotmodSparse[@spec(Double) C: Field: Eq: ClassTag]
      (lhs: PolySparse[C], rhs: PolySparse[C]): (PolySparse[C], PolySparse[C]) = {
    val rdegree = rhs.degree
    val rmaxCoeff = rhs.maxOrderTermCoeff

    @tailrec
    def inflate(ts: List[Term[C]], i: Int, es: Array[Int], cs: Array[C]): PolySparse[C] =
      ts match {
        case Term(c, e) :: ts0 => es(i) = e; cs(i) = c; inflate(ts0, i + 1, es, cs)
        case Nil => new PolySparse(es, cs)
      }

    @tailrec
    def loop(quot: List[Term[C]], rem: PolySparse[C]): (PolySparse[C], PolySparse[C]) =
      if (!rem.isZero && rem.degree >= rdegree) {
        val c0 = rem.maxOrderTermCoeff / rmaxCoeff
        val e0 = rem.degree - rdegree
        loop(Term(c0, e0) :: quot, subtractScaled(rem, c0, e0, rhs))
      } else {
        val len = quot.size
        (inflate(quot, 0, new Array[Int](len), new Array[C](len)), rem)
      }

    loop(Nil, lhs)
  }
}
