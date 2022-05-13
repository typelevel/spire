/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

package spire
package math

import spire.scalacompat.arrayBuilderMake

import spire.algebra._
import spire.math.poly._
import spire.std.array._
import spire.syntax.field._
import spire.syntax.eq._
import spire.syntax.std.seq._
import scala.annotation.nowarn

/**
 * Polynomial A univariate polynomial class and EuclideanRing extension trait for arithmetic operations. Polynomials can
 * be instantiated using any type C for which a Ring[C] and Eq[C] are in scope, with exponents given by Int values. Some
 * operations require more precise algebraic structures, such as `GCDRing`, `EuclideanRing` or `Field` to be in scope.
 */

object Polynomial extends PolynomialInstances {

  def dense[@sp(Double) C: Semiring: Eq: ClassTag](coeffs: Array[C]): PolyDense[C] = {
    var i = coeffs.length
    while (i > 0 && coeffs(i - 1) === Semiring[C].zero) i -= 1
    if (i == coeffs.length) {
      new PolyDense(coeffs)
    } else {
      val cs = new Array[C](i)
      System.arraycopy(coeffs, 0, cs, 0, i)
      new PolyDense(cs)
    }
  }

  def sparse[@sp(Double) C: Semiring: Eq: ClassTag](data: Map[Int, C]): PolySparse[C] =
    PolySparse(data)

  def apply[@sp(Double) C: Semiring: Eq: ClassTag](data: Map[Int, C]): PolySparse[C] =
    sparse(data)

  def apply[@sp(Double) C: Semiring: Eq: ClassTag](terms: IterableOnce[Term[C]]): PolySparse[C] =
    PolySparse(terms)

  def apply[@sp(Double) C: Semiring: Eq: ClassTag](c: C, e: Int): PolySparse[C] =
    PolySparse.safe(Array(e), Array(c))

  def apply(s: String): Polynomial[Rational] = parse(s)

  def zero[@sp(Double) C: Eq: Semiring: ClassTag]: Polynomial[C] =
    PolySparse.zero[C]
  def constant[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((0, c)))
  def linear[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((1, c)))
  def linear[@sp(Double) C: Eq: Semiring: ClassTag](c1: C, c0: C): Polynomial[C] =
    Polynomial(Map((1, c1), (0, c0)))
  def quadratic[@sp(Double) C: Eq: Semiring: ClassTag](c1: C, c0: C): Polynomial[C] =
    Polynomial(Map((1, c1), (0, c0)))
  def quadratic[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((2, c)))
  def quadratic[@sp(Double) C: Eq: Semiring: ClassTag](c2: C, c1: C, c0: C): Polynomial[C] =
    Polynomial(Map((2, c2), (1, c1), (0, c0)))
  def cubic[@sp(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map((3, c)))
  def cubic[@sp(Double) C: Eq: Semiring: ClassTag](c3: C, c2: C, c1: C, c0: C): Polynomial[C] =
    Polynomial(Map((3, c3), (2, c2), (1, c1), (0, c0)))
  def one[@sp(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    constant(Rig[C].one)
  def x[@sp(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    linear(Rig[C].one)
  def twox[@sp(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    linear(Rig[C].one + Rig[C].one)

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
          case None    => if (ts.isEmpty) ("+", s) else throw new IllegalArgumentException(s)
        }

        val m2 = termRe.findPrefixMatchOf(s2).getOrElse(throw new IllegalArgumentException(s2))
        val c0 = Option(m2.group(1)).getOrElse("1")
        val c = if (op == "-") "-" + c0 else c0
        val v = Option(m2.group(2)).getOrElse("")
        val e0 = Option(m2.group(3)).getOrElse("")
        val e = if (e0 != "") e0 else if (v == "") "0" else "1"

        val t =
          try {
            T(Rational(c), v, e.toInt)
          } catch {
            case _: Exception => throw new IllegalArgumentException(s"illegal term: $c*x^$e")
          }
        parse(s2.substring(m2.end), if (t.c == 0) ts else t :: ts)
      }

    // do some pre-processing to remove whitespace/outer parens
    val t = s.trim
    val u = if (t.startsWith("(") && t.endsWith(")")) t.substring(1, t.length - 1) else t
    val v = Term.removeSuperscript(u)

    // parse out the terms
    val ts = parse(v, Nil)

    // make sure we have at most one variable
    val vs = ts.view.map(_.v).toSet.filter(_ != "")
    if (vs.size > 1) throw new IllegalArgumentException("only univariate polynomials supported")

    // we're done!
    ts.foldLeft(Polynomial.zero[Rational])((a, t) => a + Polynomial(t.c, t.e))
  }

  final private def split[@sp(Double) C: ClassTag](poly: Polynomial[C]): (Array[Int], Array[C]) = {
    val es = arrayBuilderMake[Int]
    val cs = arrayBuilderMake[C]
    poly.foreach { (e, c) =>
      es += e
      cs += c
    }
    (es.result(), cs.result())
  }

  def interpolate[C: Field: Eq: ClassTag](points: (C, C)*): Polynomial[C] = {
    def loop(p: Polynomial[C], xs: List[C], pts: List[(C, C)]): Polynomial[C] =
      pts match {
        case Nil =>
          p
        case x, y :: tail =>
          val c = Polynomial.constant((y - p(x)) / xs.map(x - _).qproduct)
          val prod = xs.foldLeft(Polynomial.one[C]) { (prod, xn) =>
            prod * (Polynomial.x[C] - constant(xn))
          }
          loop(p + c * prod, x :: xs, tail)
      }
    loop(Polynomial.zero[C], Nil, points.toList)
  }
}

trait Polynomial[@sp(Double) C] { lhs =>
  implicit def ct: ClassTag[C]

  /**
   * Returns a polynmial that has a dense representation.
   */
  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C]

  /**
   * Returns a polynomial that has a sparse representation.
   */
  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C]

  /**
   * Traverses each term in this polynomial, in order of degree, lowest to highest (eg. constant term would be first)
   * and calls `f` with the degree of term and its coefficient. This may skip zero terms, or it may not.
   */
  def foreach[U](f: (Int, C) => U): Unit

  /**
   * Traverses each non-zero term in this polynomial, in order of degree, lowest to highest (eg. constant term would be
   * first) and calls `f` with the degree of term and its coefficient.
   */
  def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C], eq: Eq[C]): Unit =
    foreach { (e, c) => if (c =!= ring.zero) f(e, c) }

  /**
   * Returns the coefficients in little-endian order. So, the i-th element is coeffsArray(i) * (x ** i).
   */
  def coeffsArray(implicit ring: Semiring[C]): Array[C]

  /**
   * Returns a list of non-zero terms.
   */
  def terms(implicit ring: Semiring[C], eq: Eq[C]): List[Term[C]] = {
    val lb = new scala.collection.mutable.ListBuffer[Term[C]]
    foreachNonZero { (e, c) =>
      lb += Term(c, e)
    }
    lb.result()
  }

  /**
   * Return an iterator of non-zero terms.
   *
   * This method is used to implement equals and hashCode.
   *
   * NOTE: This method uses a (_ == 0) test to prune zero values. This makes sense in a context where Semiring[C] and
   * Eq[C] are unavailable, but not other places.
   */
  def termsIterator: Iterator[Term[C]]

  /**
   * Returns a map from exponent to coefficient of this polynomial.
   */
  def data(implicit ring: Semiring[C], eq: Eq[C]): Map[Int, C] = {
    val bldr = scala.collection.mutable.Map.newBuilder[Int, C]
    foreachNonZero { (e, c) =>
      bldr += ((e, c))
    }
    bldr.result().toMap
  }

  /**
   * Returns the real roots of this polynomial.
   *
   * Depending on `C`, the `finder` argument may need to be passed "explicitly" via an implicit conversion. This is
   * because some types (eg `BigDecimal`, `Rational`, etc) require an error bound, and so provide implicit conversions
   * to `RootFinder`s from the error type. For instance, `BigDecimal` requires either a scale or MathContext. So, we'd
   * call this method with `poly.roots(MathContext.DECIMAL128)`, which would return a `Roots[BigDecimal` whose roots are
   * approximated to the precision specified in `DECIMAL128` and rounded appropriately.
   *
   * On the other hand, a type like `Double` doesn't require an error bound and so can be called without specifying the
   * `RootFinder`.
   *
   * @param finder
   *   a root finder to extract roots with
   * @return
   *   the real roots of this polynomial
   */
  def roots(implicit finder: RootFinder[C]): Roots[C] =
    finder.findRoots(this)

  /**
   * Returns the coefficient of the n-th degree term.
   */
  def nth(n: Int)(implicit ring: Semiring[C]): C

  /**
   * Returns the term of the highest degree in this polynomial.
   */
  def maxTerm(implicit ring: Semiring[C]): Term[C] = Term(maxOrderTermCoeff, degree)

  /**
   * Returns the non-zero term of the minimum degree in this polynomial, unless it is zero. If this polynomial is zero,
   * then this returns a zero term.
   */
  @nowarn
  def minTerm(implicit ring: Semiring[C], eq: Eq[C]): Term[C] = {
    foreachNonZero { (n, c) =>
      return Term(c, n)
    }
    Term(ring.zero, 0)
  }

  /**
   * Returns `true` iff this polynomial is constant.
   */
  def isConstant: Boolean =
    degree == 0

  /**
   * Returns the degree of this polynomial.
   */
  def degree: Int

  /**
   * Returns the coefficient of max term of this polynomial.
   */
  def maxOrderTermCoeff(implicit ring: Semiring[C]): C

  /**
   * Returns a polynomial with the max term removed.
   */
  def reductum(implicit e: Eq[C], ring: Semiring[C], ct: ClassTag[C]): Polynomial[C]

  /**
   * Returns `true` if this polynomial is `ring.zero`.
   */
  def isZero: Boolean

  /**
   * Evaluate the polynomial at `x`.
   */
  def apply(x: C)(implicit r: Semiring[C]): C

  def evalWith[A: Semiring: Eq: ClassTag](x: A)(f: C => A): A =
    this.map(f).apply(x)

  /**
   * Compose this polynomial with another.
   */
  def compose(y: Polynomial[C])(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = {
    var polynomial: Polynomial[C] = Polynomial.zero[C]
    foreachNonZero { (e, c) =>
      val z: Polynomial[C] = y.pow(e) :* c
      polynomial = polynomial + z
    }
    polynomial
  }

  /**
   * Shift this polynomial along the x-axis by `h`, so that `this(x + h) == this.shift(h).apply(x)`. This is equivalent
   * to calling `this.compose(Polynomial.x + h)`, but is likely to compute the shifted polynomial much faster.
   */
  def shift(h: C)(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C] = {
    // The trick here came from this answer:
    //   http://math.stackexchange.com/questions/694565/polynomial-shift
    // This is a heavily optimized version of the same idea. This is fairly
    // critical method to be fast, since it is the most expensive part of the
    // VAS root isolation algorithm.

    def fromSafeLong(x: SafeLong): C =
      if (x.isValidInt) {
        ring.fromInt(x.toInt)
      } else {
        val d = ring.fromInt(1 << 30)
        val mask = (1L << 30) - 1

        @tailrec def loop(k: C, y: SafeLong, acc: C): C =
          if (y.isValidInt) {
            k * ring.fromInt(y.toInt) + acc
          } else {
            val z = y >> 30
            val r = ring.fromInt((y & mask).toInt)
            loop(d * k, z, k * r + acc)
          }

        loop(ring.one, x, ring.zero)
      }

    // The basic idea here is that instead of working with all the derivatives
    // of the whole polynomial, we can just break the polynomial up and work
    // with the derivatives of the individual terms. This let's us save a whole
    // bunch of allocations in a clean way.
    val coeffs: Array[C] = this.coeffsArray.clone()
    this.foreachNonZero { (deg, c) =>
      var i = 1 // Leading factor in factorial in denominator of Taylor series.
      var d = deg - 1 // The degree of the current derivative of this term.
      var m = SafeLong(1L) // The multiplier of our derivative
      var k = c // The current delta (to some power) of the Taylor series.
      while (d >= 0) {
        // Note that we do division, but only on SafeLongs. This is not just
        // for performance, but also required for us to only ask for a Ring,
        // rather than a EuclideanRing. We always know that m * (d + 1) is
        // divisible by i, so this is exact.
        m = m * (d + 1) / i
        k *= h
        coeffs(d) = coeffs(d) + fromSafeLong(m) * k
        d -= 1
        i += 1
      }
    }
    Polynomial.dense(coeffs)
  }

  /**
   * Returns this polynomial as a monic polynomial, where the leading coefficient (ie. `maxOrderTermCoeff`) is 1.
   */
  def monic(implicit f: Field[C], eq: Eq[C]): Polynomial[C] = this :/ maxOrderTermCoeff

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C]
  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C]

  /**
   * Returns the number of sign variations in the coefficients of this polynomial. Given 2 consecutive terms (ignoring 0
   * terms), a sign variation is indicated when the terms have differing signs.
   */
  def signVariations(implicit ring: Semiring[C], order: Order[C], signed: Signed[C]): Int = {
    var prevSign: Sign = Signed.Zero
    var variations = 0
    foreachNonZero { (_, c) =>
      val sign = signed.sign(c)
      if (Signed.Zero != prevSign && sign != prevSign) {
        variations += 1
      }
      prevSign = sign
    }
    variations
  }

  /**
   * Removes all zero roots from this polynomial.
   */
  def removeZeroRoots(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    val Term(_, k) = minTerm
    mapTerms { case Term(c, n) => Term(c, n - k) }
  }

  def map[D: Semiring: Eq: ClassTag](f: C => D): Polynomial[D] =
    mapTerms { case Term(c, n) => Term(f(c), n) }

  def mapTerms[D: Semiring: Eq: ClassTag](f: Term[C] => Term[D]): Polynomial[D] =
    Polynomial(termsIterator.map(f))

  /**
   * This will flip/mirror the polynomial about the y-axis. It is equivalent to `poly.compose(-Polynomial.x)`, but will
   * likely be faster to calculate.
   */
  def flip(implicit ring: Rng[C], eq: Eq[C]): Polynomial[C] =
    mapTerms { case term @ Term(coeff, exp) =>
      if (exp % 2 == 0) term
      else Term(-coeff, exp)
    }

  /**
   * Returns the reciprocal of this polynomial. Essentially, if this polynomial is `p` with degree `n`, then returns a
   * polynomial `q(x) = x^n*p(1/x)`.
   *
   * @see
   *   http://en.wikipedia.org/wiki/Reciprocal_polynomial
   */
  def reciprocal(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = {
    val d = degree
    mapTerms { case term @ Term(coeff, exp) =>
      Term(coeff, d - exp)
    }
  }

  def unary_-(implicit ring: Rng[C]): Polynomial[C]
  def +(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def -(rhs: Polynomial[C])(implicit ring: Rng[C], eq: Eq[C]): Polynomial[C] = lhs + -rhs
  def *(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]

  def **(k: Int)(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = pow(k)

  def pow(k: Int)(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = {
    def loop(b: Polynomial[C], k: Int, extra: Polynomial[C]): Polynomial[C] =
      if (k == 1)
        b * extra
      else
        loop(b * b, k >>> 1, if ((k & 1) == 1) b * extra else extra)

    if (k < 0) {
      throw new IllegalArgumentException("negative exponent")
    } else if (k == 0) {
      Polynomial.one[C]
    } else if (k == 1) {
      this
    } else {
      loop(this, k - 1, this)
    }
  }

  // VectorSpace ops.

  def *:(k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def :*(k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = k *: lhs
  def :/(k: C)(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = this :* k.reciprocal

  override def hashCode: Int = {
    val it = lhs.termsIterator
    @tailrec def loop(n: Int): Int =
      if (it.hasNext) {
        val term = it.next()
        loop(n ^ (0xfeed1257 * term.exp ^ term.coeff.##))
      } else n
    loop(0)
  }

  override def equals(that: Any): Boolean = that match {
    case rhs: Polynomial[_] if lhs.degree == rhs.degree =>
      val it1 = lhs.termsIterator
      val it2 = rhs.termsIterator
      @tailrec def loop(): Boolean = {
        val has1 = it1.hasNext
        val has2 = it2.hasNext
        if (has1 && has2) {
          if (it1.next() == it2.next()) loop() else false
        } else has1 == has2
      }
      loop()

    case rhs: Polynomial[_] =>
      false

    case n if lhs.isZero =>
      n == 0

    case n if lhs.degree == 0 =>
      val (_, lcs) = Polynomial.split(lhs)
      lcs(0) == n

    case _ =>
      false
  }

  override def toString: String =
    if (isZero) {
      "(0)"
    } else {
      val bldr = arrayBuilderMake[Term[C]]
      foreach { (e, c) => bldr += Term(c, e) }

      val ts = bldr.result()
      QuickSort.sort(ts)(Order.reverse(Order[Term[C]]), implicitly[ClassTag[Term[C]]])
      val s = ts.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}

trait PolynomialOverSemiring[@sp(Double) C] extends Semiring[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def zero: Polynomial[C] = Polynomial.zero[C]
  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x + y
  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x * y
}

trait PolynomialOverRig[@sp(Double) C] extends PolynomialOverSemiring[C] with Rig[Polynomial[C]] {
  implicit override val scalar: Rig[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

trait PolynomialOverRng[@sp(Double) C] extends PolynomialOverSemiring[C] with Rng[Polynomial[C]] {
  implicit override val scalar: Rng[C]

  def timesl(r: C, v: Polynomial[C]): Polynomial[C] = r *: v
  def negate(x: Polynomial[C]): Polynomial[C] = -x
}

trait PolynomialOverRing[@sp(Double) C] extends PolynomialOverRng[C] with Ring[Polynomial[C]] {
  implicit override val scalar: Ring[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

// we skip the CSemiring, CRng, CRig instances

trait PolynomialOverCRing[@sp(Double) C]
    extends CRing[Polynomial[C]]
    with PolynomialOverRing[C]
    with RingAssociativeAlgebra[Polynomial[C], C] {
  implicit override val scalar: CRing[C]
}

trait PolynomialOverField[@sp(Double) C]
    extends PolynomialOverRing[C]
    with EuclideanRing[Polynomial[C]]
    with VectorSpace[Polynomial[C], C]
    with FieldAssociativeAlgebra[Polynomial[C], C] { self =>
  implicit override val scalar: Field[C]

  override def divr(x: Polynomial[C], k: C): Polynomial[C] = x :/ k

  def euclideanFunction(x: Polynomial[C]): BigInt = x.degree
  def equot(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = equotmod(x, y)._1
  def emod(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = equotmod(x, y)._2
  override def equotmod(x: Polynomial[C], y: Polynomial[C]): (Polynomial[C], Polynomial[C]) = {
    require(!y.isZero, "Can't divide by polynomial of zero!")
    (x: @unchecked) match {
      case xd: poly.PolyDense[C] => poly.PolyDense.quotmodDense(xd, y)
      case xs: poly.PolySparse[C] =>
        val ys = (y: @unchecked) match {
          case yd: poly.PolyDense[C]   => poly.PolySparse.dense2sparse(yd)
          case ys1: poly.PolySparse[C] => ys1
        }
        poly.PolySparse.quotmodSparse(xs, ys)
    }
  }
}

trait PolynomialEq[@sp(Double) C] extends Eq[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def eqv(x: Polynomial[C], y: Polynomial[C]): Boolean =
    x.coeffsArray === y.coeffsArray // TODO: This is bad for sparse arrays. Do better.
}

trait PolynomialInstances0 {
  implicit def overSemiring[@sp(Double) C: ClassTag: Semiring: Eq]: PolynomialOverSemiring[C] =
    new PolynomialOverSemiring[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def eq[@sp(Double) C: ClassTag: Semiring: Eq]: PolynomialEq[C] =
    new PolynomialEq[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances1 extends PolynomialInstances0 {
  implicit def overRig[@sp(Double) C: ClassTag: Rig: Eq]: PolynomialOverRig[C] =
    new PolynomialOverRig[C] {
      val scalar = Rig[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def overRng[@sp(Double) C: ClassTag: Rng: Eq]: PolynomialOverRng[C] =
    new PolynomialOverRng[C] {
      val scalar = Rng[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances2 extends PolynomialInstances1 {
  implicit def overRing[@sp(Double) C: ClassTag: Ring: Eq]: PolynomialOverRing[C] =
    new PolynomialOverRing[C] {
      val scalar = Ring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances3 extends PolynomialInstances2 {
  implicit def overCRing[@sp(Double) C: ClassTag: CRing: Eq]: PolynomialOverCRing[C] =
    new PolynomialOverCRing[C] {
      val scalar = CRing[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances4 extends PolynomialInstances3 {
  implicit def overField[@sp(Double) C: ClassTag: Field: Eq]: PolynomialOverField[C] =
    new PolynomialOverField[C] {
      val scalar = Field[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances extends PolynomialInstances4
