package spire.math

import scala.collection.mutable.ArrayBuilder

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => spec}

import java.math.{ BigDecimal => JBigDecimal, RoundingMode, MathContext }

import spire.algebra._
import spire.math.poly._
import spire.std.array._
import spire.std.bigInt._
import spire.std.bigDecimal._
import spire.syntax.field._
import spire.syntax.eq._
import spire.syntax.signed._
import spire.syntax.std.seq._

/**
 * Polynomial
 * A univariate polynomial class and EuclideanRing extension trait 
 * for arithmetic operations. Polynomials can be instantiated using 
 * any type C for which a Ring[C] and Signed[C] are in scope, with 
 * exponents given by Int values. Some operations require a Field[C]
 * to be in scope.
*/

object Polynomial extends PolynomialInstances {

  def dense[@spec(Double) C: Semiring: Eq: ClassTag](coeffs: Array[C]): PolyDense[C] = {
    var i = coeffs.length
    while (i > 0 && (coeffs(i - 1) === Semiring[C].zero)) i -= 1
    if (i == coeffs.length) {
      new PolyDense(coeffs)
    } else {
      val cs = new Array[C](i)
      System.arraycopy(coeffs, 0, cs, 0, i)
      new PolyDense(cs)
    }
  }

  def sparse[@spec(Double) C: Semiring: Eq: ClassTag](data: Map[Int, C]): PolySparse[C] =
    PolySparse(data)

  def apply[@spec(Double) C: Semiring: Eq: ClassTag](data: Map[Int, C]): PolySparse[C] =
    sparse(data)

  def apply[@spec(Double) C: Semiring: Eq: ClassTag](terms: Iterable[Term[C]]): PolySparse[C] =
    sparse(terms.map(_.toTuple)(collection.breakOut))

  def apply[@spec(Double) C: Semiring: Eq: ClassTag](c: C, e: Int): PolySparse[C] =
    PolySparse.safe(Array(e), Array(c))

  import scala.util.{Try, Success, Failure}

  def apply(s: String): Polynomial[Rational] = parse(s)

  def zero[@spec(Double) C: Eq: Semiring: ClassTag]: Polynomial[C] =
    PolySparse.zero[C]
  def constant[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map(0 -> c))
  def linear[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map(1 -> c))
  def quadratic[@spec(Double) C: Eq: Semiring: ClassTag](c1: C, c0: C): Polynomial[C] =
    Polynomial(Map(1 -> c1, 0 -> c0))
  def quadratic[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map(2 -> c))
  def quadratic[@spec(Double) C: Eq: Semiring: ClassTag](c2: C, c1: C, c0: C): Polynomial[C] =
    Polynomial(Map(2 -> c2, 1 -> c1, 0 -> c0))
  def cubic[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map(3 -> c))
  def cubic[@spec(Double) C: Eq: Semiring: ClassTag](c3: C, c2: C, c1: C, c0: C): Polynomial[C] =
    Polynomial(Map(3 -> c3, 2 -> c2, 1 -> c1, 0 -> c0))
  def one[@spec(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    constant(Rig[C].one)
  def x[@spec(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
    linear(Rig[C].one)
  def twox[@spec(Double) C: Eq: Rig: ClassTag]: Polynomial[C] =
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
          case None => if (ts.isEmpty) ("+", s) else throw new IllegalArgumentException(s)
        }

        val m2 = termRe.findPrefixMatchOf(s2).getOrElse(throw new IllegalArgumentException(s2))
        val c0 = Option(m2.group(1)).getOrElse("1")
        val c = if (op == "-") "-" + c0 else c0
        val v = Option(m2.group(2)).getOrElse("")
        val e0 = Option(m2.group(3)).getOrElse("")
        val e = if (e0 != "") e0 else if (v == "") "0" else "1"

        val t = try {
          T(Rational(c), v, e.toInt)
        } catch {
          case _: Exception => throw new IllegalArgumentException(s"illegal term: $c*x^$e")
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
    if (vs.size > 1) throw new IllegalArgumentException("only univariate polynomials supported")

    // we're done!
    Polynomial(ts.map(t => (t.e, t.c)).toMap)
  }

  private final def split[@spec(Double) C: ClassTag](poly: Polynomial[C]): (Array[Int], Array[C]) = {
    val es = ArrayBuilder.make[Int]()
    val cs = ArrayBuilder.make[C]()
    poly foreach { (e, c) =>
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
        case (x, y) :: tail =>
          val c = Polynomial.constant((y - p(x)) / xs.map(x - _).qproduct)
          val prod = xs.foldLeft(Polynomial.one[C]) { (prod, xn) =>
            prod * (Polynomial.x[C] - constant(xn))
          }
          loop(p + c * prod, x :: xs, tail)
      }
    loop(Polynomial.zero[C], Nil, points.toList)
  }

  private implicit object JBigDecimalOrder extends Signed[JBigDecimal] with Order[JBigDecimal] {
    def signum(a: JBigDecimal): Int = a.signum
    def abs(a: JBigDecimal): JBigDecimal = a.abs
    def compare(x: JBigDecimal, y: JBigDecimal): Int = x compareTo y
  }

  def QIR(poly: Polynomial[Rational], lowerBound: Rational, upperBound: Rational, digits: Int): Interval[BigDecimal] = {

    // TODO: See if Algebraic is faster.
    def evalExact(x: JBigDecimal): JBigDecimal =
      poly(Rational(BigDecimal(x, MathContext.UNLIMITED)))
        .toBigDecimal(digits, RoundingMode.UP)
        .bigDecimal

    val lb = lowerBound.toBigDecimal(digits, RoundingMode.CEILING).bigDecimal
    val ub = upperBound.toBigDecimal(digits, RoundingMode.FLOOR).bigDecimal

    // TODO: Re-use evalExact for lb/ub in QIR.
    if (poly(lowerBound).sign != evalExact(lb).sign) {
      Interval.point(BigDecimal(lb))
    } else if (poly(upperBound).sign != evalExact(ub).sign) {
      Interval.point(BigDecimal(ub))
    } else {
      QIR(lb, ub, digits)(evalExact).mapBounds(BigDecimal(_))
    }
  }

  /**
   * An implementation of "Quadratic Interval Refinement for Real Roots" by
   * John Abbot for `BigDecimal`.
   */
  def QIR(lowerBound: JBigDecimal, upperBound: JBigDecimal, digits: Int)(evalExact: JBigDecimal => JBigDecimal): Interval[JBigDecimal] = {
    val eps = JBigDecimal.valueOf(1, digits)

    @tailrec
    def loop(
      lx: JBigDecimal,
      ly: JBigDecimal,
      rx: JBigDecimal,
      ry: JBigDecimal,
      n: Int
    ): Interval[JBigDecimal] = {
      val dy = ly.subtract(ry)
      val s = ly.divide(dy, n, RoundingMode.HALF_UP)
      val dx = rx.subtract(lx)
      if (dx.compareTo(eps) <= 0) {
        Interval.open(lx, rx)
      } else {
        val delta = dx.multiply(s.ulp).setScale(digits + 1, RoundingMode.HALF_UP)
        val k = s.unscaledValue.intValue
        val x1 = lx.add(delta.multiply(new JBigDecimal(k))).setScale(digits, RoundingMode.HALF_UP)
        val y1 = evalExact(x1)
        val s1 = y1.sign
        if (s1 == ly.sign) {
          val x2 = x1.add(delta).setScale(digits, RoundingMode.CEILING)
          val y2 = evalExact(x2)
          val s2 = y2.sign
          if (s2 == s1) loop0(lx, ly, rx, ry)
          else if (s2 == ry.sign) loop(x1, y1, x2, y2, 2 * n)
          else Interval.point(x2)
        } else if (s1 == ry.sign) {
          val x0 = x1.subtract(delta).setScale(digits, RoundingMode.FLOOR)
          val y0 = evalExact(x0)
          val s0 = y0.sign
          if (s0 == s1) loop0(lx, ly, rx, ry)
          else if (s0 == ly.sign) loop(x0, y0, x1, y1, 2 * n)
          else Interval.point(x0)
        } else {
          Interval.point(x1)
        }
      }
    }

    def bisect(
      x0: JBigDecimal,
      y0: JBigDecimal,
      x1: JBigDecimal,
      y1: JBigDecimal,
      x2: JBigDecimal,
      y2: JBigDecimal
    ): Interval[JBigDecimal] = {
      if (y0.signum == 0) Interval.point(x0)
      else if (y1.signum == 0) Interval.point(x1)
      else if (y2.signum == 0) Interval.point(x2)
      else if (y0.sign != y1.sign) loop(x0, y0, x1, y1, 1)
      else loop(x1, y1, x2, y2, 1)
    }

    def loop0(
      x0: JBigDecimal,
      y0: JBigDecimal,
      x5: JBigDecimal,
      y5: JBigDecimal
    ): Interval[JBigDecimal] = {
      val dy = y0.subtract(y5)
      val k = y0
        .divide(dy, 1, RoundingMode.HALF_UP)
        .unscaledValue
        .intValue
      val eps = x5.subtract(x0).divide(new JBigDecimal(5))
      def eval(k: Int): (JBigDecimal, JBigDecimal) = {
        val x = new JBigDecimal(k).multiply(eps).add(x0)
        val y = evalExact(x)
        (x, y)
      }

      if (k < 5) {
        val (x2, y2) = eval(2)
        if (y2.sign != y0.sign) {
          val (x1, y1) = eval(1)
          bisect(x0, y0, x1, y1, x2, y2)
        } else {
          val (x3, y3) = eval(3)
          if (y3.sign == y5.sign) {
            loop(x2, y2, x3, y3, 1)
          } else {
            val (x4, y4) = eval(4)
            bisect(x3, y3, x4, y4, x5, y5)
          }
        }
      } else {
        val (x3, y3) = eval(3)
        if (y3.sign != y5.sign) {
          val (x4, y4) = eval(4)
          bisect(x3, y3, x4, y4, x5, y5)
        } else {
          val (x2, y2) = eval(2)
          if (y2.sign == y0.sign) {
            loop(x2, y2, x3, y3, 1)
          } else {
            val (x1, y1) = eval(1)
            bisect(x0, y0, x1, y1, x2, y2)
          }
        }
      }
    }

    val lx = lowerBound
    val ly = evalExact(lx)
    val rx = upperBound
    val ry = evalExact(rx)
    val refined = loop0(lx, ly, rx, ry)
    Interval.fromBounds(
      refined.lowerBound.map(_.setScale(digits, RoundingMode.FLOOR)),
      refined.upperBound.map(_.setScale(digits, RoundingMode.CEILING))
    )
  }

  /**
   * Isolates the roots of the [[Rational]] polynomial `poly`. This returns a
   * sequence of intervals that each contain a single root of `poly`. A root
   * will appear in the sequence as many times as its multiplicity in the
   * polynomial. Other than this, all root intervals are disjoint and are
   * either open on both ends or is a single point.
   */
  final def isolateRoots(poly: Polynomial[Rational]): Vector[Interval[Rational]] = {
    val coeffs = poly.coeffsArray
    val factors = coeffs.foldLeft(BigInt(1)) { (acc, coeff) =>
      val d = coeff.denominator
      acc * (d / acc.gcd(d))
    }
    val zCoeffs = coeffs.map(n => n.numerator * (factors / n.denominator))
    val zPoly = dense(zCoeffs)
    VAS(zPoly)
  }

  /**
   * An implementation of the VAS real root isolation algorithm.
   *
   * See "A Comparative Study of Two Real Root Isolation Methods" for the paper
   * that originally presented the method implemented here, and "Complexity
   * Analysis of Algorithms in Algebraic Computation" by Vikram Sharma which
   * goes into greater detail.
   */
  final def VAS(poly: Polynomial[BigInt]): Vector[Interval[Rational]] = {
    import spire.std.bigInt._

    val x = Polynomial.x[BigInt]
    val one = Polynomial.one[BigInt]

    // Return an upper bound on the roots of the polynomial p.
    def upperBound(p: Polynomial[BigInt]): Int = {
      val lgLastCoeff = p.maxOrderTermCoeff.abs.bitLength
      val n = p.degree
      var maxBound = Double.NegativeInfinity
      p.foreachNonZero { (k, coeff) =>
        if (k != n) {
          val i = n - k
          val bound = ((coeff.abs.bitLength - lgLastCoeff - 1) / i) + 2
          maxBound = max(maxBound, bound.toDouble)
        }
      }
      if (maxBound.isValidInt) {
        maxBound.toInt
      } else {
        throw new ArithmeticException("bound too large")
      }
    }

    // Return a lower bound on the roots of the polynomial p.
    def lowerBound(p: Polynomial[BigInt]): Int =
      -upperBound(p.reciprocal)

    // Find all roots recursively that are between (0, 1) and (1, infinity).
    def split1(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt): Vector[Interval[Rational]] = {
      val r = p.compose(x + one)
      val rRoots = rec(r, a, b + a, c, d + c)
      if (r.signVariations < p.signVariations) {
        var l = p.reciprocal.compose(x + one)
        while (l(0) == 0)
          l = l.mapTerms { case Term(coeff, exp) => Term(coeff, exp - 1) }
        val lRoots = rec(l, b, a + b, d, c + d)
        lRoots ++ rRoots
      } else {
        rRoots
      }
    }

    // Isolate all positive roots in polynomial p.
    def rec(p: Polynomial[BigInt], a: BigInt, b: BigInt, c: BigInt, d: BigInt): Vector[Interval[Rational]] = {
      if (p(BigInt(0)) == BigInt(0)) {
        val p0 = p.mapTerms { case Term(coeff, exp) => Term(coeff, exp - 1) }
        Interval.point(Rational(c, d)) +: rec(p0, a, b, c, d)
      } else {
        p.signVariations match {
          case 0 => // No roots.
            Vector.empty

          case 1 => // Isolated exactly 1 root.
            def ub = Rational(BigInt(1) << upperBound(p))
            val i0 = if (c == 0) ub else Rational(a, c)
            val i1 = if (d == 0) ub else Rational(b, d)
            if (i0 < i1) Vector(Interval.open(i0, i1))
            else Vector(Interval.open(i1, i0))

          case _ => // Exists 0 or 2 or more roots.
            val lb = lowerBound(p)
            if (lb < 0) {
              split1(p, a, b, c, d)
            } else {
              val flr = BigInt(1) << lb
              split1(p.compose(x + constant(flr)), a, b + a * flr, c, d + c * flr)
            }
        }
      }
    }

    val zeroInterval = Interval.point(Rational.zero)
    val posRoots = rec(poly, 1, 0, 0, 1)
    val negRoots = rec(poly.flip, 1, 0, 0, 1).map(-_).filter(_ != zeroInterval)
    negRoots ++ posRoots
  }
}

trait Polynomial[@spec(Double) C] { lhs =>
  implicit def ct: ClassTag[C]

  /** Returns a polynmial that has a dense representation. */
  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C]

  /** Returns a polynomial that has a sparse representation. */
  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C]

  /**
   * Traverses each term in this polynomial, in order of degree, lowest to
   * highest (eg. constant term would be first) and calls `f` with the degree
   * of term and its coefficient. This may skip zero terms, or it may not.
   */
  def foreach[U](f: (Int, C) => U): Unit

  /**
   * Traverses each non-zero term in this polynomial, in order of degree, lowest
   * to highest (eg. constant term would be first) and calls `f` with the degree
   * of term and its coefficient.
   */
  def foreachNonZero[U](f: (Int, C) => U)(implicit ring: Semiring[C], eq: Eq[C]): Unit =
    foreach { (e, c) => if (c =!= ring.zero) f(e, c) }

  /**
   * Returns the coefficients in little-endian order. So, the i-th element is
   * coeffsArray(i) * (x ** i).
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

  /** Returns a map from exponent to coefficient of this polynomial. */
  def data(implicit ring: Semiring[C], eq: Eq[C]): Map[Int, C] = {
    val bldr = new scala.collection.mutable.MapBuilder[Int, C, Map[Int, C]](Map.empty[Int, C])
    foreachNonZero { (e, c) =>
      bldr += ((e, c))
    }
    bldr.result()
  }

  /** Returns the coefficient of the n-th degree term. */
  def nth(n: Int)(implicit ring: Semiring[C]): C

  /** Returns the term of the highest degree in this polynomial. */
  def maxTerm(implicit ring: Semiring[C]): Term[C] = Term(maxOrderTermCoeff, degree)

  /** Returns the degree of this polynomial. */
  def degree: Int

  /** Returns the coefficient of max term of this polynomial. */
  def maxOrderTermCoeff(implicit ring: Semiring[C]): C

  /** Returns a polynomial with the max term removed. */
  def reductum(implicit e: Eq[C], ring: Semiring[C], ct: ClassTag[C]): Polynomial[C]

  /** Returns `true` if this polynomial is `ring.zero`. */
  def isZero: Boolean

  /** Evaluate the polynomial at `x`. */
  def apply(x: C)(implicit r: Semiring[C]): C

  /** Compose this polynomial with another. */
  def compose(y: Polynomial[C])(implicit ring: Rig[C], eq: Eq[C]): Polynomial[C] = {
    var polynomial: Polynomial[C] = Polynomial.zero[C]
    foreachNonZero { (e, c) =>
      val z: Polynomial[C] = y.pow(e) :* c
      polynomial = polynomial + z
    }
    polynomial
  }

  /**
   * Returns this polynomial as a monic polynomial, where the leading
   * coefficient (ie. `maxOrderTermCoeff`) is 1.
   */
  def monic(implicit f: Field[C], eq: Eq[C]): Polynomial[C] = this :/ maxOrderTermCoeff

  def derivative(implicit ring: Ring[C], eq: Eq[C]): Polynomial[C]
  def integral(implicit field: Field[C], eq: Eq[C]): Polynomial[C]

  /**
   * Returns the number of sign variations in the coefficients of this
   * polynomial. Given 2 consecutive terms (ignoring 0 terms), a sign variation
   * is indicated when the terms have differing signs.
   */
  def signVariations(implicit ring: Semiring[C], eq: Eq[C], signed: Signed[C]): Int = {
    var prevSign: Sign = Sign.Zero
    var variations = 0
    foreachNonZero { (_, c) =>
      val sign = signed.sign(c)
      if (Sign.Zero != prevSign && sign != prevSign) {
        variations += 1
      }
      prevSign = sign
    }
    variations
  }

  def mapTerms[D: Semiring: Eq: ClassTag](f: Term[C] => Term[D])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[D] =
    Polynomial(terms map f)

  /**
   * Replace `x`, the variable, in this polynomial with `-x`. This will
   * flip/mirror the polynomial about the y-axis.
   */
  def flip(implicit ring: Rng[C], eq: Eq[C]): Polynomial[C] =
    mapTerms { case term @ Term(coeff, exp) =>
      if (exp % 2 == 0) term
      else Term(-coeff, exp)
    }

  /**
   * Returns the reciprocal of this polynomial. Essentially, if this polynomial
   * is `p` with degree `n`, then returns a polynomial `q(x) = x^n*p(1/x)`.
   *
   * @see http://en.wikipedia.org/wiki/Reciprocal_polynomial
   */
  def reciprocal(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] =
    mapTerms { case term @ Term(coeff, exp) =>
      Term(coeff, degree - exp)
    }

  // EuclideanRing ops.

  def unary_-()(implicit ring: Rng[C]): Polynomial[C]
  def +(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def -(rhs: Polynomial[C])(implicit ring: Rng[C], eq: Eq[C]): Polynomial[C] = lhs + (-rhs)
  def *(rhs: Polynomial[C])(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def /~(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = (lhs /% rhs)._1
  def /%(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): (Polynomial[C], Polynomial[C])
  def %(rhs: Polynomial[C])(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = (lhs /% rhs)._2

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

  def *: (k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C]
  def :* (k: C)(implicit ring: Semiring[C], eq: Eq[C]): Polynomial[C] = k *: lhs
  def :/ (k: C)(implicit field: Field[C], eq: Eq[C]): Polynomial[C] = this :* k.reciprocal

  override def equals(that: Any): Boolean = that match {
    case rhs: Polynomial[_] if lhs.degree == rhs.degree =>
      val (les, lcs) = Polynomial.split(lhs)
      val (res, rcs) = Polynomial.split[Any](rhs.asInstanceOf[Polynomial[Any]])

      @tailrec
      def loop(i: Int, j: Int): Boolean = {
        if (i >= les.length && j >= res.length) {
          true
        } else if (j >= res.length || les(i) < res(j)) {
          if (lcs(i) == 0) loop(i + 1, j) else false
        } else if (i >= les.length || les(i) > res(j)) {
          if (rcs(j) == 0) loop(i, j + 1) else false
        } else if (lcs(i) == rcs(j)) {
          loop(i + 1, j + 1)
        } else {
          false
        }
      }

      loop(0, 0)

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

  override def toString =
    if (isZero) {
      "(0)"
    } else {
      val bldr = ArrayBuilder.make[Term[C]]()
      foreach { (e, c) => bldr += Term(c, e) }

      val ts = bldr.result()
      QuickSort.sort(ts)(Order[Term[C]].reverse, implicitly[ClassTag[Term[C]]])
      val s = ts.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }
}

trait PolynomialSemiring[@spec(Double) C]
extends Semiring[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def zero: Polynomial[C] = Polynomial.zero[C]
  def plus(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x + y
  def times(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = x * y
}

trait PolynomialRig[@spec(Double) C] extends PolynomialSemiring[C]
with Rig[Polynomial[C]] {
  implicit override val scalar: Rig[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

trait PolynomialRng[@spec(Double) C] extends PolynomialSemiring[C]
with RingAlgebra[Polynomial[C], C] {
  implicit override val scalar: Rng[C]

  def timesl(r: C, v: Polynomial[C]): Polynomial[C] = r *: v
  def negate(x: Polynomial[C]): Polynomial[C] = -x
}

trait PolynomialRing[@spec(Double) C] extends PolynomialRng[C]
with Ring[Polynomial[C]] {
  implicit override val scalar: Ring[C]

  def one: Polynomial[C] = Polynomial.one[C]
}

trait PolynomialEuclideanRing[@spec(Double) C] extends PolynomialRing[C]
with EuclideanRing[Polynomial[C]] with VectorSpace[Polynomial[C], C] {
  implicit override val scalar: Field[C]

  override def divr(x: Polynomial[C], k: C): Polynomial[C] = x :/ k
  def quot(x: Polynomial[C], y: Polynomial[C]) = x /~ y
  def mod(x: Polynomial[C], y: Polynomial[C]) = x % y
  override def quotmod(x: Polynomial[C], y: Polynomial[C]) = x /% y

  final def gcd(x: Polynomial[C], y: Polynomial[C]): Polynomial[C] = {
    val k = spire.math.gcd(x.coeffsArray ++ y.coeffsArray)
    k *: euclid(x :/ k, y :/ k)(Polynomial.eq).monic
  }
}

trait PolynomialEq[@spec(Double) C] extends Eq[Polynomial[C]] {
  implicit def scalar: Semiring[C]
  implicit def eq: Eq[C]
  implicit def ct: ClassTag[C]

  def eqv(x: Polynomial[C], y: Polynomial[C]): Boolean =
    x.coeffsArray === y.coeffsArray // TODO: This is bad for sparse arrays. Do better.
}

trait PolynomialInstances0 {
  implicit def semiring[@spec(Double) C: ClassTag: Semiring: Eq] =
    new PolynomialSemiring[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def eq[@spec(Double) C: ClassTag: Semiring: Eq] =
    new PolynomialEq[C] {
      val scalar = Semiring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances1 extends PolynomialInstances0 {
  implicit def rig[@spec(Double) C: ClassTag: Rig: Eq] =
    new PolynomialRig[C] {
      val scalar = Rig[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }

  implicit def rng[@spec(Double) C: ClassTag: Rng: Eq] =
    new PolynomialRng[C] {
      val scalar = Rng[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances2 extends PolynomialInstances1 {
  implicit def ring[@spec(Double) C: ClassTag: Ring: Eq] =
    new PolynomialRing[C] {
      val scalar = Ring[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances3 extends PolynomialInstances2 {
  implicit def euclideanRing[@spec(Double) C: ClassTag: Field: Eq] =
    new PolynomialEuclideanRing[C] {
      val scalar = Field[C]
      val eq = Eq[C]
      val ct = implicitly[ClassTag[C]]
    }
}

trait PolynomialInstances extends PolynomialInstances3
