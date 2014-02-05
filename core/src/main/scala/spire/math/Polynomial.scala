package spire.math

import scala.collection.mutable.ArrayBuilder

import compat._
import spire.math.poly._
import scala.annotation.tailrec
import scala.reflect._
import spire.algebra._
import spire.implicits._

import scala.{specialized => spec}

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
  def quadratic[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map(2 -> c))
  def cubic[@spec(Double) C: Eq: Semiring: ClassTag](c: C): Polynomial[C] =
    if (c === Semiring[C].zero) zero[C] else Polynomial(Map(3 -> c))
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
<<<<<<< HEAD
    if (vs.size > 1) sys.error("only univariate polynomials please, you might require MultivariatePolynomial.")
=======
    if (vs.size > 1) throw new IllegalArgumentException("only univariate polynomials supported")
>>>>>>> FETCH_HEAD

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
}

trait Polynomial[@spec(Double) C] { lhs =>
  implicit def ct: ClassTag[C]

  /** Returns a polynmial that has a dense representation. */
  def toDense(implicit ring: Semiring[C], eq: Eq[C]): PolyDense[C]

  /** Returns a polynomial that has a sparse representation. */
  def toSparse(implicit ring: Semiring[C], eq: Eq[C]): PolySparse[C]

  def foreach[U](f: (Int, C) => U): Unit

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
