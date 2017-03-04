package spire
package math

import scala.math._
import scala.reflect._
import spire.algebra._
import spire.std.ArraySupport
import spire.syntax.isReal._
import spire.syntax.nroot._
import spire.syntax.vectorSpace._

/**
 * Used to implicitly define the dimensionality of the Jet space.
 * @param dimension the number of dimensions.
 */
case class JetDim(dimension: Int) {
  require(dimension > 0)
}

// scalastyle:off regex
/**
 * ==Overview==
 * A simple implementation of N-dimensional dual numbers, for automatically
 * computing exact derivatives of functions. This code (and documentation) closely follow
 * the one in Google's "Ceres" library of non-linear least-squares solvers
 * (see <a href="http://code.google.com/p/ceres-solver">Sameer Agarwal, Keir Mierle, and others: Ceres Solver.</a>)
 *
 * While a complete treatment of the mechanics of automatic differentiation is
 * beyond the scope of this header (see
 * http://en.wikipedia.org/wiki/Automatic_differentiation for details), the
 * basic idea is to extend normal arithmetic with an extra element "h" such
 * that h != 0, but h^2^ = 0.
 * Dual numbers are extensions of the real numbers analogous to complex numbers:
 * whereas complex numbers augment the reals by introducing an imaginary unit i
 * such that i^2^ = -1, dual numbers introduce an "infinitesimal" unit h such
 * that h^2^ = 0.
 * Analogously to a complex number c = x + y*i, a dual number d = x * y*h has
 * two components: the "real" component x, and an "infinitesimal" component y.
 * Surprisingly, this leads to a convenient method for computing exact
 * derivatives without needing to manipulate complicated symbolic expressions.

 * For example, consider the function
 * {{{
 *   f(x) = x * x ,
 * }}}
 * evaluated at 10. Using normal arithmetic, f(10) = 100, and df/dx(10) = 20.
 * Next, augment 10 with an infinitesimal h to get:
 * {{{
 *   f(10 + h) = (10 + h) * (10 + h)
 *             = 100 + 2 * 10 * h + h * h
 *             = 100 + 20 * h       +---
 *                     +-----       |
 *                     |            +--- This is zero
 *                     |
 *                     +----------------- This is df/dx
 * }}}
 * Note that the derivative of f with respect to x is simply the infinitesimal
 * component of the value of f(x + h). So, in order to take the derivative of
 * any function, it is only necessary to replace the numeric "object" used in
 * the function with one extended with infinitesimals. The class Jet, defined in
 * this header, is one such example of this, where substitution is done with
 * generics.
 *
 * To handle derivatives of functions taking multiple arguments, different
 * infinitesimals are used, one for each variable to take the derivative of. For
 * example, consider a scalar function of two scalar parameters x and y:
 * {{{
 *   f(x, y) = x * x + x * y
 * }}}
 * Following the technique above, to compute the derivatives df/dx and df/dy for
 * f(1, 3) involves doing two evaluations of f, the first time replacing x with
 * x + h, the second time replacing y with y + h.
 *
 * For df/dx:
 * {{{
 *   f(1 + h, y) = (1 + h) * (1 + h) + (1 + h) * 3
 *               = 1 + 2 * h + 3 + 3 * h
 *               = 4 + 5 * h
 *
 *   Therefore df/dx = 5
 * }}}
 * For df/dy:
 * {{{
 *   f(1, 3 + h) = 1 * 1 + 1 * (3 + h)
 *               = 1 + 3 + h
 *               = 4 + h
 *
 *   Therefore df/dy = 1
 * }}}
 * To take the gradient of f with the implementation of dual numbers ("jets") in
 * this file, it is necessary to create a single jet type which has components
 * for the derivative in x and y, and pass them to a routine computing function f.
 * It is convenient to use a generic version of f, that can be called also with non-jet numbers
 * for standard evaluation:
 * {{{
 *   def f[@specialized(Double) T : Field](x: T, y: T): T = x * x + x * y
 *
 *   val xValue = 9.47892774
 *   val yValue = 0.287740

 *   // The "2" means there should be 2 dual number components.
 *   implicit val dimension = JetDim(2)
 *   val x: Jet[Double] = xValue + Jet.h[Double](0);  // Pick the 0th dual number for x.
 *   val y: Jet[Double] = yValue + Jet.h[Double](1);  // Pick the 1th dual number for y.
 *
 *   val z: Jet[Double] = f(x, y);
 *   println("df/dx = " + z.infinitesimal(0) + ", df/dy = " + z.infinitesimal(1));
 * }}}
 *
 * For the more mathematically inclined, this file implements first-order
 * "jets". A 1st order jet is an element of the ring
 *{{{
 *   T[N] = T[t_1, ..., t_N] / (t_1, ..., t_N)^2
 *}}}
 * which essentially means that each jet consists of a "scalar" value 'a' from T
 * and a 1st order perturbation vector 'v' of length N:
 *{{{
 *   x = a + \sum_i v[i] t_i
 *}}}
 * A shorthand is to write an element as x = a + u, where u is the perturbation.
 * Then, the main point about the arithmetic of jets is that the product of
 * perturbations is zero:
 * {{{
 *   (a + u) * (b + v) = ab + av + bu + uv
 *                     = ab + (av + bu) + 0
 * }}}
 * which is what operator* implements below. Addition is simpler:
 * {{{
 *   (a + u) + (b + v) = (a + b) + (u + v).
 * }}}
 * The only remaining question is how to evaluate the function of a jet, for
 * which we use the chain rule:
 * {{{
 *   f(a + u) = f(a) + f'(a) u
 * }}}
 * where f'(a) is the (scalar) derivative of f at a.
 *
 * By pushing these things through generics, we can write routines that at same time
 * evaluate mathematical functions and compute their derivatives through automatic differentiation.
 */
object Jet extends JetInstances {
  // No-arg c.tor makes a zero Jet
  def apply[@sp(Float, Double) T]()(implicit c: ClassTag[T], d: JetDim, s: Semiring[T])
      : Jet[T] = Jet(s.zero)

  // From real.
  def apply[@sp(Float, Double) T](real: T)(implicit c: ClassTag[T], d: JetDim, s: Semiring[T])
      : Jet[T] = new Jet(real, Array.fill[T](d.dimension)(s.zero))

  // From real, to compute k-th partial derivative.
  def apply[@sp(Float, Double) T](a: T, k: Int)(implicit c: ClassTag[T], d: JetDim, r: Rig[T])
      : Jet[T] = {
    val v = Array.fill[T](d.dimension)(r.zero)
    v(k) = r.one
    new Jet(a, v)
  }

  // Zero real, indicator for k-th partial derivative.
  def h[@sp(Float, Double) T](k: Int)(implicit c: ClassTag[T], d: JetDim, r: Rig[T]): Jet[T] =
      Jet(r.zero, k)

  def one[@sp(Float, Double) T](implicit c: ClassTag[T], d: JetDim, r: Rig[T]): Jet[T] = Jet(r.one)

  def zero[@sp(Float, Double) T](implicit c: ClassTag[T], d: JetDim, s: Semiring[T]): Jet[T] = Jet(s.zero)

  def fromInt[@sp(Float, Double) T](n: Int)(implicit c: ClassTag[T], d: JetDim, r: Ring[T]): Jet[T] =
      Jet(r.fromInt(n))

  implicit def intToJet(n: Int)(implicit d: JetDim): Jet[Double] = {
    doubleToJet(n.toDouble)
  }

  implicit def longToJet(n: Long)(implicit d: JetDim): Jet[Double] = {
    doubleToJet(n.toDouble)
  }

  implicit def floatToJet(n: Float)(implicit d: JetDim): Jet[Float] = {
    new Jet(n.toFloat, Array.fill[Float](d.dimension)(0.0f))
  }

  implicit def doubleToJet(n: Double)(implicit d: JetDim): Jet[Double] = {
    new Jet(n, Array.fill[Double](d.dimension)(0.0))
  }

  implicit def bigIntToJet(n: BigInt)(implicit d: JetDim): Jet[BigDecimal] = {
    bigDecimalToJet(BigDecimal(n))
  }

  implicit def bigDecimalToJet(n: BigDecimal)(implicit d: JetDim): Jet[BigDecimal] = {
    implicit val mc = n.mc
    new Jet(n, Array.fill[BigDecimal](d.dimension)(0.0))
  }
}

@SerialVersionUID(0L)
final case class Jet[@sp(Float, Double) T](real: T, infinitesimal: Array[T])
  extends ScalaNumber with ScalaNumericConversions with Serializable { lhs =>

  import spire.syntax.order._

  def dimension: Int = infinitesimal.size
  implicit def jetDimension: JetDim = JetDim(dimension)

  /**
   * This is consistent with abs
   */
  def signum()(implicit r: Signed[T]): Int = real.signum()

  def asTuple: (T, Array[T]) = (real, infinitesimal)

  def isReal: Boolean = infinitesimal.forall(anyIsZero)
  def isZero: Boolean = anyIsZero(real) && isReal
  def isInfinitesimal(implicit r: IsReal[T]): Boolean = anyIsZero(real) && !isReal

  def eqv(b: Jet[T])(implicit o: Eq[T]): Boolean = {
    real === b.real && ArraySupport.eqv(infinitesimal, b.infinitesimal)
  }
  def neqv(b: Jet[T])(implicit o: Eq[T]): Boolean = {
    !(this eqv b)
  }

  def unary_-()(implicit f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(-real, -infinitesimal)
  }

  def +(b: T)(implicit f: Field[T]): Jet[T] = new Jet(real + b, infinitesimal)
  def -(b: T)(implicit f: Field[T]): Jet[T] = new Jet(real - b, infinitesimal)
  def *(b: T)(implicit f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real * b, infinitesimal :* b)
  }
  def /(b: T)(implicit f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real / b, infinitesimal :/ b)
  }
  def +(b: Jet[T])(implicit f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real + b.real, infinitesimal + b.infinitesimal)
  }
  def -(b: Jet[T])(implicit f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real - b.real, infinitesimal - b.infinitesimal)
  }
  // Multiplication rule for differentials:
  //
  //    (a + du)(b + dv) ~= ab + a dv + b du
  //
  // because du dv ~= 0
  def *(b: Jet[T])(implicit f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real * b.real, b.real *: infinitesimal + real *: b.infinitesimal)
  }

  def /(b: Jet[T])(implicit f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    // Division rule for differentials:
    //
    //   a + du   (a + du)(b - dv)    ab - a dv + b du   a    a       1      a   1         a
    //   ------ = ---------------- ~= ---------------- = - - --- dv + - du = - + - * (du - - dv)
    //   b + dv   (b + dv)(b - dv)           b^2         b   b^2      b      b   b         b
    //
    // which holds because dv dv = du dv = 0.
    val br_inv: T = f.one / b.real
    val ar_div_br: T = real * br_inv
    new Jet(ar_div_br, br_inv *: (infinitesimal - (ar_div_br *: b.infinitesimal)))
  }

  def /~(b: Jet[T])(implicit c: ClassTag[T], f: Field[T], r: IsReal[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val q = this / b
    new Jet[T](q.real.floor(), q.infinitesimal.map(r.floor))
  }

  def %(b: Jet[T])(implicit c: ClassTag[T], f: Field[T], r: IsReal[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    this - ((this /~ b) * b)
  }

  def /%(b: Jet[T])(implicit c: ClassTag[T], f: Field[T], r: IsReal[T], v: VectorSpace[Array[T], T]): (Jet[T], Jet[T]) = {
    val q = this /~ b
    (q, this - (q * b))
  }

  def **(b: Int)(implicit e: Eq[T], f: Field[T], v: VectorSpace[Array[T], T]): Jet[T] = pow(b)

  def nroot(k: Int)(
      implicit e: Eq[T], f: Field[T], r: IsReal[T], t: Trig[T], v: VectorSpace[Array[T], T])
      : Jet[T] = {
    pow(f.fromInt(k).reciprocal())
  }

  def **(b: Jet[T])(implicit c: ClassTag[T], e: Eq[T], f: Field[T], r: IsReal[T], t: Trig[T],
        v: VectorSpace[Array[T], T]): Jet[T] = {
    pow(b)
  }

  def floor()(implicit c: ClassTag[T], r: IsReal[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real.floor(), infinitesimal.map(r.floor))
  }

  def ceil()(implicit c: ClassTag[T], r: IsReal[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real.ceil(), infinitesimal.map(r.ceil))
  }

  def round()(implicit c: ClassTag[T], r: IsReal[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(real.round(), infinitesimal.map(r.round))
  }

  // Elementary math functions
  // In general, f(a + du) ~= f(a) + f'(a) du .

  /**
   * abs(x + du) ~= x + du or -(x + du)
   */
  def abs()(implicit f: Field[T], r: IsReal[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    if (real < f.zero) new Jet(-real, -infinitesimal)
    else this
  }

  // spire.math. does not define this pow generically, so there it is
  private def powScalarToScalar(b: T, e: T)(implicit
      f: Field[T], eq: Eq[T], r: IsReal[T], t: Trig[T]): T = {
    if (e === f.zero) {
      f.one
    } else if (b === f.zero) {
      if (e < f.zero) throw new Exception("raising 0 to a negative power")
      else f.zero
    } else {
      spire.math.exp(e * spire.math.log(b))
    }
  }

  // pow -- base is a constant, exponent (this) is a differentiable function.
  // b^(p + du) ~= b^p + b^p * log(b) du
  def powScalarToJet(a: T)(implicit
      c: ClassTag[T], e: Eq[T], f: Field[T], m: Module[Array[T], T], r: IsReal[T], t: Trig[T]): Jet[T] = {
    if (isZero) {
      Jet.one[T]
    } else {
      val tmp = powScalarToScalar(a, real)
      new Jet(tmp, (spire.math.log(a) * tmp) *: infinitesimal)
    }
  }

  /**
   * pow -- base (this) is a differentiable function, exponent is a constant.
   * pow(a + du, p) ~= pow(a, p) + p * pow(a, p-1) du
   */
  def pow(p: T)(implicit
      e: Eq[T], f: Field[T], r: IsReal[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tmp: T = p * powScalarToScalar(real, p - f.one)
    new Jet(powScalarToScalar(real, p), tmp *: infinitesimal)
  }

  // As above, integer exponent.
  def pow(p: Int)(implicit f: Field[T], e: Eq[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tmp = p * f.pow(real, p - 1)
    new Jet(f.pow(real, p), tmp *: infinitesimal)
  }

  /**
   * pow -- both base (this) and exponent are differentiable functions.
   * (a + du)^(b + dv) ~= a^b + b * a^(b-1) du + a^b log(a) dv
   */
  def pow(b: Jet[T])(implicit c: ClassTag[T], e: Eq[T], f: Field[T], m: Module[Array[T], T], r: IsReal[T], t: Trig[T])
      : Jet[T] = {
    if (b.isZero) {
      Jet.one[T]
    } else {
      val tmp1 = powScalarToScalar(real, b.real)
      val tmp2 = b.real * powScalarToScalar(real, b.real - f.one)
      val tmp3 = tmp1 * spire.math.log(real)
      new Jet(tmp1, (tmp2 *: infinitesimal) + (tmp3 *: b.infinitesimal))
    }
  }

  /**
   * log(a + du) ~= log(a) + du / a
   */
  def log()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(spire.math.log(real), (f.one / real) *: infinitesimal)
  }

  /**
   * sqrt(a + du) ~= sqrt(a) + du / (2 sqrt(a))
   */
  def sqrt()(implicit f: Field[T], n: NRoot[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val sa = real.sqrt
    val oneHalf = f.one / (f.one + f.one)
    new Jet(sa, (oneHalf / sa) *: infinitesimal)
  }

  /**
   * acos(a + du) ~= acos(a) - 1 / sqrt(1 - a**2) du
   */
  def acos()(implicit f: Field[T], n: NRoot[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tmp = -f.one / spire.math.sqrt(f.one - real * real)
    new Jet(spire.math.acos(real), tmp *: infinitesimal)
  }

  /**
   * asin(a + du) ~= asin(a) - 1 / sqrt(1 - a**2) du
   */
  def asin()(implicit f: Field[T], n: NRoot[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tmp = f.one / spire.math.sqrt(f.one - real * real)
    new Jet(spire.math.asin(real), tmp *: infinitesimal)
  }

  /**
   * atan(a + du) ~= atan(a) + 1 / (1 + a**2) du
   */
  def atan()(implicit f: Field[T], n: NRoot[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tmp = f.one / (f.one + real * real)
    new Jet(spire.math.atan(real), tmp *: infinitesimal)
  }

  /**
   * Defined with "this" as the y coordinate: this.atan2(a) == atan2(this, a) == atan(this / a)
   * atan2(b + dv, a + du) ~= atan2(b, a) + (- b du + a dv) / (a^2 + b^2)
   */
  def atan2(a: Jet[T])(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tmp = f.one / (a.real * a.real + real * real)
    new Jet(spire.math.atan2(real, a.real),
            ((tmp * (-real)) *: a.infinitesimal) + ((tmp * a.real) *: infinitesimal))
  }

  /**
   * exp(a + du) ~= exp(a) + exp(a) du
   */
  def exp()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val ea = spire.math.exp(real)
    new Jet[T](ea, ea *: infinitesimal)
  }

  /**
   * sin(a + du) ~= sin(a) + cos(a) du
   */
  def sin()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(spire.math.sin(real), spire.math.cos(real) *: infinitesimal)
  }
  /**
   * sinh(a + du) ~= sinh(a) + cosh(a) du
   */
  def sinh()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(spire.math.sinh(real), spire.math.cosh(real) *: infinitesimal)
  }
  /**
   * cos(a + du) ~= cos(a) - sin(a) du
   */
  def cos()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(spire.math.cos(real), -spire.math.sin(real) *: infinitesimal)
  }
  /**
   * cosh(a + du) ~= cosh(a) + sinh(a) du
   */
  def cosh()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    new Jet(spire.math.cosh(real), spire.math.sinh(real) *: infinitesimal)
  }
  /**
   * tan(a + du) ~= tan(a) + (1 + tan(a)**2) du
   */
  def tan()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tan_a = spire.math.tan(real)
    val tmp = f.one + tan_a * tan_a
    new Jet(tan_a, tmp *: infinitesimal)
  }
  /**
   * tanh(a + du) ~= tanh(a) + (1 - tanh(a)**2) du
   */
  def tanh()(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Jet[T] = {
    val tanh_a = spire.math.tanh(real)
    val tmp = f.one - tanh_a * tanh_a
    new Jet(tanh_a, tmp *: infinitesimal)
  }

  // Stuff needed by ScalaNumber
  override def floatValue: Float = doubleValue.toFloat
  override def doubleValue: Double = anyToDouble(real)
  override def byteValue: Byte = longValue.toByte
  override def shortValue: Short = longValue.toShort
  override def intValue: Int = longValue.toInt
  override def longValue: Long = anyToLong(real)
  override def underlying: Object = this
  override def isWhole: Boolean = anyIsWhole(real) && isReal
  override def isValidInt: Boolean = anyIsValidInt(real) && isReal

  // Object stuff
  override def hashCode: Int = {
    if (isReal) real.##
    else 13 * real.## + infinitesimal.foldLeft(53)((x, y) => x + y.## * 19)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Jet[_] => this === that
    case that         => isReal && real == that
  }

  def ===(that: Jet[_]): Boolean =
    real == that.real && dimension == that.dimension &&
      infinitesimal.zip(that.infinitesimal).forall{ case (x, y) => x == y }

  def =!=(that: Jet[_]): Boolean =
    !(this === that)

  override def toString: String = {
    "(%s + [%s]h)".format(real.toString, infinitesimal.mkString(", "))
  }
}

trait JetInstances {
  implicit def JetAlgebra[@sp(Float, Double) T](implicit
      c: ClassTag[T], d: JetDim, eq: Eq[T], f: Field[T], n: NRoot[T],
      t: Trig[T], r: IsReal[T]): JetAlgebra[T] = {
    import spire.std.array.ArrayVectorSpace
    new JetAlgebra[T]
  }

  implicit def JetEq[T : Eq]: Eq[Jet[T]] = new JetEq[T]
}

private[math] trait JetIsRing[@sp(Float, Double) T] extends Ring[Jet[T]] {
  implicit def c: ClassTag[T]
  implicit def d: JetDim
  implicit def eq: Eq[T]
  implicit def f: Field[T]
  implicit def r: IsReal[T]
  implicit def t: Trig[T]
  implicit def v: VectorSpace[Array[T], T]

  override def minus(a: Jet[T], b: Jet[T]): Jet[T] = a - b
  def negate(a: Jet[T]): Jet[T] = -a
  def one: Jet[T] = Jet.one[T](c, d, f)
  def plus(a: Jet[T], b: Jet[T]): Jet[T] = a + b
  override def pow(a: Jet[T], b: Int): Jet[T] = a.pow(b)
  override def times(a: Jet[T], b: Jet[T]): Jet[T] = a * b
  def zero: Jet[T] = Jet.zero(c, d, f)

  override def fromInt(n: Int): Jet[T] = Jet.fromInt[T](n)
}

/* TODO: Jet[T] is probably not a genuine GCD ring */
private[math] trait JetIsGCDRing[@sp(Float, Double) T]
    extends JetIsRing[T] with GCDRing[Jet[T]] {
  /* TODO: What exactly is this GCD trying to achieve? Tests? */
  def gcd(a: Jet[T], b: Jet[T]): Jet[T] = {
    @tailrec def _gcd(a: Jet[T], b: Jet[T]): Jet[T] =
      if (b.isZero) a else _gcd(b, a - (a / b).round * b)
    _gcd(a, b)
  }
}

/* TODO: Jet[T] is probably not a genuine Euclidean ring */
private[math] trait JetIsEuclideanRing[@sp(Float,Double) T]
    extends JetIsGCDRing[T] with EuclideanRing[Jet[T]] {
  def euclideanFunction(a: Jet[T]): BigInt = sys.error("Clarify Jet first, see #598")
  /* TODO: what are exactly the laws of Jet with respect to EuclideanRing ? */
  def quot(a: Jet[T], b: Jet[T]): Jet[T] = a /~ b
  def mod(a: Jet[T], b: Jet[T]): Jet[T] = a % b
  override def quotmod(a: Jet[T], b: Jet[T]): (Jet[T], Jet[T]) = a /% b
}

/* TODO: Jet[T] is probably not a genuine Field */
private[math] trait JetIsField[@sp(Float,Double) T]
    extends JetIsEuclideanRing[T] with Field[Jet[T]] {
  /* TODO: what are exactly the laws of Jet with respect to EuclideanRing ? */
  // duplicating methods because super[..].call() does not work on 2.10 and 2.11
  override def euclideanFunction(a: Jet[T]): BigInt = sys.error("Clarify Jet first, see #598")
  override def quot(a: Jet[T], b: Jet[T]): Jet[T] = a /~ b
  override def mod(a: Jet[T], b: Jet[T]): Jet[T] = a % b
  override def quotmod(a: Jet[T], b: Jet[T]): (Jet[T], Jet[T]) = a /% b
  override def fromDouble(n: Double): Jet[T] = Jet(f.fromDouble(n))
  def div(a: Jet[T], b: Jet[T]): Jet[T] = a / b
  def ceil(a: Jet[T]): Jet[T] = a.ceil
  def floor(a: Jet[T]): Jet[T] = a.floor
  def round(a: Jet[T]): Jet[T] = a.round
  def isWhole(a: Jet[T]): Boolean = a.isWhole
}

private[math] trait JetIsTrig[@sp(Float, Double) T] extends Trig[Jet[T]] {
  implicit def c: ClassTag[T]
  implicit def d: JetDim
  implicit def f: Field[T]
  implicit def n: NRoot[T]
  implicit def r: IsReal[T]
  implicit def t: Trig[T]
  implicit def v: VectorSpace[Array[T], T]

  def e: Jet[T] = Jet(t.e)
  def pi: Jet[T] = Jet(t.pi)

  def exp(a: Jet[T]): Jet[T] = a.exp
  def expm1(a: Jet[T]): Jet[T] = a.exp - f.one
  def log(a: Jet[T]): Jet[T] = a.log
  def log1p(a: Jet[T]): Jet[T] = (a + f.one).log

  def sin(a: Jet[T]): Jet[T] = a.sin
  def cos(a: Jet[T]): Jet[T] = a.cos
  def tan(a: Jet[T]): Jet[T] = a.tan

  def asin(a: Jet[T]): Jet[T] = a.asin
  def acos(a: Jet[T]): Jet[T] = a.acos
  def atan(a: Jet[T]): Jet[T] = a.atan
  def atan2(y: Jet[T], x: Jet[T]): Jet[T] = y.atan2(x)

  def sinh(x: Jet[T]): Jet[T] = x.sinh
  def cosh(x: Jet[T]): Jet[T] = x.cosh
  def tanh(x: Jet[T]): Jet[T] = x.tanh

  def toRadians(a: Jet[T]): Jet[T] = a
  def toDegrees(a: Jet[T]): Jet[T] = a
}

private[math] trait JetIsNRoot[T] extends NRoot[Jet[T]] {
  implicit def f: Field[T]
  implicit def n: NRoot[T]
  implicit def t: Trig[T]
  implicit def r: IsReal[T]
  implicit def c: ClassTag[T]
  implicit def v: VectorSpace[Array[T], T]

  def nroot(a: Jet[T], k: Int): Jet[T] = a.nroot(k)
  override def sqrt(a: Jet[T]): Jet[T] = a.sqrt
  def fpow(a: Jet[T], b: Jet[T]): Jet[T] = a.pow(b)
  def fpow(a: T, b: Jet[T]): Jet[T] = b.powScalarToJet(a)
}

@SerialVersionUID(0L)
private[math] class JetEq[T : Eq] extends Eq[Jet[T]] with Serializable {
  def eqv(x: Jet[T], y: Jet[T]): Boolean = x eqv y
  override def neqv(x: Jet[T], y: Jet[T]): Boolean = x neqv y
}

/* TODO
private[math] trait JetIsSigned[T] extends Signed[Jet[T]] {
  implicit def f: Field[T]
  implicit def r: IsReal[T]
  implicit def t: Trig[T]
  implicit def v: VectorSpace[Array[T],T]

  def signum(a: Jet[T]): Int = a.signum
  def abs(a: Jet[T]): Jet[T] = a.abs
  def compare(x: Jet[T], y: Jet[T]): Int = ???
}
 */

@SerialVersionUID(0L)
private[math] class JetAlgebra[@sp(Float, Double) T](implicit
    val c: ClassTag[T], val d: JetDim, val eq: Eq[T], val f: Field[T], val n: NRoot[T],
    val t: Trig[T], val r: IsReal[T], val v: VectorSpace[Array[T], T])
  extends JetIsField[T]
  with JetIsTrig[T]
  with JetIsNRoot[T]
//  with JetIsSigned[T]
  with VectorSpace[Jet[T], T]
  with FieldAlgebra[Jet[T], T]
  with Serializable {
  def scalar: Field[T] = f
  def nroot: NRoot[T] = n
  def timesl(a: T, w: Jet[T]): Jet[T] = Jet(a) * w
  def dot(x: Jet[T], y: Jet[T]): T = {
    x.infinitesimal.zip(y.infinitesimal).foldLeft{scalar.times(x.real, y.real)}{
      (xx, yy) => scalar.plus(xx, scalar.times(yy._1, yy._2))}
  }
}
