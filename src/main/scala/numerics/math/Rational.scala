package numerics.math

import scala.math.{ScalaNumber, ScalaNumericConversions, min}
import scala.collection.immutable.{Range, NumericRange}

import java.math.MathContext


/**
 * The companion object for the `Rational` class. This provides the only
 * "constructors" to create rationals. It also contains implicits to allow
 * the rationals to be treated as numbers (ie. scala.Numeric/Fractional) and
 * a default ordering.
 *
 * @author Tom Switzer
 */
object Rational {
  val zero = new Rational(0, 1)
  val one = new Rational(1, 1)

  private val RationalString = """^(-?\d+)/(-?\d+)$"""r
  private val IntegerString = """^(-?\d+)$"""r


  /**
   * Returns the ''canonical'' version of the rational number `n/d`. That is,
	 * the rational `n'/d'` is returned, s.t. `n'k = n` and `d'k = d` for some
	 * integer `k` and `n'` and `d'` are coprime.
	 *
	 * @param n The numerator of the rational.
	 * @param d The denominator of the rational.
	 * @return A `Rational` that is equivalent to `n/d`.
   */
  def apply(n: BigInt, d: BigInt): Rational = {
    if (n == d)
      one
    else if (n == 0)
      zero
    else
      canonical(n, d)
  }
  def apply(n: BigInt): Rational = Rational(n, BigInt(1))
  def apply(n: Int, d: Int): Rational = Rational(BigInt(n), BigInt(d))
  def apply(n: Int): Rational = Rational(BigInt(n), BigInt(1))
  def apply(n: Long, d: Long): Rational = Rational(BigInt(n), BigInt(d))
  def apply(n: Long): Rational = Rational(n, 1L)
  def apply(x: BigDecimal): Rational = Rational(new BigInt(x.bigDecimal.unscaledValue), BigInt(10) pow x.scale)
  def apply(x: Double): Rational = Rational(BigDecimal(x))
  def apply(x: Float): Rational = Rational(BigDecimal(x))
  def apply(r: String): Rational = r match {
    case RationalString(n, d) => Rational(BigInt(n), BigInt(d))
    case IntegerString(n) => Rational(BigInt(n))
    case x => try {
      Rational(BigDecimal(x))
    } catch {
      case nfe: NumberFormatException => throw new NumberFormatException("For input string: " + r)
    }
  }

  /**
   * An extractor for the numerator and denominator.
   */
  def unapply(r: Rational): Option[(BigInt,BigInt)] = Some((r.numerator, r.denominator))

    
  /**
   * Returns the canonical form of the number {@code n/d}. Canonical means
   * that the fraction cannot be reduced further and the denominator is
   * non-negative.
   */
  protected def canonical(n: BigInt, d: BigInt): Rational = {
    val gcd = n.gcd(d)
    if (gcd == 1)
      if (d < 0) new Rational(-n, -d) else new Rational(n, d)
    else
      if (d < 0) new Rational(-n / gcd, -d / gcd) else new Rational(n / gcd, d / gcd)
  }


  trait RationalIsConflicted extends scala.math.Numeric[Rational] {
    def plus(x: Rational, y: Rational): Rational = x + y
    def minus(x: Rational, y: Rational): Rational = x - y
    def times(x: Rational, y: Rational): Rational = x * y
    def negate(x: Rational): Rational = -x
    def fromInt(x: Int): Rational = Rational(x, 1)
    def toInt(x: Rational): Int = toBigInt(x).toInt
    def toLong(x: Rational): Long = toBigInt(x).toInt
    def toFloat(x: Rational): Float = toDouble(x).toFloat
    def toDouble(x: Rational): Double = x.n.toDouble / x.d.toDouble
    def toBigInt(x: Rational): BigInt = x.numerator / x.denominator
  }
  trait RationalIsFractional extends RationalIsConflicted with scala.math.Fractional[Rational] {
    def div(x: Rational, y: Rational): Rational = x / y
  }
  trait RationalAsIfIntegral extends RationalIsConflicted with scala.math.Integral[Rational] {
    def quot(x: Rational, y: Rational): Rational = x / y
    def rem(x: Rational, y: Rational): Rational = {
      val q = x / y
      q - q.toBigInt
    }
  }
  implicit object RationalIsFractional extends RationalIsFractional with Ordering[Rational] {
    def compare(x: Rational, y: Rational) = (x - y).n.signum
  }


  implicit def bigInt2Rational(x: BigInt): Rational = Rational(x, BigInt(1))
  implicit def int2Rational(x: Int): Rational = Rational(x)
  implicit def long2Rational(x: Long): Rational = Rational(x)
  implicit def bigDecimal2Rational(x: BigDecimal) = Rational(x)
  implicit def double2Rational(x: Double) = Rational(x)
  implicit def float2Rational(x: Float) = Rational(x)

  object Range {
    implicit object RationalAsIfIntegral extends RationalAsIfIntegral with Ordering[Rational] {
      def compare(x: Rational, y: Rational) = (x - y).n.signum
    }

    def apply(start: Rational, end: Rational, step: Rational) = NumericRange(start, end, step)
    def inclusive(start: Rational, end: Rational, step: Rational) = NumericRange.inclusive(start, end, step)
  }
}


/**
 * Rational represents a rational number.
 *
 * @param n The numerator of the rational.
 * @param d The denominator of the rational.
 *
 * @author Tom Switzer
 */
class Rational private (val n: BigInt, val d: BigInt) extends ScalaNumber with ScalaNumericConversions with Ordered[Rational] {
  require(d != 0)

  def numerator: BigInt = n
  def denominator: BigInt = d

  def abs: Rational = if (n < 0) Rational(-n, d) else this
  def signum: Int = n.signum
  def inverse: Rational = Rational(d, n)

  def +(r: Rational): Rational = {
    var dgcd = d.gcd(r.d)
    if (dgcd == 1) {

      /* This case actually simplifies things greatly. If both "this" and
       * "r" are canonical and the denominators are coprime, then
       * n*r.d+r.n*d is co-prime with d * r.d and so (n*r.d+r.n*d)/(d*r.d)
       * is also canonical.
       *
       * Proof: If it weren't, we could rewrite the above as
       *      (n*r.d/k + r.n*d/k)/(d*r.d/k),
       * for some integer k, s.t. ad/k, bc/k, bd/k are ints, k != 1,d,r.d.
       * We can assume k is prime. In this case, k must be part of the
       * prime factorization of either d or r.d (if it was part of both
       * then d would not be coprime with r.d, a contradiction). W.l.o.g.
       * assume that k divides d, then k does not divide r.d. Since
       * k divides n*r.d, k must be a factor of n. Since k is a common
       * factor of both n and d, n/d is not canonical, a contradiction.
       *
       * As noted by Wikipedia's Coprime article, the probability of 2
       * random integers being coprime is ~71%; not too shabby.
       */

      new Rational(n * r.d + r.n * d, d * r.d)
    } else {
      val den = d / dgcd
      val num = n * (r.d / dgcd) + r.n * den
      dgcd = num.gcd(dgcd)
      if (dgcd == 1)  // d / dgcd is coprime with r.d
        new Rational(num, den * r.d)
      else
        new Rational(num / dgcd, den * (r.d / dgcd))
    }
  }

  def -(r: Rational): Rational = {
    var dgcd = d.gcd(r.d)
    if (dgcd == 1) {
      new Rational(n * r.d - r.n * d, d * r.d)
    } else {
      val den = d / dgcd
      val num = n * (r.d / dgcd) - r.n * den
      dgcd = num.gcd(dgcd)
      if (dgcd == 1)
        new Rational(num, den * r.d)
      else
        new Rational(num / dgcd, den * (r.d / dgcd))
    }
  }

  def unary_- : Rational = new Rational(-n, d)

  def *(r: Rational): Rational = {
    
    /* The only prime factors that n*r.n and d*r.d will have in common are
     * between n & r.d and d & r.n since both "this" and r are canonical.
     *
     * This should provide a non-negligable speed up over the naive approach
     * (ie. def *(r: Rational) = Rational(n * r.n, d * r.d)).
     */

    if (this == r) {
      new Rational(n * r.n, d * r.d)
    } else {
      val a = n.gcd(r.d)
      val b = d.gcd(r.n)
      new Rational((n / a) * (r.n / b), (d / b) * (r.d / a))
    }
  }

  def /(r: Rational): Rational = {
    if (this == r) {
      Rational.one
    } else {
      val a = n.gcd(r.n)
      val b = d.gcd(r.d)
      val num = (n / a) * (r.d / b)
      val den = (d / b) * (r.n / a)
      if (den < 0) new Rational(-num, -den) else new Rational(num, den)
    }
  }

  def pow(exp: Int): Rational = if (exp < 0) {
    new Rational(d pow exp.abs, n pow exp.abs)
  } else {
    new Rational(n pow exp, d pow exp)
  }

  def compare(r: Rational): Int = {
    val dgcd = d.gcd(r.d)
    val num = if (dgcd == 1) n * r.d - r.n * d else n * (r.d / dgcd) - r.n * (d / dgcd)
    num signum
  }

  /**
   * Returns `true` if this is a ''whole'' number.
   * @see scala.math.ScalaNumber#isWhole
   */
  def isWhole: Boolean = (d == 1)
  
  /**
   * This doesn't use an underlying "Java" type per se, so we return a simple
   * array of size 2 of the underlying BigInts' underlying objects.
   * @see scala.math.ScalaNumber#underlying
   */
  def underlying: Array[Object] = Array(n.underlying, d.underlying)

  def toBigInt = n / d
  def toBigDecimal: BigDecimal = BigDecimal(n) / BigDecimal(d)
  def toBigDecimalWithContext(mc: MathContext): BigDecimal = {
    new BigDecimal(BigDecimal(n).bigDecimal.divide(BigDecimal(d).bigDecimal, mc))
  }

  def longValue = toBigInt.longValue
  def intValue = toBigInt.intValue
  def charValue = intValue.toChar
  
  def doubleValue: Double = if (n == 0) {
    0.0
  } else if (n < 0) {
    -(-this).toDouble
  } else {
    
    // We basically just shift n so that integer division gives us 54 bits of
    // accuracy. We use the last bit for rounding, so end w/ 53 bits total.

    val sharedLength = min(n.bitLength, d.bitLength)
    val dLowerLength = d.bitLength - sharedLength

    val nShared = n >> (n.bitLength - sharedLength)
    val dShared = d >> dLowerLength
  
    d.underlying.getLowestSetBit() < dLowerLength
    val addBit = if (nShared < dShared || (nShared == dShared && d.underlying.getLowestSetBit() < dLowerLength)) {
      1
    } else {
      0
    }

    val e = d.bitLength - n.bitLength + addBit
    val ln = n << (53 + e)    // We add 1 bit for rounding.
    val lm = (ln / d).toLong
    val m = ((lm >> 1) + (lm & 1)) & 0x000fffffffffffffL
    val bits = (m | ((1023L - e) << 52))
    java.lang.Double.longBitsToDouble(bits)
  }

  def floatValue = doubleValue.toFloat

  override def toString: String = if (isWhole) n.toString else "%d/%d" format (n, d)
  
  override def hashCode: Int =
    if (isWhole) unifiedPrimitiveHashcode
    else 29 * (37 * n.## + d.##)

  override def equals(r: Any): Boolean = r match {
    case that: Rational => ((that canEqual this) && n == that.n && d == that.d)
    case that: BigInt => isWhole && n == that
    case that: BigDecimal => try {
        toBigDecimal == that
      } catch {
        case ae: ArithmeticException => false
      }
    case that => unifiedPrimitiveEquals(that)
  }

  def canEqual(other: Any) = other.isInstanceOf[Rational]

  def until(end: Rational, step: Rational = Rational(1)) = Rational.Range(this, end, step)
  def to(end: Rational, step: Rational = Rational(1)) = Rational.Range.inclusive(this, end, step)
}

