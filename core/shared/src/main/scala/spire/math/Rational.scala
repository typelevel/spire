package spire.math

import scala.annotation.tailrec
import scala.math.{ScalaNumber, ScalaNumericConversions}

import java.math.{BigDecimal => JBigDecimal, BigInteger, MathContext, RoundingMode}

import spire.algebra.{Field, IsRational, NRoot, Sign}
import spire.algebra.Sign.{ Positive, Zero, Negative }
import spire.macros.Checked
import spire.std.long.LongAlgebra
import spire.std.double._
import spire.syntax.nroot._

//scalastyle:off equals.hash.code
sealed abstract class Rational extends ScalaNumber with ScalaNumericConversions with Ordered[Rational] { lhs =>

  def numerator: SafeLong
  def denominator: SafeLong

  def numeratorAsLong: Long
  def denominatorAsLong: Long

  def isValidLong: Boolean

  def isWhole: Boolean

  def longValue: Long = toBigInt.longValue

  def intValue: Int = longValue.intValue

  def floatValue: Float = doubleValue.toFloat

  // ugh, ScalaNumber and ScalaNumericConversions in 2.10 require this hack
  override def underlying: Object = this

  def abs: Rational = if (signum < 0) -this else this
  def inverse: Rational = reciprocal
  def reciprocal: Rational
  def signum: Int
  def isZero: Boolean
  def isOne: Boolean

  def unary_-(): Rational

  def +(rhs: Rational): Rational
  def -(rhs: Rational): Rational
  def *(rhs: Rational): Rational
  def /(rhs: Rational): Rational

  def /~(rhs: Rational): Rational = Rational(SafeLong((this / rhs).toBigInt), SafeLong.one)
  def %(rhs: Rational): Rational = this - (this /~ rhs) * rhs
  def /%(rhs: Rational): (Rational, Rational) = {
    val q = this /~ rhs
    (q, this - q * rhs)
  }

  def gcd(rhs: Rational): Rational

  def toBigDecimal(scale: Int, mode: RoundingMode): BigDecimal = {
    val n = new JBigDecimal(numerator.toBigInteger)
    val d = new JBigDecimal(denominator.toBigInteger)
    BigDecimal(n.divide(d, scale, mode))
  }

  def toBigDecimal(mc: MathContext): BigDecimal = {
    val n = new JBigDecimal(numerator.toBigInteger)
    val d = new JBigDecimal(denominator.toBigInteger)
    BigDecimal(n.divide(d, mc))
  }

  def toBigInt: BigInt
  override def shortValue: Short = longValue.toShort
  override def byteValue: Byte = longValue.toByte

  def floor: Rational
  def ceil: Rational
  def round: Rational

  def roundTo(denom: SafeLong): Rational = (this * denom).round / denom

  def pow(exp: Int): Rational

  def sign: Sign = Sign(signum)

  def compareToOne: Int

  def min(rhs: Rational): Rational =
    if ((lhs compare rhs) < 0) lhs else rhs

  def max(rhs: Rational): Rational =
    if ((lhs compare rhs) > 0) lhs else rhs

  /**
   * Returns a `Rational` whose numerator and denominator both fit in an `Int`.
   */
  def limitToInt: Rational = limitTo(BigInt(Int.MaxValue))


  /**
   * Returns a `Rational` whose numerator and denominator both fit in a `Long`.
   */
  def limitToLong: Rational = limitTo(BigInt(Long.MaxValue))


  /**
   * Returns a `Rational` whose denominator and numerator are no larger than
   * `max` and whose value is close to the original. This applies, even if, for
   * example, this `Rational` is greater than `max`. In that case,
   * `Rational(max, 1)` is returned.
   *
   * @param max A positive integer.
   */
  def limitTo(max: BigInt): Rational = if (this.signum < 0) {
    -((-this).limitTo(max))
  } else {
    require(max > 0, "Limit must be a positive integer.")

    val half = max >> 1
    val floor = this.toBigInt
    if (floor >= max) {
      Rational(max)
    } else if (floor >= (max >> 1)) {
      Rational(floor.toLong)
    } else if (this.compareToOne < 0) {
      limitDenominatorTo(max)
    } else {
      limitDenominatorTo(max * denominator / numerator)
    }
  }


  /**
   * Finds the closest `Rational` to this `Rational` whose denominator is no
   * larger than `limit`.
   *
   * See [[http://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree#Mediants_and_binary_search]]
   */
  def limitDenominatorTo(limit: SafeLong): Rational = {
    require(limit.signum > 0, "Cannot limit denominator to non-positive number.")

    // TODO: We should always perform a binary search from the left or right to
    //       speed up computation. For example, if in a search, we have a lower
    //       bound of 1/2 for many steps, then each step will only add 1/2 to
    //       the upper-bound, and so we'd converge on the number quite slowly.
    //       However, we can speed this up by tentatively checking if we could
    //       skip some intermediate values, by performing an adaptive search.
    //       We'd simply keep doubling the number of steps we're skipping until
    //       the upper-bound (eg) is now the lower bound, then go back to find
    //       the greatest lower bound in the steps we missed by binary search.
    //       Instead of adding 1/2 n times, we would try to add 1/2, 2/4, 4/8,
    //       8/16, etc., until the upper-bound swiches to a lower bound. Say
    //       this happens a (1/2)*2^k, then we simply perform a binary search in
    //       between (1/2)*2^(k-1) and (1/2)*2^k to find the new lower bound.
    //       This would reduce the number of steps to O(log n).

    @tailrec
    def closest(l: Rational, u: Rational): Rational = {
      val mediant = Rational(l.numerator + u.numerator, l.denominator + u.denominator)

      if (mediant.denominator > limit) {
        if ((this - l).abs > (u - this).abs) u else l
      } else if (mediant == this) {
        mediant
      } else if (mediant < this) {
        closest(mediant, u)
      } else {
        closest(l, mediant)
      }
    }

    this.sign match {
      case Zero => this
      case Positive => closest(Rational(this.toBigInt), LongRational(1, 0))
      case Negative => closest(LongRational(-1, 0), Rational(this.toBigInt))
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: Real => this == that.toRational
    case that: Algebraic => that == this
    case that: BigInt => isWhole && toBigInt == that
    case that: BigDecimal => try { toBigDecimal(that.mc) == that } catch { case ae: ArithmeticException => false }
    case that: SafeLong => SafeLong(toBigInt) == that
    case that: Number => Number(this) == that
    case that: Natural => isWhole && this == Rational(that.toBigInt)
    case that: Complex[_] => that == this
    case that: Quaternion[_] => that == this
    case that: Long => isValidLong && toLong == that
    case that => unifiedPrimitiveEquals(that)
  }
}

object Rational extends RationalInstances {
  private val RationalString = """^(-?\d+)/(-?\d+)$""".r
  private val IntegerString = """^(-?\d+)$""".r

  val zero: Rational = LongRational(0L, 1L)
  val one: Rational = LongRational(1L, 1L)

  private[math] def toDouble(n: SafeLong, d: SafeLong): Double = n.signum match {
    case 0 => 0.0
    case -1 => -toDouble(-n, d)
    case 1 =>
      // We basically just shift n so that integer division gives us 54 bits of
      // accuracy. We use the last bit for rounding, so end w/ 53 bits total.
      val sharedLength = java.lang.Math.min(n.bitLength, d.bitLength)
      val dLowerLength = d.bitLength - sharedLength

      val nShared = n >> (n.bitLength - sharedLength)
      val dShared = d >> dLowerLength

      val addBit = if (nShared < dShared || (nShared == dShared && d.toBigInteger.getLowestSetBit < dLowerLength)) {
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

  def apply(n: BigInt, d: BigInt): Rational =
    apply(SafeLong(n), SafeLong(d))

  def apply(n: Long, d: Long): Rational = {
    def build0(n: Long, d: Long) = if (n == 0L) zero else {
      val divisor = spire.math.gcd(n, d)
      if (divisor == 1L)
        LongRational(n, d)
      else
        LongRational(n / divisor, d / divisor)
    }
    if (d == 0) throw new IllegalArgumentException("0 denominator")
    else if (d > 0) build0(n, d)
    else if (n == Long.MinValue || d == Long.MinValue) Rational(-BigInt(n), -BigInt(d))
    else build0(-n, -d)
  }

  private[math] def buildWithDiv(num: Long, ngcd: Long, rd: Long, lden: Long): Rational = {
    val n = num / ngcd
    val d = rd / ngcd
    Checked.tryOrReturn {
      apply(n, lden * d)
    } {
      Rational(SafeLong(n), SafeLong(lden) * d)
    }
  }

  def apply(n: SafeLong, d: SafeLong): Rational = {
    if (d.isZero) throw new IllegalArgumentException("0 denominator")
    else if (n.isValidLong && d.isValidLong) apply(n.toLong, d.toLong)
    else if (d.signum < 0) return apply(-n, -d)
    else {
      val g = n gcd d
      n / g match {
        case SafeLongLong(x) => (d / g) match {
          case SafeLongLong(y) => LongRational(x, y)
          case y: SafeLongBigInteger => BigRational(x, y)
        }
        case x: SafeLongBigInteger => BigRational(x, d / g)
      }
    }
  }

  implicit def apply(x: Int): Rational = if(x == 0) Rational.zero else LongRational(x, 1L)
  implicit def apply(x: Long): Rational = if(x == 0L) Rational.zero else LongRational(x, 1L)
  implicit def apply(x: BigInt): Rational = apply(x, SafeLong.one)

  implicit def apply(x: Float): Rational = apply(x.toDouble)

  implicit def apply(x: Double): Rational = if (x == 0D) {
    zero
  } else {
    val bits = java.lang.Double.doubleToLongBits(x)
    val value = if ((bits >> 63) < 0) -(bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L)
                else (bits & 0x000FFFFFFFFFFFFFL | 0x0010000000000000L)
    val exp = ((bits >> 52) & 0x7FF).toInt - 1075 // 1023 + 52
    if (exp > 10) {
        apply(BigInt(value) << exp, BigInt(1))
    } else if (exp >= 0) {
      apply(value << exp, 1L)
    } else if (exp >= -52 && (~((-1L) << (-exp)) & value) == 0L) {
      apply(value >> (-exp), 1L)
    } else {
      apply(BigInt(value), BigInt(1) << (-exp))
    }
  }

  implicit def apply(x:BigDecimal): Rational = {
    if (x.ulp >= 1) {
      apply(x.toBigInt, 1)
    } else {
      val n = (x / x.ulp).toBigInt
      val d = (BigDecimal(1.0) / x.ulp).toBigInt
      apply(n, d)
    }
  }

  def apply(r: String): Rational = r match {
    case RationalString(n, d) => Rational(SafeLong(n), SafeLong(d))
    case IntegerString(n) => Rational(SafeLong(n))
    case s => try {
      Rational(BigDecimal(s))
    } catch {
      case nfe: NumberFormatException => throw new NumberFormatException("For input string: " + s)
    }
  }

  implicit def apply(n: SafeLong): Rational =
    n match {
      case SafeLongLong(x) => if (x == 0) Rational.zero else LongRational(x, 1L)
      case x:SafeLongBigInteger => BigRational(x, SafeLong.one)
    }

  implicit def apply(x: Number): Rational = x match {
    case RationalNumber(n) => n
    case IntNumber(n) => apply(n)
    case FloatNumber(n) => apply(n)
    case DecimalNumber(n) => apply(n)
  }

  // $COVERAGE-OFF$
  /**
   * Returns an interval that bounds the nth-root of the integer x.
   *
   * TODO: This is really out-of-place too.
   */
  def nroot(x: SafeLong, n: Int): (SafeLong, SafeLong) = {
    def findnroot(prev: SafeLong, add: Int): (SafeLong, SafeLong) = {
      val min = SafeLong(prev.toBigInteger.setBit(add))
      val max = min + 1

      val fl = min pow n
      val cl = max pow n

      if (fl > x) {
        findnroot(prev, add - 1)
      } else if (cl < x) {
        findnroot(min, add - 1)
      } else if (cl == x) {
        (max, max)
      } else if (fl == x) {
        (min, min)
      } else {
        (min, max)
      }
    }

    findnroot(SafeLong.zero, (x.bitLength + n - 1) / n) // ceil(x.bitLength / n)
  }


  /**
   * Returns an interval that bounds the nth-root of the integer x.
   */
  def nroot(x: Long, n: Int): (Long, Long) = {
    def findnroot(prev: Long, add: Long): (Long, Long) = {
      val min = prev | add
      val max = min + 1
      val fl = pow(min, n)
      val cl = pow(max, n)

      if (fl <= 0 || fl > x) {
        findnroot(prev, add >> 1)
      } else if (cl < x) {
        findnroot(min, add >> 1)
      } else if (cl == x) {
        (max, max)
      } else if (fl == x) {
        (min, min)
      } else {
        (min, max)
      }
    }

    // TODO: Could probably get a better initial add then this.
    findnroot(0, 1L << ((65 - n) / n))
  }
  // $COVERAGE-ON$
}

@SerialVersionUID(0L)
private[math] final case class LongRational(n: Long, d: Long) extends Rational with Serializable {
  def numerator: SafeLong = SafeLong(n)
  def denominator: SafeLong = SafeLong(d)

  def numeratorAsLong: Long = n
  def denominatorAsLong: Long = d

  def reciprocal: Rational =
    if (n == 0L) throw new ArithmeticException("reciprocal called on 0/1")
    else if (n > 0L) LongRational(d, n)
    else if (n == Long.MinValue || d == Long.MinValue) BigRational(-SafeLong(d), -SafeLong(n))
    else LongRational(-d, -n)

  override def signum: Int = java.lang.Long.signum(n)

  override def isWhole: Boolean = d == 1L

  override def isZero: Boolean = n == 0L

  override def isOne: Boolean = d == 1L && n == 1L

  override def isValidChar: Boolean = isWhole && n.isValidChar

  override def isValidByte: Boolean = isWhole && n.isValidByte

  override def isValidShort: Boolean = isWhole && n.isValidShort

  override def isValidInt: Boolean = isWhole && n.isValidInt

  override def isValidLong: Boolean = isWhole

  override def toBigInt: BigInt = BigInt(n / d)

  override def doubleValue: Double = Rational.toDouble(n, d)

  override def unary_-(): Rational =
    if (n == Long.MinValue) BigRational(SafeLong.safe64, SafeLong(d))
    else LongRational(-n, d)

  def +(r: Rational): Rational = r match {
    case r: LongRational =>
      val dgcd: Long = spire.math.gcd(d, r.d)
      if (dgcd == 1L) {
        Checked.tryOrReturn[Rational] {
          Rational(n * r.d + r.n * d, d * r.d)
        } {
          Rational(SafeLong(n) * r.d + SafeLong(r.n) * d, SafeLong(d) * r.d)
        }

      } else {

        val lden: Long = d / dgcd
        val rden: Long = r.d / dgcd
        Checked.tryOrReturn {
          val num: Long = n * rden + r.n * lden

          val ngcd: Long = spire.math.gcd(num, dgcd)

          if (ngcd == 1L)
            Rational(num, lden * r.d)
          else
            Rational.buildWithDiv(num, ngcd, r.d, lden)
        } {
          val num: SafeLong = SafeLong(n) * rden + SafeLong(r.n) * lden

          val ngcd: Long = spire.math.gcd(dgcd, (num % dgcd).toLong)

          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      }
    case r: BigRational =>
      val dgcd: Long = spire.math.gcd(d, (r.d % d).toLong)
      if (dgcd == 1L) {

        val num = r.d * n + r.n * d
        val den = r.d * d
        Rational(num, den)

      } else {

        val lden: Long = d / dgcd
        val rden: SafeLong = r.d / dgcd
        val num: SafeLong = rden * n + r.n * lden
        val ngcd: Long = num match {
          case SafeLongLong(x) => spire.math.gcd(x, dgcd)
          case SafeLongBigInteger(x) => spire.math.gcd(dgcd, (x mod BigInteger.valueOf(dgcd)).longValue)
        }

        if (ngcd == 1L)
          Rational(num, SafeLong(lden) * r.d)
        else
          Rational(num / ngcd, r.d / ngcd * lden)

      }
  }

  def -(r: Rational): Rational = r match {
    case r: LongRational =>
      val dgcd: Long = spire.math.gcd(d, r.d)
      if (dgcd == 1L) {
        Checked.tryOrReturn[Rational] {
          Rational(n * r.d - r.n * d, d * r.d)
        } {
          Rational(SafeLong(n) * r.d - SafeLong(r.n) * d, SafeLong(d) * r.d)
        }

      } else {

        val lden: Long = d / dgcd
        val rden: Long = r.d / dgcd
        Checked.tryOrReturn {
          val num: Long = n * rden - r.n * lden

          val ngcd: Long = spire.math.gcd(num, dgcd)

          if (ngcd == 1L)
            Rational(num, lden * r.d)
          else
            Rational.buildWithDiv(num, ngcd, r.d, lden)
        } {
          val num: SafeLong = SafeLong(n) * rden - SafeLong(r.n) * lden

          val ngcd: Long = spire.math.gcd(dgcd, (num % dgcd).toLong)

          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      }
    case r: BigRational =>
      val dgcd: Long = spire.math.gcd(d, (r.d % d).toLong)
      if (dgcd == 1L) {

        val num = r.d * n - r.n * d
        val den = r.d * d
        Rational(num, den)

      } else {

        val lden: Long = d / dgcd
        val rden: SafeLong = r.d / dgcd
        val num: SafeLong = rden * n - r.n * lden
        val ngcd: Long = num match {
          case SafeLongLong(x) => spire.math.gcd(x, dgcd)
          case SafeLongBigInteger(x) => spire.math.gcd(dgcd, (x mod BigInteger.valueOf(dgcd)).longValue)
        }

        if (ngcd == 1L)
          Rational(num, SafeLong(lden) * r.d)
        else
          Rational(num / ngcd, r.d / ngcd * lden)

      }
  }


  def *(r: Rational): Rational = if (n == 0L) Rational.zero else r match {
    case r: LongRational =>
      val a = spire.math.gcd(n, r.d)
      val b = spire.math.gcd(d, r.n)
      // this does not have to happen within the checked block, since the divisions are guaranteed to work
      val n1 = n / a
      val n2 = r.n / b
      val d1 = d / b
      val d2 = r.d / a
      Checked.tryOrReturn[Rational] {
        LongRational(n1 * n2, d1 * d2)
      } {
        // we know that the result does not fit into a LongRational, and also that the denominators are positive.
        // so we can just call BigRational.apply directly
        BigRational(SafeLong(n1) * n2, SafeLong(d1) * d2)
      }
    case r: BigRational =>
      val a = spire.math.gcd(n, (r.d % n).toLong)
      val b = spire.math.gcd(d, (r.n % d).toLong)
      Rational(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
  }


  def /(r: Rational): Rational = r match {
    case r: LongRational =>
      // we only have to do this check in the long branch, since 0 is always represented as LongRational(0,1)
      if (r.n == 0L) throw new ArithmeticException("divide (/) by 0")
      if (n == 0L) return this
      val a = spire.math.gcd(n, r.n)
      val b = spire.math.gcd(d, r.d)
      // this does not have to happen within the checked block, since the divisions are guaranteed to work
      val n1 = n / a
      val n2 = r.n / a
      var d1 = d / b
      var d2 = r.d / b
      // this is to make sure that the denominator of the result is positive
      if (n2 < 0) {
        // denominators are always positive, so negating them can not yield an overflow
        d1 = -d1
        d2 = -d2
      }
      Checked.tryOrReturn[Rational] {
        LongRational(n1 * d2, d1 * n2)
      } {
        // we know that the result does not fit into a LongRational, and we have made sure that the product of d1
        // and n2 is positive. So we can just call BigRational.apply directly
        BigRational(SafeLong(n1) * d2, SafeLong(d1) * n2)
      }
    case r: BigRational =>
      if (n == 0L) return this
      val a = spire.math.gcd(n, (r.n % n).toLong)
      val b = spire.math.gcd(d, (r.d % d).toLong)
      val num = SafeLong(n / a) * (r.d / b)
      val den = SafeLong(d / b) * (r.n / a)
      if (den.signum < 0) Rational(-num, -den) else Rational(num, den)
  }

  def gcd(r: Rational): Rational = if(isZero) r.abs else if(isOne) this else r match {
    case r: LongRational =>
      val dgcd: Long = spire.math.gcd(d, r.d)
      val n0 = spire.math.abs(n)
      val n1 = spire.math.abs(r.n)
      if (dgcd == 1L) {
        Rational(spire.math.gcd(n0, n1), SafeLong(d) * r.d)
      } else {
        val lm = d / dgcd
        val rm = r.d / dgcd
        Rational((SafeLong(n0) * rm) gcd (SafeLong(n1) * lm), SafeLong(dgcd) * lm * rm)
      }

    case r: BigRational =>
      val dgcd: Long = spire.math.gcd(d, (r.d % d).toLong)
      if (dgcd == 1L) {
        Rational(spire.math.gcd(spire.math.abs(n), spire.math.abs((r.n % n).toLong)),
          SafeLong(d) * r.d)
      } else {
        val lm = d / dgcd
        val rm = r.d / dgcd
        Rational((SafeLong(spire.math.abs(n)) * rm) gcd (r.n.abs * lm),
          SafeLong(dgcd) * lm * rm)
      }
  }

  def floor: Rational =
    if (d == 1L) this
    else if (n >= 0) Rational(n / d, 1L)
    else Rational(n / d - 1L, 1L)

  def ceil: Rational =
    if (d == 1L) this
    else if (n >= 0) Rational(n / d + 1L, 1L)
    else Rational(n / d, 1L)

  def round: Rational =
    if (n >= 0) {
      val m = (n % d)
      if (m >= (d - m)) Rational(n / d + 1) else Rational(n / d)
    } else {
      val m = -(n % d)
      if (m >= (d - m)) Rational(n / d - 1) else Rational(n / d)
    }

  def pow(exp: Int): Rational = if (exp == 0)
    Rational.one
  else if (exp < 0)
    reciprocal.pow(-exp)
  else
    Rational(SafeLong(n).pow(exp), SafeLong(d).pow(exp))

  def compareToOne: Int = n compare d

  def compare(r: Rational): Int = r match {
    case r: LongRational =>
      Checked.tryOrElse {
        LongAlgebra.compare(n * r.d, r.n * d)
      } {
        val dgcd = spire.math.gcd(d, r.d)
        if (dgcd == 1L)
          (SafeLong(n) * r.d) compare (SafeLong(r.n) * d)
        else
          (SafeLong(n) * (r.d / dgcd)) compare (SafeLong(r.n) * (d / dgcd))
      }

    case r: BigRational =>
      val dgcd = spire.math.gcd(d, (r.d % d).toLong)
      if (dgcd == 1L)
        (SafeLong(n) * r.d) compare (r.n * d)
      else
        (SafeLong(n) * (r.d / dgcd)) compare (r.n * (d / dgcd))
  }

  override def longValue: Long =
    if(d == 1L) n
    else n / d

  override def equals(that: Any): Boolean = that match {
    case that: LongRational => this.n == that.n && this.d == that.d
    case _ => super.equals(that)
  }

  override def hashCode: Int =
    if (d == 1L) unifiedPrimitiveHashcode
    else 29 * (37 * n.## + d.##)

  override def toString: String = if(isWhole) n.toString else s"$n/$d"
}

@SerialVersionUID(0L)
private[math] final case class BigRational(n: SafeLong, d: SafeLong) extends Rational with Serializable {
  def numerator: SafeLong = n
  def denominator: SafeLong = d

  def numeratorAsLong: Long = n.toLong
  def denominatorAsLong: Long = d.toLong

  def reciprocal: Rational = if (signum < 0)
    Rational(-d, -n)
  else
    BigRational(d, n)

  override def signum: Int = n.signum

  override def isWhole: Boolean = d.isOne

  override def isZero: Boolean = false

  override def isOne: Boolean = false

  override def isValidChar: Boolean = false

  override def isValidByte: Boolean = false

  override def isValidShort: Boolean = false

  override def isValidInt: Boolean = false

  override def isValidLong: Boolean = false

  override def toBigInt: BigInt = (n / d).toBigInt

  override def doubleValue: Double = Rational.toDouble(n, d)

  override def unary_-(): Rational = Rational(-n, d)

  def +(r: Rational): Rational = r match {
    case r: LongRational => r + this
    case r: BigRational =>
      val dgcd: SafeLong = d.gcd(r.d)
      if (dgcd.isOne) {
        Rational(r.d * n + r.n * d, r.d * d)
      } else {
        val lden: SafeLong = d / dgcd
        val rden: SafeLong = r.d / dgcd
        val num: SafeLong = rden * n + r.n * lden
        val ngcd: SafeLong = num.gcd(dgcd)
        if (ngcd.isOne)
          Rational(num, lden * r.d)
        else
          Rational(num / ngcd, (r.d / ngcd) * lden)
      }
  }

  def -(r: Rational): Rational = r match {
    case r: LongRational => (-r) + this
    case r: BigRational =>
      val dgcd: SafeLong = d.gcd(r.d)
      if (dgcd.isOne) {
        Rational(r.d * n - r.n * d, r.d * d)
      } else {
        val lden: SafeLong = d / dgcd
        val rden: SafeLong = r.d / dgcd
        val num: SafeLong = rden * n - r.n * lden
        val ngcd: SafeLong = num.gcd(dgcd)
        if (ngcd.isOne)
          Rational(num, lden * r.d)
        else
          Rational(num / ngcd, (r.d / ngcd) * lden)
      }
  }


  def *(r: Rational): Rational = r match {
    case r: LongRational => r * this
    case r: BigRational =>
      val a = n.gcd(r.d)
      val b = d.gcd(r.n)
      Rational((n / a) * (r.n / b), (d / b) * (r.d / a))
  }


  def /(r: Rational): Rational = r match {
    case r: LongRational => r.inverse * this
    case r: BigRational =>
      val a = n.gcd(r.n)
      val b = d.gcd(r.d)
      val num = (n / a) * (r.d / b)
      val den = (d / b) * (r.n / a)
      if (den.signum < 0) Rational(-num, -den) else Rational(num, den)
  }

  def gcd(r: Rational): Rational = r match {
    case r: LongRational => r gcd this
    case r: BigRational =>
      val dgcd: SafeLong = d.gcd(r.d)
      if (dgcd.isOne) {
        Rational(n.abs gcd r.n.abs, d * r.d)
      } else {
        val lm = d / dgcd
        val rm = r.d / dgcd
        Rational((n * rm).abs gcd (r.n * lm).abs, dgcd * lm * rm)
      }
  }

  def floor: Rational =
    if (isWhole) this
    else if (n.signum >= 0) Rational(n / d, BigInt(1))
    else Rational(n / d - 1, BigInt(1))

  def ceil: Rational =
    if (isWhole) this
    else if (n.signum >= 0) Rational(n / d + 1, BigInt(1))
    else Rational(n / d, BigInt(1))

  def round: Rational =
    if (n.signum >= 0) {
      val m = n % d
      if (m >= (d - m)) Rational(n / d + 1) else Rational(n / d)
    } else {
      val m = -(n % d)
      if (m >= (d - m)) Rational(n / d - 1) else Rational(n / d)
    }

  def pow(exp: Int): Rational = if (exp == 0)
    Rational.one
  else if (exp < 0)
    Rational(d pow -exp, n pow -exp)
  else
    Rational(n pow exp, d pow exp)

  def compareToOne: Int = n compare d

  def compare(r: Rational): Int = r match {
    case r: LongRational =>
      val dgcd = spire.math.gcd(r.d, (d % r.d).toLong)
      if (dgcd == 1L)
        (n * r.d) compare (SafeLong(r.n) * d)
      else
        (n * (r.d / dgcd)) compare (SafeLong(r.n) * (d / dgcd))
    case r: BigRational =>
      val dgcd = d.gcd(r.d)
      if (dgcd.isOne)
        (n * r.d) compare (r.n * d)
      else
        ((r.d / dgcd) * n) compare ((d / dgcd) * r.n)
  }

  override def equals(that: Any): Boolean = that match {
    case that: BigRational => this.n == that.n && this.d == that.d
    case _ => super.equals(that)
  }

  override def hashCode: Int =
    29 * (37 * n.## + d.##)

  override def toString: String = if(isWhole) n.toString else s"$n/$d"
}

trait RationalInstances {
  implicit final val RationalAlgebra = new RationalAlgebra
  import NumberTag._
  implicit final val RationalTag = new LargeTag[Rational](Exact, Rational.zero)

}

/**
 * Used for Fractional[Rational] and Numeric[Rational] to provide
 * approximate sqrt and nroot implementations.
 *
 * These operations occur at Double precision.
 */
private[math] trait RationalApproximateNRoot extends NRoot[Rational] {
  def nroot(n: Rational, k: Int): Rational =
    Rational(n.toDouble nroot k)

  def fpow(n: Rational, k: Rational): Rational =
    Rational(n.toDouble ** k.toDouble)
}

private[math] trait RationalIsField extends Field[Rational] {
  override def minus(a:Rational, b:Rational): Rational = a - b
  def negate(a:Rational): Rational = -a
  def one: Rational = Rational.one
  def plus(a:Rational, b:Rational): Rational = a + b
  override def pow(a:Rational, b:Int): Rational = a.pow(b)
  override def times(a:Rational, b:Rational): Rational = a * b
  def zero: Rational = Rational.zero
  def quot(a:Rational, b:Rational): Rational = a /~ b
  def mod(a:Rational, b:Rational): Rational = a % b
  override def quotmod(a:Rational, b:Rational): (Rational, Rational) = a /% b
  def gcd(a:Rational, b:Rational): Rational = a gcd b
  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
  def div(a:Rational, b:Rational): Rational = a / b
}

private[math] trait RationalIsReal extends IsRational[Rational] {
  override def eqv(x:Rational, y:Rational): Boolean = x == y
  override def neqv(x:Rational, y:Rational): Boolean = x != y
  override def gt(x: Rational, y: Rational): Boolean = x > y
  override def gteqv(x: Rational, y: Rational): Boolean = x >= y
  override def lt(x: Rational, y: Rational): Boolean = x < y
  override def lteqv(x: Rational, y: Rational): Boolean = x <= y
  def compare(x: Rational, y: Rational): Int = x compare y

  override def sign(a: Rational): Sign = a.sign
  def signum(a: Rational): Int = a.signum
  def abs(a: Rational): Rational = a.abs

  def toDouble(r: Rational): Double = r.toDouble
  def ceil(a:Rational): Rational = a.ceil
  def floor(a:Rational): Rational = a.floor
  def round(a:Rational): Rational = a.round
  def toRational(a: Rational): Rational = a
  def isWhole(a:Rational): Boolean = a.isWhole
}

@SerialVersionUID(1L)
class RationalAlgebra extends RationalIsField with RationalIsReal with Serializable
