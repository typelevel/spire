package spire.math

import scala.annotation.tailrec
import scala.math.{ScalaNumber, ScalaNumericConversions}

import java.lang.Math

import spire.algebra._
import spire.algebra.Sign.{ Positive, Zero, Negative }

sealed abstract class Rational extends ScalaNumber with ScalaNumericConversions with Ordered[Rational] {
  import LongRationals.LongRational
  import BigRationals.BigRational

  def numerator: BigInt
  def denominator: BigInt

  def numeratorAsLong: Long
  def denominatorAsLong: Long

  def isWhole: Boolean

  // ugh, ScalaNumber and ScalaNumericConversions in 2.10 require this hack
  override def underlying: List[Any] = sys.error("unimplemented")

  def abs: Rational = if (this < Rational.zero) -this else this
  def inverse: Rational = Rational.one / this
  def reciprocal: Rational
  def signum: Int = numerator.signum

  def unary_-(): Rational = Rational.zero - this

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

  def toBigInt: BigInt
  def toBigDecimal: BigDecimal
  override def shortValue = longValue.toShort
  override def byteValue = longValue.toByte

  def floor: Rational = Rational(toBigInt)
  def ceil: Rational = if (denominator == 1) floor else Rational(toBigInt + 1)
  def round: Rational

  def pow(exp: Int): Rational

  /**
   * Returns this `Rational` to the exponent `exp`. Both the numerator and
   * denominator of `exp` must be valid integers. Anything larger will cause
   * `pow` to throw an `ArithmeticException`.
   */
  def pow(exp: Rational)(implicit ctxt: ApproximationContext[Rational]): Rational = {
    if (exp < 0) {
      this.inverse.pow(-exp)(ctxt)
    } else if (!(exp.numerator.isValidInt) || !(exp.denominator.isValidInt)) {
      throw new ArithmeticException("Exponent is too large!")
    } else {
      
      // nroot must be done last so the context is still valid, otherwise, we'd
      // need to adjust the error, as the absolute error would increase,
      // relatively, by (1 + e)^exp.numerator if nroot was done before the pow.

      (this pow exp.numerator.toInt).nroot(exp.denominator.toInt)(ctxt)
    }
  }


  /**
   * Find the n-th root of this `Rational`. This requires an (implicit)
   * `ApproximationContext` to bound the allowable absolute error of the answer.
   */
  def nroot(k: Int)(implicit ctxt: ApproximationContext[Rational]): Rational = if (k == 0) {
    Rational.one
  } else if (k < 0) {
    this.inverse.nroot(-k)(ctxt)
  } else if (numerator == 0) {
    Rational.zero
  } else {

    // TODO: Is this necessary with the better init. approx in the else?

    val (low, high) = this match {
      case LongRational(n, d) => {
        val n_ = Rational.nroot(n, k)
        val d_ = Rational.nroot(d, k)
        (Rational(n_._1, d_._2), Rational(n_._2, d_._1))
      }
      case BigRational(n, d) => {
        val n_ = Rational.nroot(n, k)
        val d_ = Rational.nroot(d, k)
        (Rational(n_._1, d_._2), Rational(n_._2, d_._1))
      }
    }
    
    if (low == high) {
      low
    } else {
    
      import Rational.{ nroot => intNroot }

      // To ensure the initial approximation is within (relatively) 1/n of the
      // actual root, we need to ensure the num. and den. are both >= min.
      // Otherwise, we need to find a single multiplier for them that can
      // guarantee this. From there, we can simply use the integer version of
      // nroot to get a good approximation.

      val min = (BigInt(k) * 2 + 1) pow k
      val mul = min / (this.numerator min this.denominator) + 1
      val numIntRt = intNroot(numerator * mul, k)
      val denIntRt = intNroot(denominator * mul, k)
      val low = Rational(numIntRt._1, denIntRt._2)
      val high = Rational(numIntRt._2, denIntRt._1)

      // Reduction in absolute error from n-th root algorithm:
      // Let x(k) be the approximation at step k, x(oo) be the n-th root. Let
      // e(k) be the relative error at step k, thus x(k) = x(oo)(1 + e(k)). If
      // x(0) > x(oo), then x(k) > x(oo) (this can be seen during the
      // derivation).
      //
      // x(k+1) = 1/n [(n-1) * x(k) + x(oo)^n / x(k)^(n-1)]
      //          1/n [(n-1) * x(oo) * (1 + e(k)) + x(oo)^n / (x(oo) * (1 + e(k)))^(n-1)]
      //          x(oo)[(n-1)*(1+e(k)) / n + 1 / (n * (1 + e(k))^(n-1))]
      //          x(oo)[1 + e(k) + (1 + e(k))/n + 1 / (n * (1 + e(k))^(n-1))]
      //          x(oo)[1 + e(k) * (1 - ((1 + e(k))^n - 1) / (e(k) * n * (1 + e(k))^(n-1))]
      //          x(oo)[1 + e(k) * (1 - ((1 + n*e(k) + nC2*e(k)^2 + ... + e(k)^n) - 1) / (.. as above ..))]
      //          x(oo)[1 + e(k) * (1 - (1 + nC2*e^2/n + ... + e^(n-1)/n) / (1 + e(k))^(n-1))]
      //        < x(oo)[1 + e(k) * (1 - 1 / (1 + e(k))^(n-1))]
      //        < x(oo)[1 + e(k) * (1 - 1 / (1 + 1/n)^(n-1))]
      // Let e = (1 + 1/n)^(n-1).
      //        < x(oo)[1 + e(k) * (e - 1 / e)]
      //          
      // So, we use a = (e - 1) / e as the relative error multiplier.

      val e = Rational(k + 1, k) pow (k - 1)
      val a = (e - 1) / e   // The relative error is multiplied by this each iter.
      val error = ctxt.error
      val absErr = high - low // I have no idea why I had this: high / k

      // absErr * a^k < error => a^k = error / absErr => k = log(error / absErr) / log(a)
      val maxiters = math.ceil(math.log((error / absErr).toDouble) / math.log(a.toDouble)).toInt
      
      // A single step of the nth-root algorithm.
      @inline def refine(x: Rational) = (x * (k - 1) + this / (x pow (k - 1))) / k

      def findNthRoot(prev: Rational, i: Int): Rational = if (i == maxiters) {
        prev
      } else {
        val next = refine(prev)
        
        // We know x(e0 - e1) > (1 - a)xe0, so xe0 < x(e0 - e1) / (1 - a).
        // Thus, if we have the difference, we can recalculate our guess of the
        // absolute error more "accurately" by dividing the difference of the
        // previous 2 guesses by (1 - a).
        //
        // This recalculation helps a lot. The numbers are a lot saner.

        // TODO: If we remove the iters constraint and just use this, will we
        //       ever perform worse than iters + 1 iterations? Need proof.

        if (prev == next || ((prev - next) / (Rational(1) - a)) < error) {
          prev
        } else {
          findNthRoot(next, i + 1)
        }
      }

      findNthRoot(high, 0)
    }
  }

  def sign: Sign = Sign(signum)

  def compareToOne: Int

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
    } else if (this < Rational(1)) {
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
  def limitDenominatorTo(limit: BigInt): Rational = {
    require(limit > 0, "Cannot limit denominator to non-positive number.")

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
      case Positive => closest(Rational(this.toBigInt), LongRationals.LongRational(1, 0))
      case Negative => closest(LongRationals.LongRational(-1, 0), Rational(this.toBigInt))
    }
  }
}


object Rational extends RationalInstances {
  private val RationalString = """^(-?\d+)/(-?\d+)$""".r
  private val IntegerString = """^(-?\d+)$""".r

  import LongRationals.LongRational
  import BigRationals.BigRational

  val zero: Rational = LongRational(0L, 1L)
  val one: Rational = LongRational(1L, 1L)
  
  def apply(n: SafeLong, d: SafeLong): Rational = {
    n.foldWith[Rational,LongRational,BigRational](d)(LongRational(_, _), BigRational(_, _))
  }

  def apply(n: Long, d: Long): Rational = LongRationals.build(n, d)
  def apply(n: BigInt, d: BigInt): Rational = BigRationals.build(n, d)

  protected[math] def unsafeBuild(n: Long, d: Long) = LongRationals.unsafeBuild(n, d)
  protected[math] def unsafeBuild(n: BigInt, d: BigInt) = BigRationals.unsafeBuild(n, d)

  implicit def apply(x:Long): Rational = LongRationals.build(x, 1L)
  implicit def apply(x:BigInt): Rational = BigRationals.build(x, BigInt(1))

  implicit def apply(x:Double): Rational = {
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
      BigRationals.build(x.toBigInt, 1)
    } else {
      val n = (x / x.ulp).toBigInt
      val d = (BigDecimal(1.0) / x.ulp).toBigInt
      BigRationals.build(n, d)
    }
  }

  def apply(r: String): Rational = r match {
    case RationalString(n, d) => Rational(BigInt(n), BigInt(d))
    case IntegerString(n) => Rational(BigInt(n))
    case s => try {
      Rational(BigDecimal(s))
    } catch {
      case nfe: NumberFormatException => throw new NumberFormatException("For input string: " + s)
    }
  }

  implicit def apply(x: SafeLong): Rational =
    x.fold(LongRationals.unsafeBuild(_, 1L), BigRationals.unsafeBuild(_, BigInt(1)))

  implicit def apply(x: Number): Rational = x match {
    case RationalNumber(n) => apply(n)
    case IntNumber(n) => apply(n)
    case FloatNumber(n) => apply(n)
    case DecimalNumber(n) => apply(n)
  }

  /**
   * Returns an interval that bounds the nth-root of the integer x.
   *
   * TODO: This is really out-of-place too.
   */
  def nroot(x: BigInt, n: Int): (BigInt, BigInt) = {
    def findnroot(prev: BigInt, add: Int): (BigInt, BigInt) = {
      val min = prev setBit add
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

    findnroot(BigInt(0), (x.bitLength + n - 1) / n) // ceil(x.bitLength / n)
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
}




private[math] abstract class Rationals[@specialized(Long) A](implicit integral: Integral[A]) {
  import LongRationals._
  import BigRationals._
  import integral._

  def build(n: A, d: A): Rational

  //sealed trait RationalLike extends Rational with Fraction[A] {
  sealed trait RationalLike extends Rational {
    def num: A
    def den: A
    
    override def signum: Int = scala.math.signum(integral.compare(num, zero))

    def isWhole: Boolean = den == one

    override def underlying = List(num, den)

    def toBigInt: BigInt = (integral.toBigInt(num) / integral.toBigInt(den))
    def toBigDecimal: BigDecimal = integral.toBigDecimal(num) / integral.toBigDecimal(den)

    def longValue = toBigInt.longValue    // Override if possible.
    def intValue = longValue.intValue

    def floatValue = doubleValue.toFloat

    def doubleValue: Double = if (num == zero) {
      0.0
    } else if (integral.lt(num, zero)) {
      -((-this).toDouble)
    } else {

      // We basically just shift n so that integer division gives us 54 bits of
      // accuracy. We use the last bit for rounding, so end w/ 53 bits total.

      val n = integral.toBigInt(num)
      val d = integral.toBigInt(den)

      val sharedLength = Math.min(n.bitLength, d.bitLength)
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
      

    override def hashCode: Int =
      if (isWhole && toBigInt == toLong) unifiedPrimitiveHashcode
      else 29 * (37 * num.## + den.##)

    override def equals(that: Any): Boolean = that match {
      //case that: Rational with Fraction[_] => num == that.num && den == that.den
      case that: RationalLike => num == that.num && den == that.den
      case that: BigInt => isWhole && toBigInt == that
      case that: BigDecimal =>
        try {
          toBigDecimal == that
        } catch {
          case ae: ArithmeticException => false
        }
      case that => unifiedPrimitiveEquals(that)
    }

    override def toString: String = if (den == 1L)
      num.toString
    else
      "%s/%s" format (num, den)
  }
}


private[math] object LongRationals extends Rationals[Long] {
  import BigRationals.BigRational

  def build(n: Long, d: Long): Rational = {
    if (d == 0) throw new IllegalArgumentException("0 denominator")
    unsafeBuild(n, d)
  }

  def unsafeBuild(n: Long, d: Long): Rational = {
    val divisor = spire.math.gcd(n, d)
    if (divisor == 1L) {
      if (d < 0)
        Rational(SafeLong(-n), SafeLong(-d))
      else
        LongRational(n, d)
    } else {
      if (d < 0)
        LongRational(-n / divisor, -d / divisor)
      else
        LongRational(n / divisor, d / divisor)
    }
  }

  case class LongRational private[math] (n: Long, d: Long) extends RationalLike {
    def num: Long = n
    def den: Long = d

    def numerator = ConvertableFrom[Long].toBigInt(n)
    def denominator = ConvertableFrom[Long].toBigInt(d)

    def numeratorAsLong: Long = n
    def denominatorAsLong: Long = d

    def reciprocal = if (n == 0L)
      throw new ArithmeticException("reciprocal called on 0/1")
    else if (n < 0L)
      LongRational(-d, -n)
    else
      LongRational(d, n)

    override def signum: Int = if (n > 0) 1 else if (n < 0) -1 else 0

    override def unary_-(): Rational = if (n == Long.MinValue) {
      BigRational(-BigInt(Long.MinValue), BigInt(d))
    } else {
      LongRational(-n, d)
    }

    def +(r: Rational): Rational = r match {
      case r: LongRationals.LongRational =>
        val dgcd: Long = spire.math.gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d + SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRational =>
        val dgcd: Long = spire.math.gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n + r.n * d)
          val den = SafeLong(r.d * d)
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def -(r: Rational): Rational = r match {
      case r: LongRationals.LongRational =>
        val dgcd: Long = spire.math.gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d - SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRational =>
        val dgcd: Long = spire.math.gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n - r.n * d)
          val den = SafeLong(r.d * d)
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(spire.math.gcd(_, dgcd), num => spire.math.gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def *(r: Rational): Rational = {
      if (n == 0L) Rational.zero else (r match {
        case r: LongRationals.LongRational =>
          val a = spire.math.gcd(n, r.d)
          val b = spire.math.gcd(d, r.n)
          Rational(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
        case r: BigRational =>
          val a = spire.math.gcd(n, (r.d % n).toLong)
          val b = spire.math.gcd(d, (r.n % d).toLong)
          Rational(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
      })
    }


    def /(r: Rational): Rational = {
      if (r == Rational.zero) throw new ArithmeticException("divide (/) by 0")
      r match {
        case r: LongRationals.LongRational => {
          val a = spire.math.gcd(n, r.n)
          val b = spire.math.gcd(d, r.d)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
        }
        case r: BigRational => {
          val a = spire.math.gcd(n, (r.n % n).toLong)
          val b = spire.math.gcd(d, (r.d % d).toLong)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
        }
      }
    }

    def gcd(r: Rational): Rational = r match {
      case r: LongRationals.LongRational =>
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
          Rational((SafeLong(spire.math.abs(n)) * rm) gcd (SafeLong(r.n.abs) * lm),
            SafeLong(dgcd) * lm * rm)
        }
    }

    def round: Rational = {
      val m = (n % d).abs
      if (m >= (d - m).abs) Rational(n / d + 1) else Rational(n / d)
    }

    def pow(exp: Int): Rational = if (exp == 0)
      Rational.one
    else if (exp < 0)
      reciprocal.pow(-exp)
    else
      Rational(SafeLong(n).pow(exp), SafeLong(d).pow(exp))

    def compareToOne: Int = n compare d

    def compare(r: Rational): Int = r match {
      case r: LongRationals.LongRational =>
        val dgcd = spire.math.gcd(d, r.d)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum

      case r: BigRational =>
        val dgcd = spire.math.gcd(d, (r.d % d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
    }
  }
}


private[math] object BigRationals extends Rationals[BigInt] {
  import LongRationals.LongRational

  def build(n: BigInt, d: BigInt): Rational = {
    if (d == 0) throw new IllegalArgumentException("0 denominator")
    unsafeBuild(n, d)
  }

  def unsafeBuild(n: BigInt, d:BigInt): Rational = {
    val gcd = n.gcd(d)
    if (gcd == 1) {
      if (d < 0)
        Rational(SafeLong(-n), SafeLong(-d))
      else
        Rational(SafeLong(n), SafeLong(d))
    } else {
      if (d < 0)
        Rational(-SafeLong(n / gcd), -SafeLong(d / gcd))
      else
        Rational(SafeLong(n / gcd), SafeLong(d / gcd))
    }
  }


  case class BigRational private[math] (n: BigInt, d: BigInt) extends RationalLike {
    def num: BigInt = n
    def den: BigInt = d

    def numerator = n
    def denominator = d

    def numeratorAsLong: Long = n.toLong
    def denominatorAsLong: Long = d.toLong

    def reciprocal = if (n == 0)
      throw new ArithmeticException("reciprocal called on 0/1")
    else if (n < 0)
      BigRational(-d, -n)
    else
      BigRational(d, n)

    override def signum: Int = n.signum

    override def unary_-(): Rational = Rational(-SafeLong(n), SafeLong(d))

    def +(r: Rational): Rational = r match {
      case r: LongRational => r + this
      case r: BigRationals.BigRational =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rational(SafeLong(r.d * n + r.n * d), SafeLong(r.d * d))
        } else {
          val lden: BigInt = d / dgcd
          val rden: BigInt = r.d / dgcd
          val num: BigInt = rden * n + r.n * lden
          val ngcd: BigInt = num.gcd(dgcd)
          if (ngcd == 1)
            Rational(SafeLong(num), SafeLong(lden * r.d))
          else
            Rational(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
        }
    }


    def -(r: Rational): Rational = r match {
      case r: LongRational => (-r) + this
      case r: BigRationals.BigRational =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rational(SafeLong(r.d * n - r.n * d), SafeLong(r.d * d))
        } else {
          val lden: BigInt = d / dgcd
          val rden: BigInt = r.d / dgcd
          val num: BigInt = rden * n - r.n * lden
          val ngcd: BigInt = num.gcd(dgcd)
          if (ngcd == 1)
            Rational(SafeLong(num), SafeLong(lden * r.d))
          else
            Rational(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
        }
    }


    def *(r: Rational): Rational = r match {
      case r: LongRational => r * this
      case r: BigRationals.BigRational =>
        val a = n.gcd(r.d)
        val b = d.gcd(r.n)
        Rational(SafeLong((n / a) * (r.n / b)), SafeLong((d / b) * (r.d / a)))
    }


    def /(r: Rational): Rational = r match {
      case r: LongRational => r.inverse * this
      case r: BigRationals.BigRational =>
        val a = n.gcd(r.n)
        val b = d.gcd(r.d)
        val num = SafeLong(n / a) * (r.d / b)
        val den = SafeLong(d / b) * (r.n / a)
        if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
    }

    def gcd(r: Rational): Rational = r match {
      case r: LongRational => r gcd this
      case r: BigRationals.BigRational =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rational(n.abs gcd r.n.abs, d * r.d)
        } else {
          val lm = d / dgcd
          val rm = r.d / dgcd
          Rational((n * rm).abs gcd (r.n * lm).abs, dgcd * lm * rm)
        }
    }

    def round: Rational = {
      val m = (n % d).abs
      if (m >= (d - m).abs) Rational(n / d + 1) else Rational(n / d)
    }

    def pow(exp: Int): Rational = if (exp == 0)
      Rational.one
    else if (exp < 0)
      BigRationals.build(d pow -exp, n pow -exp)
    else
      BigRationals.build(n pow exp, d pow exp)

    def compareToOne: Int = n compare d

    def compare(r: Rational): Int = r match {
      case r: LongRational => {
        val dgcd = spire.math.gcd(r.d, (d % r.d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
      case r: BigRationals.BigRational => {
        val dgcd = d.gcd(r.d)
        if (dgcd == 1)
          (SafeLong(n * r.d) - r.n * d).signum
        else
          (SafeLong(r.d / dgcd) * n - SafeLong(d / dgcd) * r.n).signum
      }
    }
  }
}

trait RationalInstances {
  implicit final val RationalAlgebra = new RationalIsField {}
  implicit def RationalIsNRoot(implicit c:ApproximationContext[Rational]) = new RationalIsNRoot {
    implicit def context = c
  }
  implicit final val RationalIsReal = new RationalIsReal {}
}

private[math] trait RationalIsField extends Field[Rational] {
  override def minus(a:Rational, b:Rational): Rational = a - b
  def negate(a:Rational): Rational = -a
  def one: Rational = Rational.one
  def plus(a:Rational, b:Rational): Rational = a + b
  override def pow(a:Rational, b:Int): Rational = a.pow(b)
  override def times(a:Rational, b:Rational): Rational = a * b
  def zero: Rational = Rational.zero
  def quot(a:Rational, b:Rational) = a /~ b
  def mod(a:Rational, b:Rational) = a % b
  override def quotmod(a:Rational, b:Rational) = a /% b
  def gcd(a:Rational, b:Rational):Rational = a gcd b
  override def fromInt(n: Int): Rational = Rational(n)
  override def fromDouble(n: Double): Rational = Rational(n)
  def div(a:Rational, b:Rational) = a / b
}

private[math] trait RationalIsNRoot extends NRoot[Rational] {
  implicit def context:ApproximationContext[Rational]
  def nroot(a: Rational, k: Int): Rational = a.nroot(k)
  def fpow(a: Rational, b: Rational): Rational = a.pow(b)
}

private[math] trait RationalIsReal extends IsReal[Rational] {
  override def eqv(x:Rational, y:Rational) = x == y
  override def neqv(x:Rational, y:Rational) = x != y
  override def gt(x: Rational, y: Rational) = x > y
  override def gteqv(x: Rational, y: Rational) = x >= y
  override def lt(x: Rational, y: Rational) = x < y
  override def lteqv(x: Rational, y: Rational) = x <= y
  def compare(x: Rational, y: Rational) = if (x < y) -1 else if (x > y) 1 else 0

  override def sign(a: Rational): Sign = a.sign
  def signum(a: Rational): Int = a.signum
  def abs(a: Rational): Rational = a.abs

  def toDouble(r: Rational): Double = r.toDouble
  def ceil(a:Rational): Rational = a.ceil
  def floor(a:Rational): Rational = a.floor
  def round(a:Rational): Rational = a.round
  def isWhole(a:Rational) = a.denominator == 1
}
