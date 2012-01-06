package numerics.math

import scala.math.{ScalaNumber, ScalaNumericConversions, abs, min}
import Implicits._
//import Ordering.Implicits._


trait Fraction[@specialized(Long) A] {
  def num: A
  def den: A
}


sealed abstract class Rational extends ScalaNumber with ScalaNumericConversions with Ordered[Rational] {
  import LongRationals.LongRational
  import BigRationals.BigRational

  def numerator: BigInt
  def denominator: BigInt

  def abs: Rational = if (this < Rational.zero) -this else this
  def inverse: Rational = Rational.one / this
  def signum: Int = scala.math.signum(this compare Rational.zero)

  def unary_-(): Rational = Rational.zero - this

  def +(rhs: Rational): Rational
  def -(rhs: Rational): Rational
  def *(rhs: Rational): Rational
  def /(rhs: Rational): Rational

  def quot(rhs: Rational): Rational = Rational(SafeLong((this / rhs).toBigInt), SafeLong.one)
  def %(rhs: Rational): Rational = this - (this quot rhs)

  def toBigInt: BigInt
  def toBigDecimal: BigDecimal

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
  } else if (this == 0) {
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
}


object Rational {
  private val RationalString = """^(-?\d+)/(-?\d+)$"""r
  private val IntegerString = """^(-?\d+)$"""r

  import LongRationals.LongRational
  import BigRationals.BigRational

  val zero: Rational = LongRational(0L, 1L)
  val one: Rational = LongRational(1L, 1L)
  
  private[math] def apply(n: SafeLong, d: SafeLong): Rational = {
    n.foldWith[Rational,LongRational,BigRational](d)(LongRational(_, _), BigRational(_, _))
  }

  def apply(n: BigInt, d: BigInt): Rational = BigRationals.build(n, d)
  def apply(n: Long, d: Long): Rational = LongRationals.build(n, d)

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
    val n = (x / x.ulp).toBigInt
    val d = (BigDecimal(1.0) / x.ulp).toBigInt
    BigRationals.build(n, d)
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


  /**
   * Finds x ^ y in log y multiplications. If `y` is `0`, then `1` is returned.
   * If `y` is negative, then the result is undefined.
   *
   * TODO: This is really-out-of-place here :-\
   */
  def pow(x: Long, y: Int): Long = if (y == 0) {
    1
  } else if (y == 1) {
    x
  } else {
    val z = pow(x, y >>> 1)
    if ((y & 1) == 1) z * z * x else z * z
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




protected abstract class Rationals[@specialized(Long) A](implicit integral: Integral[A]) {
  import LongRationals._
  import BigRationals._
  import integral._

  def build(n: A, d: A): Rational

  trait RationalLike extends Rational with Fraction[A] {
    
    override def signum: Int = scala.math.signum(integral.compare(num, zero))

    def isWhole: Boolean = den == one

    def underlying = List(num, den)

    def toBigInt: BigInt = (integral.toBigInt(num) / integral.toBigInt(den))
    def toBigDecimal: BigDecimal = integral.toBigDecimal(num) / integral.toBigDecimal(den)

    def longValue = toBigInt.longValue    // Override if possible.
    def intValue = longValue.intValue
    def floatValue = doubleValue.toFloat

    def doubleValue: Double = if (num == zero) {
      0.0
    } else if (num < zero) {
      -((-this).toDouble)
    } else {

      // We basically just shift n so that integer division gives us 54 bits of
      // accuracy. We use the last bit for rounding, so end w/ 53 bits total.

      val n = integral.toBigInt(num)
      val d = integral.toBigInt(den)

      val sharedLength = scala.math.min(n.bitLength, d.bitLength)
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
      case that: Rational with Fraction[_] => num == that.num && den == that.den
      case that: BigInt => isWhole && toBigInt == that
      case that: BigDecimal =>
        try {
          toBigDecimal == that
        } catch {
          case ae: ArithmeticException => false
        }
      case that => unifiedPrimitiveEquals(that)
    }

    override def toString: String = "%s/%s" format (num.toString, den.toString)
  }
}


object LongRationals extends Rationals[Long] {
  import BigRationals.BigRational


  private[math] def gcd(a: Long, b: Long): Long = if (b == 0L) {
    abs(a)
  } else {
    gcd(b, a % b)
  }


  def build(n: Long, d: Long): Rational = {
    val divisor = gcd(n, d)
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

    def numerator = n.toBigInt
    def denominator = d.toBigInt

    override def unary_-(): Rational = if (n == Long.MinValue) {
      BigRational(-BigInt(Long.MinValue), BigInt(d))
    } else {
      LongRational(-n, d)
    }

    def +(r: Rational): Rational = r match {
      case r: LongRational =>
        val dgcd: Long = gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d + SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRational =>
        val dgcd: Long = gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n + r.n * d)
          val den = SafeLong(r.d * d)
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def -(r: Rational): Rational = r match {
      case r: LongRational =>
        val dgcd: Long = gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d - SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRational =>
        val dgcd: Long = gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n - r.n * d)
          val den = SafeLong(r.d * d)
          Rational(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rational(num, SafeLong(lden) * r.d)
          else
            Rational(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def *(r: Rational): Rational = r match {
      case r: LongRational =>
        val a = gcd(n, r.d)
        val b = gcd(d, r.n)
        Rational(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
      case r: BigRational =>
        val a = gcd(n, (r.d % n).toLong)
        val b = gcd(d, (r.n % d).toLong)
        Rational(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
    }


    def /(r: Rational): Rational = {
      if (r == Rational.zero) throw new IllegalArgumentException("/ by 0")
      r match {
        case r: LongRational => {
          val a = gcd(n, r.n)
          val b = gcd(d, r.d)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
        }
        case r: BigRational => {
          val a = gcd(n, (r.n % n).toLong)
          val b = gcd(d, (r.d % d).toLong)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
        }
      }
    }

    // FIXME: SafeLong would ideally support pow
    def pow(exp: Int): Rational = {
      val num = n.toBigInt.pow(exp.abs)
      val den = d.toBigInt.pow(exp.abs)
      if (exp > 0) BigRationals.build(num, den) else BigRationals.build(den, num)
    }

    def compare(r: Rational): Int = r match {
      case r: LongRational => {
        val dgcd = gcd(d, r.d)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
      case r: BigRational => {
        val dgcd = gcd(d, (r.d % d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
    }
  }
}


object BigRationals extends Rationals[BigInt] {
  import LongRationals.{LongRational,gcd}

  def build(n: BigInt, d: BigInt): Rational = {
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

    override def unary_-(): Rational = Rational(-SafeLong(n), SafeLong(d))

    def +(r: Rational): Rational = r match {
      case r: LongRational => r + this
      case r: BigRational =>
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
      case r: BigRational =>
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
      case r: BigRational =>
        val a = n.gcd(r.d)
        val b = d.gcd(r.n)
        Rational(SafeLong((n / a) * (r.n / b)), SafeLong((d / b) * (r.d / a)))
    }
    

    def /(r: Rational): Rational = r match {
      case r: LongRational => r.inverse * this
      case r: BigRational =>
        val a = n.gcd(r.n)
        val b = d.gcd(r.d)
        val num = SafeLong(n / a) * (r.d / b)
        val den = SafeLong(d / b) * (r.n / a)
        if (den < SafeLong.zero) Rational(-num, -den) else Rational(num, den)
    }

    def pow(exp: Int): Rational = if (exp < 0) {
      BigRationals.build(d pow exp.abs, n pow exp.abs)
    } else {
      BigRationals.build(n pow exp, d pow exp)
    }

    def compare(r: Rational): Int = r match {
      case r: LongRational => {
        val dgcd = gcd(r.d, (d % r.d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
      case r: BigRational => {
        val dgcd = d.gcd(r.d)
        if (dgcd == 1)
          (SafeLong(n * r.d) - r.n * d).signum
        else
          (SafeLong(r.d / dgcd) * n - SafeLong(d / dgcd) * r.n).signum
      }
    }
  }
}
