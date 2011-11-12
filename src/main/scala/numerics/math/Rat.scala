package numerics.math


import scala.math.{ScalaNumber, ScalaNumericConversions, abs, min}
import Implicits._
import Ordering.Implicits._


trait Fraction[@specialized(Long) A] {
  def num: A
  def den: A
}


sealed abstract class Rat extends ScalaNumber with ScalaNumericConversions with Ordered[Rat] {
  import LongRats.LongRat
  import BigRats.BigRat

  def numerator: BigInt
  def denominator: BigInt

  def abs: Rat = if (this < Rat.zero) -this else this
  def inverse: Rat = Rat.one / this
  def signum: Int = scala.math.signum(this compare Rat.zero)

  def unary_-(): Rat = Rat.zero - this

  def +(rhs: Rat): Rat
  def -(rhs: Rat): Rat
  def *(rhs: Rat): Rat
  def /(rhs: Rat): Rat

  def quot(rhs: Rat): Rat = Rat(SafeLong((this / rhs).toBigInt), SafeLong.one)
  def %(rhs: Rat): Rat = this - (this quot rhs)

  def toBigInt: BigInt
  def toBigDecimal: BigDecimal

  def pow(exp: Int): Rat
}


object Rat {
  private val RationalString = """^(-?\d+)/(-?\d+)$"""r
  private val IntegerString = """^(-?\d+)$"""r

  import LongRats.LongRat
  import BigRats.BigRat

  val zero: Rat = LongRat(0L, 1L)
  val one: Rat = LongRat(1L, 1L)
  
  private[math] def apply(n: SafeLong, d: SafeLong): Rat = {
    n.foldWith[Rat,LongRat,BigRat](d)(LongRat(_, _), BigRat(_, _))
  }

  def apply(n: BigInt, d: BigInt): Rat = BigRats.build(n, d)
  def apply(n: Long, d: Long): Rat = LongRats.build(n, d)

  implicit def apply(x:Long): Rat = LongRats.build(x, 1L)
  implicit def apply(x:BigInt): Rat = BigRats.build(x, BigInt(1))

  implicit def apply(x:Double): Rat = {
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

  implicit def apply(x:BigDecimal): Rat = {
    val n = (x / x.ulp).toBigInt
    val d = (BigDecimal(1.0) / x.ulp).toBigInt
    BigRats.build(n, d)
  }

  def apply(r: String): Rat = r match {
    case RationalString(n, d) => Rat(BigInt(n), BigInt(d))
    case IntegerString(n) => Rat(BigInt(n))
    case s => try {
      Rat(BigDecimal(s))
    } catch {
      case nfe: NumberFormatException => throw new NumberFormatException("For input string: " + s)
    }
  }
}




protected abstract class Rats[@specialized(Long) A](implicit integral: Integral[A]) {
  import LongRats._
  import BigRats._
  import integral._

  def build(n: A, d: A): Rat

  trait RatLike extends Rat with Fraction[A] {
    
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
      case that: Rat with Fraction[_] => num == that.num && den == that.den
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


object LongRats extends Rats[Long] {
  import BigRats.BigRat


  private[math] def gcd(a: Long, b: Long): Long = if (b == 0L) {
    abs(a)
  } else {
    gcd(b, a % b)
  }


  def build(n: Long, d: Long): Rat = {
    val divisor = gcd(n, d)
    if (divisor == 1L) {
      if (d < 0)
        Rat(SafeLong(-n), SafeLong(-d))
      else
        LongRat(n, d)
    } else {
      if (d < 0)
        LongRat(-n / divisor, -d / divisor)
      else
        LongRat(n / divisor, d / divisor)
    }
  }


  case class LongRat private[math] (n: Long, d: Long) extends RatLike {
    def num: Long = n
    def den: Long = d

    def numerator = n.toBigInt
    def denominator = d.toBigInt

    override def unary_-(): Rat = if (n == Long.MinValue) {
      BigRat(-BigInt(Long.MinValue), BigInt(d))
    } else {
      LongRat(-n, d)
    }

    def +(r: Rat): Rat = r match {
      case r: LongRat =>
        val dgcd: Long = gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d + SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rat(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rat(num, SafeLong(lden) * r.d)
          else
            Rat(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRat =>
        val dgcd: Long = gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n + r.n * d)
          val den = SafeLong(r.d * d)
          Rat(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n + SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rat(num, SafeLong(lden) * r.d)
          else
            Rat(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def -(r: Rat): Rat = r match {
      case r: LongRat =>
        val dgcd: Long = gcd(d, r.d)
        if (dgcd == 1L) {

          val num = SafeLong(n) * r.d - SafeLong(r.n) * d
          val den = SafeLong(d) * r.d
          Rat(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: Long = r.d / dgcd
          val num: SafeLong = SafeLong(n) * rden - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rat(num, SafeLong(lden) * r.d)
          else
            Rat(num / ngcd, SafeLong(lden) * (r.d / ngcd))
        }
      case r: BigRat =>
        val dgcd: Long = gcd(d, (r.d % d).toLong)
        if (dgcd == 1L) {

          val num = SafeLong(r.d * n - r.n * d)
          val den = SafeLong(r.d * d)
          Rat(num, den)

        } else {

          val lden: Long = d / dgcd
          val rden: SafeLong = SafeLong(r.d) / dgcd
          val num: SafeLong = rden * n - SafeLong(r.n) * lden
          val ngcd: Long = num.fold(gcd(_, dgcd), num => gcd(dgcd, (num % dgcd).toLong))
          if (ngcd == 1L)
            Rat(num, SafeLong(lden) * r.d)
          else
            Rat(num / ngcd, SafeLong(r.d / ngcd) * lden)

        }
    }


    def *(r: Rat): Rat = r match {
      case r: LongRat =>
        val a = gcd(n, r.d)
        val b = gcd(d, r.n)
        Rat(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
      case r: BigRat =>
        val a = gcd(n, (r.d % n).toLong)
        val b = gcd(d, (r.n % d).toLong)
        Rat(SafeLong(n / a) * (r.n / b), SafeLong(d / b) * (r.d / a))
    }


    def /(r: Rat): Rat = {
      if (r == Rat.zero) throw new IllegalArgumentException("/ by 0")
      r match {
        case r: LongRat => {
          val a = gcd(n, r.n)
          val b = gcd(d, r.d)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rat(-num, -den) else Rat(num, den)
        }
        case r: BigRat => {
          val a = gcd(n, (r.n % n).toLong)
          val b = gcd(d, (r.d % d).toLong)
          val num = SafeLong(n / a) * (r.d / b)
          val den = SafeLong(d / b) * (r.n / a)
          if (den < SafeLong.zero) Rat(-num, -den) else Rat(num, den)
        }
      }
    }

    // FIXME: SafeLong would ideally support pow
    def pow(exp: Int): Rat = {
      val num = n.toBigInt.pow(exp.abs)
      val den = d.toBigInt.pow(exp.abs)
      if (exp > 0) BigRats.build(num, den) else BigRats.build(den, num)
    }

    def compare(r: Rat): Int = r match {
      case r: LongRat => {
        val dgcd = gcd(d, r.d)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
      case r: BigRat => {
        val dgcd = gcd(d, (r.d % d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
    }
  }
}


object BigRats extends Rats[BigInt] {
  import LongRats.{LongRat,gcd}

  def build(n: BigInt, d: BigInt): Rat = {
    val gcd = n.gcd(d)
    if (gcd == 1) {
      if (d < 0)
        Rat(SafeLong(-n), SafeLong(-d))
      else
        Rat(SafeLong(n), SafeLong(d))
    } else {
      if (d < 0)
        Rat(-SafeLong(n / gcd), -SafeLong(d / gcd))
      else
        Rat(SafeLong(n / gcd), SafeLong(d / gcd))
    }
  }


  case class BigRat private[math] (n: BigInt, d: BigInt) extends RatLike {
    def num: BigInt = n
    def den: BigInt = d

    def numerator = n
    def denominator = d

    override def unary_-(): Rat = Rat(-SafeLong(n), SafeLong(d))

    def +(r: Rat): Rat = r match {
      case r: LongRat => r + this
      case r: BigRat =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rat(SafeLong(r.d * n + r.n * d), SafeLong(r.d * d))
        } else {
          val lden: BigInt = d / dgcd
          val rden: BigInt = r.d / dgcd
          val num: BigInt = rden * n + r.n * lden
          val ngcd: BigInt = num.gcd(dgcd)
          if (ngcd == 1)
            Rat(SafeLong(num), SafeLong(lden * r.d))
          else
            Rat(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
        }
    }


    def -(r: Rat): Rat = r match {
      case r: LongRat => (-r) + this
      case r: BigRat =>
        val dgcd: BigInt = d.gcd(r.d)
        if (dgcd == 1) {
          Rat(SafeLong(r.d * n - r.n * d), SafeLong(r.d * d))
        } else {
          val lden: BigInt = d / dgcd
          val rden: BigInt = r.d / dgcd
          val num: BigInt = rden * n - r.n * lden
          val ngcd: BigInt = num.gcd(dgcd)
          if (ngcd == 1)
            Rat(SafeLong(num), SafeLong(lden * r.d))
          else
            Rat(SafeLong(num / ngcd), SafeLong(r.d / ngcd) * lden)
        }
    }


    def *(r: Rat): Rat = r match {
      case r: LongRat => r * this
      case r: BigRat =>
        val a = n.gcd(r.d)
        val b = d.gcd(r.n)
        Rat(SafeLong((n / a) * (r.n / b)), SafeLong((d / b) * (r.d / a)))
    }
    

    def /(r: Rat): Rat = r match {
      case r: LongRat => r.inverse * this
      case r: BigRat =>
        val a = n.gcd(r.n)
        val b = d.gcd(r.d)
        val num = SafeLong(n / a) * (r.d / b)
        val den = SafeLong(d / b) * (r.n / a)
        if (den < SafeLong.zero) Rat(-num, -den) else Rat(num, den)
    }

    def pow(exp: Int): Rat = if (exp < 0) {
      BigRats.build(d pow exp.abs, n pow exp.abs)
    } else {
      BigRats.build(n pow exp, d pow exp)
    }

    def compare(r: Rat): Int = r match {
      case r: LongRat => {
        val dgcd = gcd(r.d, (d % r.d).toLong)
        if (dgcd == 1L)
          (SafeLong(n) * r.d - SafeLong(r.n) * d).signum
        else
          (SafeLong(n) * (r.d / dgcd) - SafeLong(r.n) * (d / dgcd)).signum
      }
      case r: BigRat => {
        val dgcd = d.gcd(r.d)
        if (dgcd == 1)
          (SafeLong(n * r.d) - r.n * d).signum
        else
          (SafeLong(r.d / dgcd) * n - SafeLong(d / dgcd) * r.n).signum
      }
    }
  }
}
