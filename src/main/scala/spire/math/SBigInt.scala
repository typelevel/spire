package spire.math

import scala.math.{ ScalaNumber, ScalaNumericConversions }
import scala.math.{ BigInt => _ }
import SBigInt._
import scala.collection.immutable.NumericRange

object SBigInt {
  // 2.10 got this, but 2.9 not. So we define it here for now.
  def ??? = throw new UnsupportedOperationException
  
  /**
   * While addition, subtraction and multiplication work the same on signed 
   * and unsigned numbers, this is necessary to convert an signed to an unsigned value.
   */
  final val UnsignedIntMask: Long = 0xFFFFFFFFL
  
  /**
   * 0 is a bit special: It is the only value which is allowed to have
   * a 0 sign and an empty array as the magnitude.
   * 
   * Zero is the only instance of this value.
   */
  final val Zero: SBigInt = new SBigInt(0, Array[Int]())
  final val One: SBigInt = new SBigInt(1, Array(1))
  
  def apply(num: Int): SBigInt = {
    if (num == 0) return SBigInt.Zero
    // Invariant: Not Zero
    var newNum = 0
    var newSign = 0

    if (num < 0) {
      // Make num positive again ...
      newNum = -num
      // and make the sign negative.
      newSign = -1
    } else {
      newNum = num
      newSign = 1
    }

    assert(newSign != 0)
    assert(newNum != 0)

    new SBigInt(newSign, Array(newNum))
  }

  def apply(num: Long): SBigInt = {
    if (num == 0) return SBigInt.Zero
    // Invariant: Not Zero
    var newNum = 0L
    var newSign = 0

    if (num < 0) {
      // Make num positive again ...
      newNum = -num
      // and make the sign negative.
      newSign = -1
    } else {
      newNum = num
      newSign = 1
    }

    assert(newSign != 0)
    assert(newNum != 0)

    val upperInt: Int = (newNum >>> 32).toInt
    if (upperInt != 0)
    // We need an array with 2 elements
      new SBigInt(newSign, Array(upperInt, newNum.toInt))
    else
    // We need an array with 1 element
      new SBigInt(newSign, Array(newNum.toInt))
  }
  def fromString(s: String) = ???
  def fromStringOfRadix(s: String, radix: Int) = ???
    
  def fromArray(arr: Array[Int]) = ???
  
  /** Implicit conversion from `Int` to `SBigInt`. */
  implicit def int2bigInt(i: Int): SBigInt = apply(i)

  /** Implicit conversion from `Long` to `SBigInt`. */
  implicit def long2bigInt(l: Long): SBigInt = apply(l)
}

/**
 * Number class for signed whole numbers with unlimited precision.
 * 
 * Internally, the sign is stored as an Int and the magnitude as the two complement in a BE array.
 * 
 * It is made sure that the same value has the same underlying representation.
 * 
 * TODO: Verify that private[math] works as intended when used from Java.
 */
final class SBigInt private(final val signum: Int, final private[math] val arr: Array[Int]) extends ScalaNumber with ScalaNumericConversions with Ordered[SBigInt] with Serializable {
  type UInt = Int
  
  def isWhole: Boolean = true
  def underlying = SBigInt.this

  /**
   * Returns this value as a `Long`.
   * If the magnitude is too large, the lowest 64 bits will be returned. 
   */
  def longValue: Long = ???
  /**
   * Returns this value as an `Int`.
   * If the magnitude is too large, the lowest 32 bits will be returned. 
   */
  def intValue: Int = ???
  /**
   * Returns this value as a `Float`. Might lose precision.
   * If the magnitude is too large, `Float.MaxValue` (iff `sign == 1`) 
   * or `Float.MinValue` (iff `sign == -1`) are returned.
   */
  def floatValue: Float = {
    //TODO: Look up max magnitude representable as a Float.
    val maxMag = -42
    if(arr.length <= maxMag) {
      //Convert to Float
      ???
    } else {
      if(signum == 1) 
        Float.MaxValue
      else 
        Float.MinValue
    }
  }
  /**
   * Returns this value as a `Double`. Might lose precision.
   * If the magnitude is too large, `Double.MaxValue` (iff `sign == 1`) 
   * or `Double.MinValue` (iff `sign == -1`) are returned.
   */
  def doubleValue: Double = {
    //TODO: Look up max magnitude representable as a Double.
    val maxMag = -42
    if(arr.length <= maxMag) {
      //Convert to Double
      ???
    } else {
      if(signum == 1) 
        Double.MaxValue
      else 
        Double.MinValue
    }
  }
  
  def compare(rhs: SBigInt): Int = ??? 
    
  def abs = if (signum < 0) -this else this
  def unary_! : SBigInt = ???
  def unary_~ : SBigInt = ???
  def unary_- : SBigInt = new SBigInt(-signum, arr)

  
  def +(rhs: SBigInt): SBigInt = {
    // Check if one of the numbers are zero and return the other one.
    if (rhs.signum == 0) return SBigInt.this
    if (SBigInt.this.signum == 0) return rhs

    // Check if both numbers have the same sign.
    // If true, keep the sign and add the numbers.
    if (SBigInt.this.signum == rhs.signum)
      return new SBigInt(SBigInt.this.signum,
        if (SBigInt.this.arr.length >= rhs.arr.length)
          arrayPlusArray(SBigInt.this.arr, rhs.arr)
        else
          arrayPlusArray(rhs.arr, SBigInt.this.arr))
    
    ???
    
  }
  
  def -(rhs: SBigInt): SBigInt = {
    // Check if the other number is zero.
    if (rhs.signum == 0) return SBigInt.this
    // If this is Zero, return the other one, negated.
    if (SBigInt.this.signum == 0) return -rhs
    // Invariant: Both numbers are non-zero.

    if (SBigInt.this.signum != rhs.signum)
      return new SBigInt(SBigInt.this.signum,
        if (SBigInt.this.arr.length >= rhs.arr.length)
          arrayPlusArray(SBigInt.this.arr, rhs.arr)
        else
          arrayPlusArray(rhs.arr, SBigInt.this.arr))
    
    ???
  }
  
  def *(rhs: SBigInt): SBigInt = {
    if (SBigInt.this.signum == 0 || rhs.signum == 0)
      return SBigInt.Zero
      
    if (SBigInt.this == SBigInt.One) 
      return rhs
    if (rhs == SBigInt.One) 
      return SBigInt.this
            
    ???
  }
  
  def /(rhs: SBigInt): SBigInt = {
    if (rhs == SBigInt.One)
      return SBigInt.this
      
    ???
  }
  def %(rhs: SBigInt): SBigInt = ???
  def /% (rhs: SBigInt): (SBigInt, SBigInt) = (SBigInt.this / rhs, SBigInt.this % rhs)

  /** Leftshift of SBigInt */
  def << (n: Int): SBigInt = ???

  /** (Signed) rightshift of SBigInt */
  def >> (n: Int): SBigInt = ???

  /** Bitwise and of SBigInts */
  def &  (rhs: SBigInt): SBigInt = ???

  /** Bitwise or of SBigInts */
  def |  (rhs: SBigInt): SBigInt = ???

  /** Bitwise exclusive-or of SBigInts
   */
  def ^  (rhs: SBigInt): SBigInt = ???

  /** Bitwise and-not of SBigInts. Returns a SBigInt whose value is (this & ~rhs). */
  def &~ (rhs: SBigInt): SBigInt = ???

  /** Returns the greatest common divisor of abs(this) and abs(rhs) */
  def gcd(rhs: SBigInt): SBigInt = ???

  /** Returns a SBigInt whose value is (this mod m).
   *  This method differs from `%` in rhs it always returns a non-negative SBigInt. */
  def mod(rhs: SBigInt): SBigInt = ???

  /** Returns the minimum of this and rhs */
  def min(rhs: SBigInt): SBigInt = ???

  /** Returns the maximum of this and rhs */
  def max(rhs: SBigInt): SBigInt = ???

  /** Returns a SBigInt whose value is (`this` raised to the power of `exp`). */
  def pow (exp: Int): SBigInt = ???

  // We don't use Integral, right? What to do here?
  //def until(end: SBigInt, step: SBigInt = SBigInt(1)) = NumericRange(this, end, step)
  //def to(end: SBigInt, step: SBigInt = SBigInt(1)) = NumericRange.inclusive(this, end, step)
  
  def isZero: Boolean = this eq SBigInt.Zero 
  
  def isValidLong: Boolean = this >= Long.MinValue && this <= Long.MaxValue

  /** Compares this SBigInt with the specified value for equality. */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: SBigInt     => this equalsSBigInt rhs
    case rhs: BigDecimal => rhs.toBigIntExact exists (SBigInt.this equals _)
    case x                => isValidLong && unifiedPrimitiveEquals(x)
  }
  
  //private def equalsBigInt(rhs: BigInt): Boolean = compare(rhs) == 0
  private def equalsSBigInt(rhs: SBigInt): Boolean = 
    (this eq rhs) || 
    (this.signum == 0 && rhs.signum == 0) || 
    (this.signum == rhs.signum) && sameArrayContents(this.arr, rhs.arr)
  
  override def hashCode: Int =
    if (isValidLong) unifiedPrimitiveHashcode
    else ???
    
  override def toString = signToString(signum) + intsToString(arr)
  
  private def signToString(sign: Int) = if (sign == -1) "-" else ""

  private def intsToString(pArr: Array[Int]): String = {
    val iArr: Array[Int] = new Array[Int](pArr.length)
    System.arraycopy(pArr, 0, iArr, 0, pArr.length)
    val ret: Array[Char] = new Array[Char](10 * iArr.length)
    var retIndex: Int = ret.length
    var stop = false
    var result: String = null

    while (!stop) {
      var isZero: Boolean = true
      var carry: Int = 0

      var i: Int = 0
      while (i < iArr.length) {
        var value: Long = unsignedIntToLong(iArr(i))
        if (value != 0L) isZero = false
        value += carry * Radix
        carry = (value % 10L).toInt
        value /= 10L
        iArr(i) = longToUnsignedInt(value)

        i += 1;

      }

      if (isZero) {
        if (retIndex == ret.length) {
          stop = true;
          result = "0"
        } else {
          stop = true;
          result = new String(ret, retIndex, ret.length - retIndex)
        }
      }
      if (!stop) {
        assert((retIndex > 0))

        ret(({
          retIndex -= 1;
          retIndex //+ 1
        })) = (carry + '0'.toInt).toChar
      }
    }

    result
  }
  
  @inline private final def unsignedIntToLong(unsignedInt: UInt): Long =
    unsignedInt & UnsignedIntMask

  private def longToUnsignedInt(long: Long): UInt = {
    assert(long >= 0L)
    assert(long < Radix)
    if (long <= Int.MaxValue.toLong) return long.toInt
    else return (long - Radix).toInt
  }

  private final val Radix: Long = -2L * Int.MinValue.toLong
  
  
    
    
  ////////////////////////////////////////////////////////////////////////////////

  private def arrayPlusArray(lhs: Array[Int], rhs: Array[Int]): Array[Int] = ???
  private def arrayMinusArray(lhs: Array[Int], rhs: Array[Int]): Array[Int] = ???
  
  private def multLong(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multKaratsuba(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multToomCook(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multSchönhageStrassen(lhs: Array[Int], rhs: Array[Int]) = ???

  // Don't trust sameContents.
  private def sameArrayContents(a: Array[Int], b: Array[Int]): Boolean = {
    val aLen = a.length
    if (aLen != b.length)
      return false
    var i = 0
    while (i < aLen) {
      if (a(i) != b(i)) {
        return false
      }
      i += 1
    }

    return true
  }  
}