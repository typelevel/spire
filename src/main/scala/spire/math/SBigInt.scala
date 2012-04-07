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
  
  def apply(str: String): SBigInt = {
    //Ugly but potentially faster than Character.isDigit etc.  
    @inline def fastDigit(c: Char) = 
      if(c >= '0' && c <= '9') c - '0'
      else -1
    
    val len = str.length
    if(len == 0)
      throw new NumberFormatException
      
    var start = 0
    var firstChar = str(0) 
    
    val sign = 
      if(firstChar == '+') {
       start = 1
       1
      } else if(firstChar == '-') {
        start = 1
        -1
      } else
        1

   /* If there are no more than 9/18 digits (Int/Long.MaxValue.toString.length == 10/19)
    * the String can safely converted to Int/Long.
    */
    if(len-start < 9) return apply(str.toInt)
    if(len-start < 19) return apply(str.toLong)
    
    /*
     * String is longer ...
     */
    
    println(str)
    
    ???
  }
  
  def fromStringOfRadix(s: String, radix: Int) = ???
    
  def fromArray(arr: Array[Int]) = ???
  
  /**
   * Returns the original array if unchanged.
   */
  private def stripLeadingZeroes(arr: Array[Int]): Array[Int] = {
    var i = arr.length -1
    var empty = -1
    var stop = false
    while(i >= 0 && stop == false){
      if(arr(i) == 0)
        empty = i
      else
        stop = true
      i -= 1
    }
    if (empty == -1) {
      arr
    } else {
      val newArr = new Array[Int](empty)
      System.arraycopy(arr, 0, newArr, 0, empty)
      newArr
    }
  }
  
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
@SerialVersionUID(1L)
final class SBigInt private(final val signum: Int, final private[math] val arr: Array[Int]) extends ScalaNumber with ScalaNumericConversions with Ordered[SBigInt] with Serializable {
  type UInt = Int
  
  def isWhole: Boolean = true
  def underlying = this
  def bigInteger: java.math.BigInteger = {
    // Avoid copying of potentially large arrays.
    val ctor = classOf[java.math.BigInteger].getDeclaredConstructor(classOf[Array[Int]], classOf[Int])
    ctor setAccessible true
    ctor.newInstance(arr, signum.asInstanceOf[Object])
  }

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
    //TODO: Look up max arr size representable as a Double.
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
    //TODO: Look up max arr size representable as a Double.
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
    if (rhs.isZero) return this
    if (this.isZero) return rhs

    // Check if both numbers have the same sign.
    // If true, keep the sign and add the numbers.
    if (this.signum == rhs.signum)
      return new SBigInt(this.signum,
        if (this.arr.length >= rhs.arr.length)
          arrayPlusArray(this.arr, rhs.arr)
        else
          arrayPlusArray(rhs.arr, this.arr))
    
    ???
    
  }
  
  def -(rhs: SBigInt): SBigInt = {
    // Check if the other number is zero.
    if (rhs.signum == 0) return this
    // If this is Zero, return the other one, negated.
    if (this.signum == 0) return -rhs
    // Invariant: Both numbers are non-zero.

    if (this.signum != rhs.signum)
      return new SBigInt(this.signum,
        if (this.arr.length >= rhs.arr.length)
          arrayPlusArray(this.arr, rhs.arr)
        else
          arrayPlusArray(rhs.arr, this.arr))
    
    ???
  }
  
  def *(rhs: SBigInt): SBigInt = {
    if (this.isZero || rhs.isZero)
      return SBigInt.Zero
      
    if (this.isOne) 
      return rhs
    if (rhs.isOne) 
      return this
            
    ???
  }
  
  def /(rhs: SBigInt): SBigInt = {
    if(rhs.isZero)
      throw new ArithmeticException
    
    if (this.isZero)
      return SBigInt.Zero
      
    if (this.isOne) 
      ???
    if (rhs.isOne) 
      return this
      
    ???
  }
  def %(rhs: SBigInt): SBigInt = ???
  def /% (rhs: SBigInt): (SBigInt, SBigInt) = (this / rhs, this % rhs)

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
  def isOne: Boolean = signum == 1 && arr.length == 1 && arr(0) == 1
  
  def isValidLong: Boolean = this >= Long.MinValue && this <= Long.MaxValue

  /** Compares this SBigInt with the specified value for equality. */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: SBigInt     => this equalsSBigInt rhs
    case rhs: BigDecimal => rhs.toBigIntExact exists (this equals _)
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
    val Radix: Long = -2L * Int.MinValue.toLong
    val iArr: Array[Int] = new Array[Int](pArr.length)
    compat.Platform.arraycopy(pArr, 0, iArr, 0, pArr.length)
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

  @inline private def longToUnsignedInt(long: Long): UInt = {
    assert(long >= 0L)
    assert(long <= UnsignedIntMask)
    if (long <= Int.MaxValue.toLong) return long.toInt
    else return (long & UnsignedIntMask).toInt
  }
  
  /**
   * Iff the value is negative return 0xFFFFFFFF (all bits set),
   * else 0x00000000 (no bits set).
   */
  private def signBits: Int =
    if (signum < 0) -1
    else 0
    
  /**
   * TODO: Better name.
   */
  private def apply(i: Int): Int = {
    // Return 0 if i is negative. (Or throw an exception?)
    if(i < 0) return 0
    // Return the signBits, if the requested value is larger than the magnitude. 
    if(i >= arr.length) return signBits
    //Return the requested parts of the magnitude.
    ???
  }

  
  /**
   * Standard Serialization would work, but we have to make sure
   * that we sanitize the array to verify our invariant of no leading
   * “zeroes” in our magnitude.
   * 
   * Otherwise all methods depending on it will be broken.
   * 
   * TODO: It probably makes sense to write an independent sanitizing method,
   * which can be shared and call it from here...
   */
  @throws(classOf[java.io.IOException]) @throws(classOf[java.lang.ClassNotFoundException])
  private def readObject(in: java.io.ObjectInputStream): Unit = {
    @inline def setField(name: String, value: Any): Unit = {
      val field = this.getClass.getDeclaredField(name)
      field.setAccessible(true)
      field.set(this, value)
      field.setAccessible(false)
    }
    
    var sign = in.readByte
    if(sign > 1 || sign < -1) 
      throw new java.io.StreamCorruptedException 
    setField("signum", sign)
    var inArr = in.readObject.asInstanceOf[Array[UInt]]
    if(sign == 0 && inArr.length != 0)
      throw new java.io.StreamCorruptedException
    setField("arr", stripLeadingZeroes(inArr))
  }
  
  @throws(classOf[java.io.ObjectStreamException])
  private def readReplace(): Object = {
    if(signum == 0) return SBigInt.Zero
    else this
  }
  
  @throws(classOf[java.io.IOException])
  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeByte(signum)
    out.writeObject(arr)
    out.close()
  }
    
  ////////////////////////////////////////////////////////////////////////////////

  private def arrayPlusArray(lhs: Array[UInt], rhs: Array[UInt]): Array[UInt] = ???
  private def arrayMinusArray(lhs: Array[UInt], rhs: Array[UInt]): Array[UInt] = ???
  
  private def multLong(lhs: Array[UInt], rhs: Array[UInt]) = ???
  private def multKaratsuba(lhs: Array[UInt], rhs: Array[UInt]) = ???
  private def multToomCook(lhs: Array[UInt], rhs: Array[UInt]) = ???
  private def multSchönhageStrassen(lhs: Array[UInt], rhs: Array[UInt]) = ???

  // Don't trust sameContents.
  @inline private final def sameArrayContents(a: Array[UInt], b: Array[UInt]): Boolean = {
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