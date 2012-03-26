package spire.math

import scala.math.{ ScalaNumber, ScalaNumericConversions }
import scala.math.{ BigInt => _ }
import SBigInt._

//final class UInt(val uint: Int) extends AnyVal 

object SBigInt {
  // 2.10 got this, but 2.9 not.
  def ??? = throw new UnsupportedOperationException
  
  final val Zero: SBigInt = new SBigInt(0, Array[Int]())
  final val One: SBigInt = SBigInt(1)
  
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
  def apply(s: String) = ???
  def fromArray(arr: Array[Int]) = ???
  
  /** Implicit conversion from `Int` to `SBigInt`. */
  implicit def int2bigInt(i: Int): SBigInt = apply(i)

  /** Implicit conversion from `Long` to `SBigInt`. */
  implicit def long2bigInt(l: Long): SBigInt = apply(l)
}

final class SBigInt private(final val signum: Int, final private[math] val arr: Array[Int]) extends ScalaNumber with ScalaNumericConversions with Ordered[SBigInt] with Serializable {
  
  def isWhole: Boolean = true
  def underlying = SBigInt.this

  def longValue = ???
  def intValue = ???
  def floatValue = ???
  def doubleValue = ???
  
  def compare(rhs: SBigInt): Int = ??? 
    
  def abs = if (signum < 0) -SBigInt.this else SBigInt.this
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
  
  def isValidLong: Boolean = SBigInt.this >= Long.MinValue && SBigInt.this <= Long.MaxValue

  /** Compares this SBigInt with the specified value for equality. */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: SBigInt     => SBigInt.this equalsSBigInt rhs
    case rhs: BigDecimal => rhs.toBigIntExact exists (SBigInt.this equals _)
    case x                => isValidLong && unifiedPrimitiveEquals(x)
  }
  
  //private def equalsBigInt(rhs: BigInt): Boolean = compare(rhs) == 0
  private def equalsSBigInt(rhs: SBigInt): Boolean = (this eq rhs) || compare(rhs) == 0
  
  override def hashCode: Int =
    if (isValidLong) unifiedPrimitiveHashcode
    else ???
    
  override def toString = "toString not implemented yet!"
    
  ////////////////////////////////////////////////////////////////////////////////

  private def arrayPlusArray(lhs: Array[Int], rhs: Array[Int]): Array[Int] = ???
  private def arrayMinusArray(lhs: Array[Int], rhs: Array[Int]): Array[Int] = ???
  
  private def multLong(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multKaratsuba(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multToomCook(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multSchönhageStrassen(lhs: Array[Int], rhs: Array[Int]) = ???
  
}