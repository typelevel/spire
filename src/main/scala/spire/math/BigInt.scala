package spire.math
import scala.math.{ ScalaNumber, ScalaNumericConversions }
import scala.math.{ BigInt => _ }

object BigInt {
  def apply(i: Int) = ???
  def apply(i: Long) = ???
  def apply(s: String) = ???
  def fromArray(arr: Array[Int]) = ???
  
  /** Implicit conversion from `Int` to `BigInt`. */
  implicit def int2bigInt(i: Int): BigInt = apply(i)

  /** Implicit conversion from `Long` to `BigInt`. */
  implicit def long2bigInt(l: Long): BigInt = apply(l)
}

final class BigInt private(final val sign: Int, final val arr: Array[Int]) extends ScalaNumber with ScalaNumericConversions with Ordered[BigInt] with Serializable {
  
  def isWhole: Boolean = true
  def underlying = this

  def longValue = ???
  def intValue = ???
  def floatValue = ???
  def doubleValue = ???
  
  def compare(rhs: BigInt): Int = ??? 
    
  def signum(rhs: BigInt) = ???
  def abs(rhs: BigInt) = ???
  def unary_!(rhs: BigInt) = ???
  def unary_~(rhs: BigInt) = ???
  
  def +(rhs: BigInt) = ???
  def -(rhs: BigInt) = ???
  def *(rhs: BigInt) = ???
  def /(rhs: BigInt) = ???
  def %(rhs: BigInt): BigInt = ???
  def /% (rhs: BigInt): (BigInt, BigInt) = (this / rhs, this % rhs)

  /** Leftshift of BigInt */
  def << (n: Int): BigInt = ???

  /** (Signed) rightshift of BigInt */
  def >> (n: Int): BigInt = ???

  /** Bitwise and of BigInts */
  def &  (rhs: BigInt): BigInt = ???

  /** Bitwise or of BigInts */
  def |  (rhs: BigInt): BigInt = ???

  /** Bitwise exclusive-or of BigInts
   */
  def ^  (rhs: BigInt): BigInt = ???

  /** Bitwise and-not of BigInts. Returns a BigInt whose value is (this & ~rhs). */
  def &~ (rhs: BigInt): BigInt = ???

  /** Returns the greatest common divisor of abs(this) and abs(rhs) */
  def gcd(rhs: BigInt): BigInt = ???

  /** Returns a BigInt whose value is (this mod m).
   *  This method differs from `%` in rhs it always returns a non-negative BigInt. */
  def mod(rhs: BigInt): BigInt = ???

  /** Returns the minimum of this and rhs */
  def min(rhs: BigInt): BigInt = ???

  /** Returns the maximum of this and rhs */
  def max(rhs: BigInt): BigInt = ???

  /** Returns a BigInt whose value is (`this` raised to the power of `exp`). */
  def pow (exp: Int): BigInt = ???
  
  def isValidLong: Boolean = this >= Long.MinValue && this <= Long.MaxValue

  /** Compares this BigInt with the specified value for equality. */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: BigInt     => this equalsBigInt rhs
    case rhs: BigDecimal => rhs.toBigIntExact exists (this equals _)
    case x                => isValidLong && unifiedPrimitiveEquals(x)
  }
  
  def equalsBigInt(rhs: BigInt): Boolean = compare(rhs) == 0
  
  override def hashCode: Int =
    if (isValidLong) unifiedPrimitiveHashcode
    else ???
    
  override def toString = ???
    
  ////////////////////////////////////////////////////////////////////////////////
  
  private def arrayPlusArray(lhs: Array[Int], rhs: Array[Int]): Array[Int] = ???
  private def arrayMinusArray(lhs: Array[Int], rhs: Array[Int]): Array[Int] = ???
  
  private def multLong(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multKaratsuba(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multToomCook(lhs: Array[Int], rhs: Array[Int]) = ???
  private def multSchönhageStrassen(lhs: Array[Int], rhs: Array[Int]) = ???
  
}