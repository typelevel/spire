package numerics.math

import scala.math.{ScalaNumber, ScalaNumericConversions, floor}
import Implicits._
import numerics.math.fun._

// TODO: implement at least BigIntNumber, BigDecimalNumber, RationalNumber.
// TODO: implement toNumber and fromNumber in ConvertableTo/From.
// TODO: create implicits to support things like 3 + Number(4) -> Number(7)
// TODO: profile SafeLong to see just how fast it is, and maybe optimize a bit.
// TODO: try using SafeLong for overflow checking/BigInt support
// TODO: if using SafeLong, rename LongNumber (and don't make BigIntNumber)

/**
 * Convenient apply and implicits for Numbers
 */
object Number {
  def apply(n:Byte):Number = LongNumber(n)
  def apply(n:Short):Number = LongNumber(n)
  def apply(n:Int):Number = LongNumber(n)
  def apply(n:Long):Number = LongNumber(n)

  def apply(n:Double):Number = DoubleNumber(n)

  implicit def byteToNumber(n:Int) = LongNumber(n)
  implicit def shortToNumber(n:Int) = LongNumber(n)
  implicit def intToNumber(n:Int) = LongNumber(n)
  implicit def longToNumber(n:Long) = LongNumber(n)

  implicit def doubleToNumber(n:Double) = DoubleNumber(n)
}

sealed trait Number extends ScalaNumber with ScalaNumericConversions with Ordered[Number] {
  def abs:Number
  def signum:Int

  def unary_-():Number

  def +(rhs:Number):Number

  def *(rhs:Number):Number

  def -(rhs:Number):Number
  protected[math] def rhs_-(lhs:Number):Number

  def /(rhs:Number):Number
  protected[math] def rhs_/(lhs:Number):Number

  def /~(rhs:Number):Number
  protected[math] def rhs_/~(lhs:Number):Number

  def %(rhs:Number):Number
  protected[math] def rhs_%(lhs:Number):Number

  def /%(rhs:Number) = (this /~ rhs, this % rhs)
  protected[math] def rhs_/%(lhs:Number) = (this rhs_/~ lhs, this rhs_% lhs)

  def pow(rhs:Number):Number
  def **(rhs:Number) = this pow rhs
  def ~^(rhs:Number) = this pow rhs

  def **:(lhs:Number) = this rhs_pow lhs
  def ~^:(lhs:Number) = this rhs_pow lhs
  protected[math] def rhs_pow(lhs:Number):Number
}


/**
 * Number with an underlying Long representation.
 */
protected[math] case class LongNumber(n:Long) extends Number {
  def abs = LongNumber(n.abs)
  def signum = n.signum

  def underlying = new java.lang.Long(n)
  def isWhole = true
  def doubleValue = n.toDouble
  def floatValue = n.toFloat
  def longValue = n
  def intValue = n.toInt

  def compare(rhs:Number) = rhs match {
    case LongNumber(m) => n.compare(m)
    case t => -t.compare(this)
  }

  override def equals(that: Any): Boolean = that match {
    case LongNumber(m) => n == m
    case t:Number => t.equals(this)
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n + m)
    case t => t + this
  }

  def *(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n * m)
    case t => t * this
  }

  def -(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n - m)
    case t => t.rhs_-(this)
  }
  def rhs_-(lhs:Number) = lhs match {
    case LongNumber(m) => Number(m - n)
    case t => t - this
  }

  def /(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n - m)
    case t => t rhs_/ this
  }
  def rhs_/(lhs:Number) = lhs match {
    case LongNumber(m) => Number(m / n)
    case t => t / this
  }

  def /~(rhs:Number) = rhs match {
    case LongNumber(m) => Number(floor(n / m))
    case t => t.rhs_/~(this)
  }
  def rhs_/~(lhs:Number) = lhs match {
    case LongNumber(m) => Number(floor(m / n))
    case t => t /~ this
  }

  def %(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n % m)
    case t => t.rhs_%(this)
  }
  def rhs_%(lhs:Number) = lhs match {
    case LongNumber(m) => Number(m % n)
    case t => t % this
  }

  def pow(rhs:Number) = rhs match {
    case LongNumber(m) => Number(math.pow(n, m))
    case t => t.rhs_pow(this)
  }
  def rhs_pow(lhs:Number) = lhs match {
    case LongNumber(m) => Number(math.pow(m, n))
    case t => t pow this
  }
}

object LongNumber {
  
}

protected[math] case class DoubleNumber(n:Double) extends Number {
  def abs = DoubleNumber(n.abs)
  def signum = n.signum

  def underlying = new java.lang.Double(n)
  def isWhole = n.isWhole
  def doubleValue = n
  def floatValue = n.toFloat
  def longValue = n.toLong
  def intValue = n.toInt

  def compare(rhs:Number) = rhs match {
    case LongNumber(m) => {
      val d = m.toDouble
      val cmp = n compare d
      if (cmp == 0) {
        d.toLong compare m
      } else {
        cmp
      }
    }
    case DoubleNumber(m) => n compare m
    case t => -t.compare(this)
  }

  override def equals(that: Any): Boolean = that match {
    case LongNumber(m) => {
      val d = m.toDouble
      if (d.toLong == m) n == d else false
    }
    case DoubleNumber(m) => n == m
    case t:Number => t.equals(this)
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n + m.toDouble)
    case DoubleNumber(m) => Number(n + m)
    case t => t + this
  }

  def *(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n * m.toDouble)
    case DoubleNumber(m) => Number(n * m)
    case t => t * this
  }

  def -(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n - m.toDouble)
    case DoubleNumber(m) => Number(n - m)
    case t => t.rhs_-(this)
  }
  def rhs_-(lhs:Number) = lhs match {
    case LongNumber(m) => Number(m.toDouble - n)
    case DoubleNumber(m) => Number(m - n)
    case t => t - this
  }

  def /(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n - m.toDouble)
    case DoubleNumber(m) => Number(n - m)
    case t => t rhs_/ this
  }
  def rhs_/(lhs:Number) = lhs match {
    case LongNumber(m) => Number(m.toDouble / n)
    case DoubleNumber(m) => Number(m / n)
    case t => t / this
  }

  def /~(rhs:Number) = rhs match {
    case LongNumber(m) => Number(floor(n / m.toDouble))
    case DoubleNumber(m) => Number(floor(n / m))
    case t => t.rhs_/~(this)
  }
  def rhs_/~(lhs:Number) = lhs match {
    case LongNumber(m) => Number(floor(m.toDouble / n))
    case DoubleNumber(m) => Number(floor(m / n))
    case t => t /~ this
  }

  def %(rhs:Number) = rhs match {
    case LongNumber(m) => Number(n % m.toDouble)
    case DoubleNumber(m) => Number(n % m)
    case t => t.rhs_%(this)
  }
  def rhs_%(lhs:Number) = lhs match {
    case LongNumber(m) => Number(m.toDouble % n)
    case DoubleNumber(m) => Number(m % n)
    case t => t % this
  }

  def pow(rhs:Number) = rhs match {
    case LongNumber(m) => Number(math.pow(n, m.toDouble))
    case DoubleNumber(m) => Number(math.pow(n, m))
    case t => t.rhs_pow(this)
  }
  def rhs_pow(lhs:Number) = lhs match {
    case LongNumber(m) => Number(math.pow(m.toDouble, n))
    case DoubleNumber(m) => Number(math.pow(m, n))
    case t => t pow this
  }

}
