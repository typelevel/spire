package spire.math

import scala.math.{floor, BigDecimal}

//import language.implicitConversions

// TODO: implement RationalNumber.
// TODO: implement toNumber and fromNumber in ConvertableTo/From.
// TODO: create implicits to support things like 3 + Number(4) -> Number(7)
// TODO: profile SafeLong
// TODO: pow() is hairy; should support more cases and generate better errors
// TODO: decide what should be public/private

/**
 * Convenient apply and implicits for Numbers
 */
object Number {
  def apply(n:Byte):Number = IntNumber(SafeLong(n))
  def apply(n:Short):Number = IntNumber(SafeLong(n))
  def apply(n:Int):Number = IntNumber(SafeLong(n))
  def apply(n:Long):Number = IntNumber(SafeLong(n))
  def apply(n:BigInt):Number = IntNumber(SafeLong(n))
  def apply(n:SafeLong):Number = IntNumber(n)
  def apply(n:Double):Number = {
    if (n == n.toLong) IntNumber(SafeLong(n.toLong)) else FloatNumber(n)
  }
  def apply(n:BigDecimal):Number = {
    n.toBigIntExact.map(x => IntNumber(SafeLong(x))).getOrElse(DecimalNumber(n))
  }

  def apply(s:String):Number = {
    def conv(n: => Number):Option[Number] = try { Some(n) } catch { case _:Exception => None }
    conv(Number(java.lang.Long.parseLong(s))) orElse
    conv(Number(BigInt(s))) orElse
    conv(Number(java.lang.Double.parseDouble(s))) orElse
    conv(Number(BigDecimal(s))) getOrElse
    sys.error("could not parse Number: %s" format s)
  }

  implicit def byteToNumber(n:Int) = Number(n)
  implicit def shortToNumber(n:Int) = Number(n)
  implicit def intToNumber(n:Int) = Number(n)
  implicit def longToNumber(n:Long) = Number(n)
  implicit def bigIntToNumber(n:BigInt) = Number(n)
  implicit def doubleToNumber(n:Double) = Number(n)
  implicit def bigDecimalToNumber(n:BigDecimal) = Number(n)
}

sealed trait Number extends scala.math.ScalaNumber
with scala.math.ScalaNumericConversions with Ordered[Number] {
  def abs:Number
  def signum:Int

  protected[math] def withinInt: Boolean
  protected[math] def withinLong: Boolean
  protected[math] def withinDouble: Boolean

  protected[math] def canBeInt:Boolean
  protected[math] def canBeLong:Boolean

  def unary_-():Number

  def toBigInt:BigInt
  def toBigDecimal:BigDecimal

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

  def pow(rhs:Number): Number
  def **(rhs:Number) = this.pow(rhs)

  def nroot(rhs:Number): Number = rhs match {
    case IntNumber(m) => {
      val x = m.fold(_.toInt, _.toInt)
      if (m != x) sys.error("invalid exponent: %s" format rhs)
      nroot(x)
    }
    case _ => sys.error("invalid exponent: %s" format rhs)
  }

  def sqrt: Number = nroot(2)

  protected[math] def nroot(rhs:Int): Number
}


/**
 * Number representing an integer value.
 *
 * This is the most basic of the Number classes, so operations mixing this and
 * another type will handled by that other type. In general operations on two
 * IntNumber classes will return an IntNumber, except in cases like division.
 */
protected[math] case class IntNumber(n:SafeLong) extends Number {
  def abs = IntNumber(n.abs)
  def signum = n.signum

  def withinInt = SafeLong(Int.MinValue) <= n && n <= SafeLong(Int.MaxValue)
  def withinLong = SafeLong(Long.MinValue) <= n && n <= SafeLong(Long.MaxValue)
  def withinDouble = BigDecimal(Double.MinValue) <= n.toBigDecimal && n.toBigDecimal <= BigDecimal(Double.MaxValue)

  def canBeInt = isWhole && withinInt
  def canBeLong = isWhole && withinLong

  def toBigInt: BigInt = n.toBigInt
  def toBigDecimal: BigDecimal = n.toBigDecimal

  private def fold[A](l:Long => A, b:BigInt => A) = n.fold(l, b)

  def underlying = fold(x => new java.lang.Long(x), b => b)

  def isWhole = true
  def doubleValue = fold(_.toDouble, _.toDouble)
  def floatValue = fold(_.toFloat, _.toFloat)
  def longValue = fold(x => x, _.toLong)
  def intValue = fold(x => x.toInt, _.toInt)

  def compare(rhs:Number) = rhs match {
    case IntNumber(m) => n.compare(m)
    case t:Number => -t.compare(this)
    case t => sys.error("invalid compare: %s vs %s" format (this, rhs))
  }

  override def equals(that: Any): Boolean = that match {
    case IntNumber(m) => n.compare(m) == 0
    case t:Number => t.equals(this)
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs:Number) = rhs match {
    case IntNumber(m) => IntNumber(n + m)
    case t => t + this
  }

  def *(rhs:Number) = rhs match {
    case IntNumber(m) => IntNumber(n * m)
    case t => t * this
  }

  def -(rhs:Number) = rhs match {
    case IntNumber(m) => IntNumber(n - m)
    case t => t rhs_- this
  }
  def rhs_-(lhs:Number) = lhs match {
    case IntNumber(m) => IntNumber(m - n)
    case t => t - this
  }

  def /(rhs:Number) = rhs match {
    case IntNumber(m) => n.fold(
      x => m.fold(y => FloatNumber(x.toDouble / y.toDouble),
                  y => DecimalNumber(BigDecimal(x) / BigDecimal(y))),
      x => Number(BigDecimal(x) / m.toBigDecimal)
    )
    case t => t rhs_/ this
  }
  def rhs_/(lhs:Number) = lhs match {
    case IntNumber(m) => n.fold(
      x => m.fold(y => FloatNumber(y.toDouble / x.toDouble),
                  y => DecimalNumber(BigDecimal(y) / BigDecimal(x))),
      x => Number(m.toBigDecimal / BigDecimal(x))
    )
    case t => t / this
  }
  
  def /~(rhs:Number) = rhs match {
    case IntNumber(m) => IntNumber(n / m)
    case t => t rhs_/~ this
  }
  def rhs_/~(lhs:Number) = lhs match {
    case IntNumber(m) => IntNumber(m / n)
    case t => t /~ this
  }
  
  def %(rhs:Number) = rhs match {
    case IntNumber(m) => IntNumber(n % m)
    case t => t rhs_% this
  }
  def rhs_%(lhs:Number) = lhs match {
    case IntNumber(m) => IntNumber(m % n)
    case t => t % this
  }

  def pow(rhs:Number) = rhs match {
    case _ if rhs.canBeInt => Number(n.pow(rhs.intValue))
    case FloatNumber(m) if (withinDouble) => Number(fun.pow(doubleValue, m))
    case _ => Number(fun.pow(this.toBigDecimal, rhs.toBigDecimal))
  }

  def nroot(rhs:Int):Number = n.fold(x => Number(Numeric[Long].nroot(x, rhs)),
                                     x => Number(Numeric[BigInt].nroot(x, rhs)))
}

protected[math] case class FloatNumber(n:Double) extends Number {
  def abs = FloatNumber(n.abs)
  def signum = n.signum

  def withinInt = Int.MinValue.toDouble <= n && n <= Int.MaxValue.toDouble
  def withinLong = Long.MinValue.toDouble <= n && n <= Long.MaxValue.toDouble
  def withinDouble = Double.MinValue <= n && n <= Double.MaxValue

  def canBeInt = isWhole && withinInt
  def canBeLong = isWhole && withinLong

  def underlying = new java.lang.Double(n)
  def isWhole = (n % 1) == 0.0
  def doubleValue = n
  def floatValue = n.toFloat
  def longValue = n.toLong
  def intValue = n.toInt

  def toBigInt: BigInt = BigDecimal(n).toBigInt
  def toBigDecimal: BigDecimal = BigDecimal(n)

  def compare(rhs:Number) = rhs match {
    case IntNumber(m) => BigDecimal(n) compare m.toBigDecimal
    case FloatNumber(m) => n compare m
    case t => -t.compare(this)
  }

  override def equals(that: Any): Boolean = that match {
    case IntNumber(m) => if (!this.isWhole) {
      false
    } else {
      m.fold(x => canBeLong && n.toLong == x,
             x => BigDecimal(n).toBigInt == x)
    }
    case FloatNumber(m) => n == m
    case t:Number => t.equals(this)
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs:Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n + x), x => Number(BigDecimal(x) + n))
    case FloatNumber(m) => Number(n + m)
    case t => t + this
  }

  def *(rhs:Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n * x), x => Number(BigDecimal(n) * BigDecimal(x)))
    case FloatNumber(m) => Number(n * m)
    case t => t * this
  }

  def -(rhs:Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n - x), x => Number(BigDecimal(n) + BigDecimal(x)))
    case FloatNumber(m) => Number(n - m)
    case t => t rhs_- this
  }
  def rhs_-(lhs:Number) = lhs match {
    case IntNumber(m) => m.fold(x => Number(x - n), x => Number(BigDecimal(x) - BigDecimal(n)))
    case FloatNumber(m) => Number(m - n)
    case t => t - this
  }

  def /(rhs:Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n / x), x => Number(BigDecimal(n) / BigDecimal(x)))
    case FloatNumber(m) => Number(n / m)
    case t => t rhs_/ this
  }
  def rhs_/(lhs:Number) = lhs match {
    case IntNumber(m) => m.fold(x => Number(x / n), x => Number(BigDecimal(x) / BigDecimal(n)))
    case FloatNumber(m) => Number(m / n)
    case t => t / this
  }

  def /~(rhs:Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(floor(n / x)),
                                x => Number(BigDecimal(n) quot BigDecimal(x)))
    case FloatNumber(m) => Number(floor(n / m))
    case t => t rhs_/~ this
  }
  def rhs_/~(lhs:Number) = lhs match {
    case IntNumber(m) => m.fold(x => Number(floor(x / n)),
                                x => Number(BigDecimal(x) quot n))
    case FloatNumber(m) => Number(floor(m / n))
    case t => t /~ this
  }
  
  def %(rhs:Number) = rhs match {
    case IntNumber(m) => m.fold(x => Number(n % x),
                                 x => Number(BigDecimal(n) % BigDecimal(x)))
    case FloatNumber(m) => Number(n % m)
    case t => t.rhs_%(this)
  }
  def rhs_%(lhs:Number) = lhs match {
    case IntNumber(m) => m.fold(x => Number(x % n),
                                 x => Number(BigDecimal(x) % n))
    case FloatNumber(m) => Number(m % n)
    case t => t % this
  }

  def pow(rhs:Number) = rhs match {
    case FloatNumber(m) => Number(fun.pow(n, m))
    case _ if rhs.withinDouble => Number(fun.pow(n, rhs.doubleValue));
    case _ => Number(fun.pow(BigDecimal(n), rhs.toBigDecimal))
  }

  def nroot(rhs:Int) = Number(Numeric[Double].nroot(n, rhs))
}


protected[math] case class DecimalNumber(n:BigDecimal) extends Number {
  def abs = DecimalNumber(n.abs)
  def signum = n.signum

  def withinInt = BigDecimal(Int.MinValue) <= n && n <= BigDecimal(Int.MaxValue)
  def withinLong = BigDecimal(Long.MinValue) <= n && n <= BigDecimal(Long.MaxValue)
  def withinDouble = BigDecimal(Double.MinValue) <= n && n <= BigDecimal(Double.MaxValue)

  def canBeInt = isWhole && withinInt
  def canBeLong = isWhole && withinLong

  def underlying = n
  def isWhole = n % 1 == 0
  def doubleValue = n.toDouble
  def floatValue = n.toFloat
  def longValue = n.toLong
  def intValue = n.toInt

  def toBigInt: BigInt = n.toBigInt
  def toBigDecimal: BigDecimal = n

  def compare(rhs:Number) = rhs match {
    case IntNumber(m) => n compare m.toBigDecimal
    case FloatNumber(m) => n compare BigDecimal(m)
    case DecimalNumber(m) => n compare m
    case t => -t.compare(this)
  }

  override def equals(that: Any): Boolean = that match {
    case IntNumber(m) => m.fold(n == _, n == _)
    case FloatNumber(m) => n == m
    case DecimalNumber(m) => n == m
    case t:Number => t == this
    case t => unifiedPrimitiveEquals(t)
  }

  def unary_- = Number(-n)

  def +(rhs:Number) = rhs match {
    case IntNumber(m) => Number(n + m.toBigDecimal)
    case FloatNumber(m) => Number(n + m)
    case DecimalNumber(m) => Number(n + m)
    case t => t + this
  }

  def *(rhs:Number) = rhs match {
    case IntNumber(m) => Number(n * m.toBigDecimal)
    case FloatNumber(m) => Number(n * m)
    case DecimalNumber(m) => Number(n * m)
    case t => t * this
  }

  def -(rhs:Number) = rhs match {
    case IntNumber(m) => Number(n - m.toBigDecimal)
    case FloatNumber(m) => Number(n - m)
    case DecimalNumber(m) => Number(n - m)
    case t => t.rhs_-(this)
  }
  def rhs_-(lhs:Number) = lhs match {
    case IntNumber(m) => Number(m.toBigDecimal - n)
    case FloatNumber(m) => Number(BigDecimal(m) - n)
    case DecimalNumber(m) => Number(m - n)
    case t => t - this
  }

  def /(rhs:Number) = rhs match {
    case IntNumber(m) => Number(n / m.toBigDecimal)
    case FloatNumber(m) => Number(n / BigDecimal(m))
    case DecimalNumber(m) => Number(n / m)
    case t => t rhs_/ this
  }
  def rhs_/(lhs:Number) = lhs match {
    case IntNumber(m) => Number(m.toBigDecimal / n)
    case FloatNumber(m) => Number(BigDecimal(m) / n)
    case DecimalNumber(m) => Number(m / n)
    case t => t / this
  }
  
  def /~(rhs:Number) = rhs match {
    case IntNumber(m) => Number(n quot m.toBigDecimal)
    case DecimalNumber(m) => Number(n quot m)
    case t => t rhs_/~ this
  }
  def rhs_/~(lhs:Number) = lhs match {
    case IntNumber(m) => Number(m.toBigDecimal quot n)
    case DecimalNumber(m) => DecimalNumber(m quot n)
    case t => t /~ this
  }
  
  def %(rhs:Number) = rhs match {
    case IntNumber(m) => Number(n % m.toBigDecimal)
    case FloatNumber(m) => Number(n % m)
    case DecimalNumber(m) => Number(n % m)
    case t => t.rhs_%(this)
  }
  def rhs_%(lhs:Number) = lhs match {
    case IntNumber(m) => Number(m.toBigDecimal % n)
    case FloatNumber(m) => Number(BigDecimal(m) % n)
    case DecimalNumber(m) => Number(m % n)
    case t => t % this
  }

  def pow(rhs:Number) = if (rhs.canBeInt) {
    Number(n.pow(rhs.intValue))
  } else {
    Number(fun.pow(n, rhs.toBigDecimal))
  }

  def nroot(rhs:Int) = Number(Numeric[BigDecimal].nroot(n, rhs))
}
