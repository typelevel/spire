package numerics.math.real

import numerics.math._


trait RealLike { self: Real =>
  def sign: Sign
  def signum: Int = sign.toInt
  
  def compare(that: Real): Int = (this - that).signum
  
  def abs: Real = if (this.sign == Negative) -this else this

  def *(that: Real): Real = Mul(this, that)
  def +(that: Real): Real = Add(this, that)
  def -(that: Real): Real = Sub(this, that)
  def /(that: Real): Real = Div(this, that)
  def unary_-(): Real = Neg(this)
  def sqrt: Real = this nroot 2
  def nroot(k: Int): Real = {
    if (this.sign == Negative && k % 2 == 0) {
      throw new ArithmeticException("Cannot find an even root of a negative Real.")
    }

    KRoot(this, k)
  }

  // TODO: Create Pow as a 1st class citizen.
  def pow(k: Int): Real = if (k < 0) {
    IntLit(1) / (this pow -k)
  } else if (k == 0) {
    IntLit(1)
  } else if (k == 1) {
    this
  } else {
    val x = this pow (k / 2)
    val x2 = x * x
    if (k % 2 == 0) x2 else x2 * this
  }

}


